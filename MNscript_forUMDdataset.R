#MGLP Project
#Jacob Angus 30June2022

#  Script for Minnesota that can be run as a job
#  It can also be run in individual sections
#  It has five major sections: Data Cleaning and Joining, Filtering,
#  Interpolation, Calculating VHOD, Calculating TDO3

library("tidyverse")
library("lubridate")
library("zoo")
library("rLakeAnalyzer")
library("broom")
library("readxl")
library("plotly")
library("skimr")

###### Data Cleaning, Preparation, and Joining ==================================
#  The goal of this section of the script is to filter lakes for
#  geometric ratio, combine data sets with metadata, and join Minnesota
#  data sets together. It is split into two sections: WQP and MNPCA
#  Run time is about 5 minutes

setwd("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/Jacob")
#### Load files
# WQP 
allMN.do = read_csv("MNDataRetrievalCombined.csv") #All MN Observations of Temp and DO from 1940 to 2020 from WQP
MN.allsites = read_csv("MNDataRetrievalMetadata.csv") #Site data for all the observations above
LakeDepthArea = read_csv("lake_depth.csv") #Dataset containing the lake depth and area for all lakes in the US
Link = read_csv("lake_link.csv") #Dataset that can link the Lagos dataset to the STORET dataset

#  Convert Monitoring Location Identifier into DOW 
#  DOW is the official way to identify lakes in Minnesota; whereas,
#  Monitoring Location Identifiers are specific sites on a lake.
all.MN.do.DOW = allMN.do %>%
  filter(OrganizationIdentifier == "MNPCA")%>%
  separate(MonitoringLocationIdentifier, into = c("MNPCA", "County", "LakeID", "ID", "Basin"), sep = "-", remove = F)%>%
  unite(col = DOW, c(County, LakeID, ID), sep = "")
all.MN.DOW = allMN.do %>%
  filter(OrganizationIdentifier != "MNPCA")%>%
  mutate(DOW = MonitoringLocationIdentifier)%>%
  full_join(all.MN.do.DOW)



 #Summary of WQP Data  #1933202 obs
# all.MN.DOW %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #7,626 Sample Sites
# all.MN.DOW %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #4,755 Lakes
# all.MN.DOW %>%
#   mutate(Year = year(ymd(ActivityStartDate)))%>%
#   group_by(Year)%>%
#   summarise(n = n()) #78 Years
# all.MN.DOW %>%
#   mutate(Year = year(ymd(ActivityStartDate)))%>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #24,672 Lake-Years
# all.MN.DOW %>%
#   mutate(Year = year(ymd(ActivityStartDate)),
#          DOY = yday(ymd(ActivityStartDate)))%>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #157,124 Profiles

#Clean
rm(allMN.do, all.MN.do.DOW)
gc()



#  MNPCA
MN_sum = read_csv("aug_profiles.csv") #Summary of profiles from MNPCA
MN_df = read_csv("1945_2020_All_MNDNR_MPCA_Temp_DO_Profiles.csv") #Lake Profile data
MN.station = read_csv("MNstation.csv") %>% #Metadata for all MNPCA sites
  filter(OrganizationIdentifier == "MNPCA")%>% # Here we also need to make DOW from Monitoring Location Identifier
  separate(MonitoringLocationIdentifier, into = c("MNPCA", "County", "Township", "LakeID", "Basin"), sep = "-", remove = F)%>%
  unite(col = DOW, c("County", "Township", "LakeID"), sep = "")%>%
  select(c(DOW, LatitudeMeasure, LongitudeMeasure))%>%
  distinct(DOW, .keep_all = T)
#  Here we are creating Monitoring Location Identifiers for the MNPCA and MNDNR
#  dataset. They have "PROFID" and DOW which can be combined to make a 
#  Monitoring Location Identifier. This will later help when removing duplicate
#  profiles and joining the data together.
MN_df.MLI = MN_df %>% 
  separate(DOW_DATE_AGENCY_PROFID, into = c("DOW", "DATE", "AGENCY", "PROFID"), sep = "_", remove = F)%>%
  mutate(Sample.Site = floor(as.numeric(PROFID))) %>%
  mutate(County = substr(DOW, start = 1, stop = 2),
         Township = substr(DOW, start = 3, stop = 6),
         LakeID = substr(DOW, start = 7, stop = 8),
         MNAgency = paste("MN", AGENCY, sep = ""),
         MonitoringLocationIdentifier = paste(MNAgency, County, Township, LakeID, Sample.Site, sep = "-"))


### Remove Duplicate Profiles across data sets
#  The MNPCA reports most of their data to the WQP; therefore, we likely have 
#  duplicated data. If we screen for profiles (Monitoring Location ID + Date),
#  then we can see which ones are overlapping and remove them from one data set
WQP.Profiles = all.MN.DOW %>% # This identifies all unique profiles in the WQP data
  mutate(Year = year(ymd(ActivityStartDate)),
         DOY = yday(ymd(ActivityStartDate)),
         Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/"))%>%
  distinct(Profile)
MN.Duplicate.Profiles = MN_df.MLI%>% # This identifies all unique profiles in the MNPCA data
  mutate(Year = year(mdy(DATE)),
         DOY = yday(mdy(DATE)),
         Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/"))%>%
  distinct(Profile)%>%
  inner_join(WQP.Profiles) # Joins the two together with inner_join, so we only select for the ones that both data sets have
MN_DF = MN_df.MLI %>% #Finally remove the duplicate ones from the MNPCA data set 
  mutate(Year = year(mdy(DATE)),
         DOY = yday(mdy(DATE)),
         Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/"))%>%
  anti_join(MN.Duplicate.Profiles, by = "Profile") #Use anti_join to remove anything that overlaps

# # Summarize the MNPCA Data #434023 obs
# MN_DF %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #8,470 Sample Sites
# MN_DF %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #4,358 Lakes
# MN_DF %>%
#   mutate(Year = year(mdy(DATE)))%>%
#   group_by(Year)%>%
#   summarise(n = n()) #80 Years
# MN_DF %>%
#   mutate(Year = year(mdy(DATE)))%>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #22,981 Lake-Years
# MN_DF %>%
#   mutate(Year = year(mdy(DATE)),
#          DOY = yday(mdy(DATE)))%>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #40,120 Profiles

# # #Check for overlap
# WQP.MLI = all.MN.DOW %>%
#   distinct(MonitoringLocationIdentifier)
# MN.MLI = MN_DF %>%
#   distinct(MonitoringLocationIdentifier)%>%
#   inner_join(WQP.MLI) #301 Overlap of MonitoringLocationIdentifier
# WQP.DOW = all.MN.DOW %>%
#   distinct(DOW)
# MN.DOW = MN_DF %>%
#   distinct(DOW)%>%
#   inner_join(WQP.DOW) #2269 Overlap of Lakes
# WQP.Year = all.MN.DOW %>%
#   mutate(Year = year(ymd(ActivityStartDate)))%>%
#   distinct(Year)
# MN.Year = MN_DF %>%
#   mutate(Year = year(mdy(DATE)))%>%
#   distinct(Year)%>%
#   inner_join(WQP.Year) #74 Overlap of Years
# WQP.LkYear = all.MN.DOW %>%
#   mutate(Year = year(ymd(ActivityStartDate)),
#          Lk_Yr = paste(DOW, Year, sep = "/"))%>%
#   distinct(Lk_Yr)
# MN.LkYear = MN_DF %>%
#   mutate(Year = year(mdy(DATE)),
#          Lk_Yr = paste(DOW, Year, sep = "/"))%>%
#   distinct(Lk_Yr)%>%
#   inner_join(WQP.LkYear)#4,098 Overlap of Lake Year Combos
WQP.Profile.Check = all.MN.DOW %>%
  mutate(Year = year(ymd(ActivityStartDate)),
         DOY = yday(ymd(ActivityStartDate)),
         Profile = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
  distinct(Profile)
MN.Profile.Check = MN_DF %>%
  mutate(Year = year(mdy(DATE)),
         DOY = yday(mdy(DATE)),
         Profile = paste(MonitoringLocationIdentifier,Year, DOY, sep = "/"))%>%
  distinct(Profile)%>%
  inner_join(WQP.Profile.Check)#0 Overlap of Profiles

#Clean
rm(MN_df, WQP.Profiles, MN.Duplicate.Profiles, MN_df.MLI,WQP.Profile.Check,MN.Profile.Check,
   MN.LkYear, MN.Year, MN.DOW, MN.MLI, WQP.MLI, WQP.DOW, WQP.Year, WQP.LkYear)
gc()

#### Filter Metadata
#WQP
MN.LakeDepthArea = LakeDepthArea %>% 
  filter(lake_states == "MN")%>% #We are only looking at MN lakes right now
  mutate(lake_area_m2 = lake_waterarea_ha * 10000)%>% #Fang and Stefan 2009 uses the As^0.25:Z with As in square meters
  mutate(GR = (lake_area_m2^0.25)/lake_maxdepth_m)%>% #(Gorham and Boyce, 1989) is one of the first paper to use Geometric Ratio
  filter(GR < 4) #Lakes with a GR greater than 4 do not stratify

#Connect everything together
MN.WQP = Link %>%
  select(c("lagoslakeid","wqp_monitoringlocationidentifier")) %>%#get rid of extra columns in the data to keep it simpler
  inner_join(MN.LakeDepthArea, by = "lagoslakeid") %>% #lagoslakeid also contains all the basins for lakes
  mutate(MonitoringLocationIdentifier = wqp_monitoringlocationidentifier) %>% ###Making the metadata files share columns
  inner_join(MN.allsites, by = "MonitoringLocationIdentifier") %>% #Joining by wqp monitoring location identifier
  inner_join(all.MN.DOW, by = "MonitoringLocationIdentifier")

df = MN.WQP %>%
  filter(DOW == "03035900" |
           DOW == "03038300" |
           DOW == "10004800" |
           DOW == "11047200" |
           DOW == "18021100" |
           DOW == "19600600" |
           DOW == "27009800" |
           DOW == "29013000" |
           DOW == "62005400" |
           DOW == "77015002" |
           DOW == "80003800" |
           DOW == "85001102")%>%
  select(DOW,lake_waterarea_ha, lake_maxdepth_m, GR )%>%
  distinct(DOW, .keep_all = T)
#Prepare for joining of the Minnesota Data
MN.feet.WQP = MN.WQP %>% #Convert feet to meters
  filter(ActivityDepthHeightMeasure.MeasureUnitCode == "feet"|
           ActivityDepthHeightMeasure.MeasureUnitCode == "ft")%>%
  mutate(Depth = round(ActivityDepthHeightMeasure.MeasureValue * 0.3038))
MN.meters.WQP = MN.WQP %>% #Join the meters together and remove NA values
  filter(ActivityDepthHeightMeasure.MeasureUnitCode == "m" |
           ActivityDepthHeightMeasure.MeasureUnitCode == "meters") %>%
  mutate(Depth = round(ActivityDepthHeightMeasure.MeasureValue))%>%
  full_join(MN.feet.WQP)%>%
  select(!ActivityDepthHeightMeasure.MeasureUnitCode)
#Separate Temperature and DO
MN.fah.WQP = MN.meters.WQP %>% #Convert fahrenheit to celsius
  filter(CharacteristicName == "Temperature, water")%>%
  filter(ResultMeasure.MeasureUnitCode == "deg F")%>%
  mutate(Temperature = (ResultMeasureValue - 32) * 5/9)

MN.cel.WQP = MN.meters.WQP %>%  #Join the celsius together and separate Temp from DO
  filter(CharacteristicName == "Temperature, water")%>%
  filter(ResultMeasure.MeasureUnitCode == "deg C")%>%
  mutate(Temperature = ResultMeasureValue)%>%
  full_join(MN.fah.WQP) %>%
  mutate(Latitude = lake_lat_decdeg                                    ,
         Longitude = lake_lon_decdeg                                   ,
         Max_Depth = lake_maxdepth_m                                   ,
         MonitoringLocationIdentifier                                  ,
         Date = ActivityStartDate                                      ,
         DOY = yday(Date),
         Year = year(Date))%>%
  select(c(MonitoringLocationIdentifier, Year, Date, DOY, DOW,
           Depth, Temperature, Latitude, Longitude, Max_Depth, OrganizationIdentifier.y))

MN.WQP.join = MN.meters.WQP %>% #Separate DO from Temp
  filter(ResultMeasure.MeasureUnitCode == "mg/l")%>%
  mutate(DO = ResultMeasureValue,
         Latitude = lake_lat_decdeg                                    ,
         Longitude = lake_lon_decdeg                                   ,
         Max_Depth = lake_maxdepth_m                                   ,
         MonitoringLocationIdentifier                                  ,
         Date = ActivityStartDate                                      ,
         DOY = yday(Date),
         Year = year(Date)) %>%
  select(c(MonitoringLocationIdentifier, Year, Date, DOY, DOW,
           Depth, DO, Latitude, Longitude, Max_Depth, OrganizationIdentifier.y)) %>%
  inner_join(MN.cel.WQP) %>% #Join back together to make sure we have a temp and DO for every temp
  group_by(MonitoringLocationIdentifier, Year, Date, DOY, DOW,
           Depth, Latitude, Longitude, Max_Depth, OrganizationIdentifier.y)%>%
  summarise(DO = mean(DO), #This will remove duplicates in the observations
            Temperature = mean(Temperature))%>%
  ungroup()

# #Summary of Data #588732 obs
# MN.WQP.join %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,438 MLI
# MN.WQP.join %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,220 Lakes
# MN.WQP.join %>%
#   group_by(Year)%>%
#   summarise(n = n()) #71 Years
# MN.WQP.join %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #7,300 Lake-Years
# MN.WQP.join %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #53,616 Profiles


#Clean up
rm(MN.allsites, MN.cel.WQP, MN.fah.WQP, MN.feet.WQP, MN.LakeDepthArea,
   MN.meters.WQP, MN.WQP, Link, LakeDepthArea, all.MN.DOW)
gc()



#  MNPCA
MN.sum.long = MN_sum %>%
  mutate(DOW = as.factor(DOW),###converts ID to factor, not number
         Z_m = Max_Depth_Ft*0.3048,
         area_km = Lake_Area_Acres*0.00404686,
         GR = (area_km^0.25)/Z_m)%>%###look at geometry ratio of lakes to filter out unstratified systems
  ###stefan et al 1996 GR=(A^0.25)/Zmax #(Gorham and Boyce, 1989) is one of the first paper to use Geometric Ratio
  filter(GR < 4) %>% ###filter out lakes with GR>4 
  pivot_longer("1945":"2019",names_to = "Yr", #pivot longer
               values_to = "n_profile")%>%
  mutate(Year = as.numeric(Yr),
         DOW_yr = as.character(paste(DOW,"_",Year)))
#  Make a data frame of max depths for metadata
MN.sum.depths = MN.sum.long%>%
  distinct(DOW, .keep_all = T)%>%
  mutate(DOW2 = DOW,
         Max_Depth = Z_m)%>%
  select(c("DOW2", "Max_Depth"))
###  pull lakes from profile dataset
MN_df2 = MN_DF %>%
  mutate(DOW2 = as.factor(substring(DOW_DATE_AGENCY_PROFID,1,6)),###make DOW with leading 0s
         Year = year(mdy(DATE)),
         DOW_yr = as.character(paste(DOW2,"_",Year))) %>%
  filter(DOW_yr %in% MN.sum.long$DOW_yr) %>% #Removes DOW that had a GR greater than 4
  inner_join(MN.station, by = "DOW") %>% #Adding a metadata file with Lat and Lon
  inner_join(MN.sum.depths, by = "DOW2")%>% #Adding max depth metadata
  mutate(Date = mdy(DATE),
         Depth = round(DEPTH_M),
         Temperature = TEMP_C,
         DO = DO_PPM,
         Latitude = LatitudeMeasure,
         Longitude = LongitudeMeasure,
         DOY = yday(Date)) %>%
  select(MonitoringLocationIdentifier, Year, Date, DOY,
         Depth, DO, Latitude, Longitude, Max_Depth, Temperature, DOW)

# #  Summary of Data
# 
# MN_df2 %>% #359734 obs
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #7,256 Sites
# MN_df2 %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,203 Lakes
# MN_df2 %>%
#   group_by(Year)%>%
#   summarise(n = n()) #70 Years
# MN_df2 %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #20,780 Lake-Years
# MN_df2 %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #77,536 Profiles


#Clean
rm(MN_DF, MN_sum, MN.station, MN.sum.depths, MN.sum.long)
gc()




####Join the Complete Observations Together

MN.data.forjoin = MN.WQP.join %>% 
  full_join(MN_df2)%>%
  group_by(MonitoringLocationIdentifier, Date, Depth, DOW)%>% #Checking for any multiple observations in the data and removing them
  summarise(DO = median(DO),
            Temperature = median(Temperature),
            Latitude = mean(Latitude),
            Longitude = mean(Longitude),
            Max_Depth = round(median(Max_Depth)))%>%
  mutate(Year = year(Date),
         DOY = yday(Date))%>%
  ungroup()

#  Heidi has some additional data for Green Lake in Kandiyohi County
Green.Lake.fromH = read_csv("DOTEMP_Green.csv")%>%
  mutate(Agency = ifelse(Source == "DNR", "MNDNR", "MNPCA"),
         MonitoringLocationIdentifier = paste(Agency, "34-0079-00", Station, sep = "-"),
         DOW = "34007900",
         Latitude = 45.2521,
         Longitude = -94.9044,
         Date = mdy(Date),
         DOY = yday(Date),
         Max_Depth = 34,
         Depth = floor(`Depth (M)`),
         Temperature = `Temp (C)`,
         DO = `DO (ppm)`)%>%
  select(MonitoringLocationIdentifier ,
         Date                         ,
         Depth                        ,
         DOW                          ,
         DO                           ,
         Temperature                  ,
         Latitude                     ,
         Longitude                    ,
         Max_Depth                    ,
         Year                         ,
         DOY                          )


GreenLake = MN.data.forjoin %>%
  filter(DOW == "34007900")
#Remove duplicates
GreenLake.fromH.Profiles = Green.Lake.fromH %>%
  mutate(Profiles = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
  distinct(Profiles)

GreenLake.DuplicateProfiles = GreenLake %>%
  ungroup()%>%
  mutate(Profiles = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
  distinct(Profiles)%>%
  inner_join(GreenLake.fromH.Profiles)

GreenLake.forjoin = Green.Lake.fromH %>%
  mutate(Profiles = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
  anti_join(GreenLake.DuplicateProfiles)%>%
  select(!Profiles)%>%
  group_by(MonitoringLocationIdentifier,Date,Depth,DOW,Latitude,
           Longitude,Max_Depth,Year,DOY)%>%
  summarise(DO = median(DO),
            Temperature = median(Temperature))%>%
  ungroup()
#  Join with the rest of the data
MN.ALL.Data = MN.data.forjoin %>%
  bind_rows(GreenLake.forjoin)


# #Summary #848842 obs
# MN.ALL.Data %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #7,480 Sample Locations
# MN.ALL.Data %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,444 Lakes
# MN.ALL.Data %>%
#   group_by(Year)%>%
#   summarise(n = n()) #76 Years
# MN.ALL.Data %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #21,496 Lake-Years
# MN.ALL.Data %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #84,915 Profiles


#write_csv(MN.ALL.Data, "MN.ALL.DATA.csv") 


#Cleaning
rm(MN.WQP.join, MN_df2, Green.Lake.fromH, GreenLake, GreenLake.DuplicateProfiles,
   GreenLake.forjoin, GreenLake.fromH.Profiles, MN.data.forjoin)
gc()


###### Filtering the Data in Preparation for Interpolation =================
#  The goal of this section is to filter the data by several factors:
#  Lakes must have a profile at the start of stratification (121 <= DOY <= 166)
#  Lakes must have a profile at the end of stratification (196 <= DOY <= 258)
#  Profiles must be taken from glacial lakes
#  Profiles will be interpolated at 1m depths 
#  Data points where DO < 2 mg/L will be discarded due to non-linearity 
#  Run time is about 3.5 minutes

MN.ALL.Data = read_csv("MN.ALL.DATA.csv") 


#Filtering
filter1.mn <- MN.ALL.Data %>% #Remove impossible values ~ These likely came from input error
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")),
         Profile = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/")) %>%
  filter(Temperature < 40) #Jane et al 2021

#  Summary of filter #843450 obs
# filter1.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #7,217 Sample Sites
# filter1.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) # 2,440 Lakes
# filter1.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #76 Years
# filter1.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #20,676 Lake-Years
# filter1.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #83,011 Profiles

filter2.mn = filter1.mn %>%            #Profiles must have a minimum depth less than 3 meters; 
  group_by(Profile)%>%                 #otherwise the profiles shallowest read could be in 
  summarise(min.depth = min(Depth))%>% #the metalimnion and capture none of the epilimnion
  filter(min.depth < 3) #Jane et al 2021

filter3.mn = filter1.mn %>%
  semi_join(filter2.mn, by = "Profile") 

# # Summary #842671 obs
# filter3.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #7,196 Sample Sites
# filter3.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #2,440 Lakes
# filter3.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #74 Years
# filter3.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #20,639 Lake-Years
# filter3.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #82,684 Profiles


#  We want to make sure that lakes were sampled in both the spring post stratification
#  and later in the summer. This ensures that we can calculate VHOD
filter4.mn = filter3.mn %>%
  filter(DOY >= 121 &
           DOY <= 258)%>%
  group_by(Year, MonitoringLocationIdentifier)%>% 
  summarise(Early=min(DOY),Late=max(DOY), N=n_distinct(DOY))%>% 
  filter(N>1)%>%
  filter(Early >= 121 & #The early sample must be taken between DOY 121 & 166
           Early <= 166 &
           Late >= 196 & #and this adds that the last sample was taken between DOY 196 and 258
           Late <= 258)%>%
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/"))) #11,289 obs of 6 variables (Lost 16917 Lake-Year Combos)

#  Finally, we put it all together
filter5.mn = filter3.mn %>%
  semi_join(filter4.mn, by = "Location_Year") %>% 
  filter(DOY >= 121 &
           DOY <= 258) #Removes profiles where it may have been sampled while ice is on or during turnover

# Summary #495659 obs
# filter5.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,304 Site
# filter5.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,332 DOW
# filter5.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #49 Years
# filter5.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #5,744 Lake-Years
# filter5.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #46,255 Profiles

# Remove Lake Superior Sites, Mines, and Pits ~ We only want Glacial Lakes
filter6.mn = filter5.mn %>%
  filter(MonitoringLocationIdentifier != "EPA_GLNPO-SU19") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "EPA_GLNPO-SU18") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "EPA_GLNPO-SU104550")%>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "21GPBCH-LS_PT_1") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "21GPBCH-LS_PT_9") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "GPC5_WQX-LS_PT_1") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "GPC5_WQX-LS_PT_9") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "GPC5_WQX-LS_PT_8") %>% #Lake Superior Site
  filter(MonitoringLocationIdentifier != "MNPCA-69-1297-00-202") %>% #Sherman Pit
  filter(MonitoringLocationIdentifier != "MNPCA-69-0429-00-201") %>% #Sabin Pit
  filter(MonitoringLocationIdentifier != "21GPBCH-LS_PT_8") %>%      #Lake Superior Site
  filter(MonitoringLocationIdentifier != "MNPCA-18-0440-01-100") %>% #Mahnomen Mine
  filter(MonitoringLocationIdentifier != "MNPCA-69-1297-00-201") %>% #Sherman Pit
  filter(MonitoringLocationIdentifier != "MNPCA-18-0439-00-100") %>% #Pennington Mine
  filter(MonitoringLocationIdentifier != "MNPCA-31-1325-04-201") %>% #Canisteo Mine Pit
  filter(MonitoringLocationIdentifier != "MNPCA-69-1298-00-203") %>% #South Twin City Pit
  filter(MonitoringLocationIdentifier != "MNPCA-18-0093-01-202") %>% #Rabbit Lake
  filter(MonitoringLocationIdentifier != "MNPCA-18-0041-01-201") %>% #Crooked Lake - Silver Bay
  filter(MonitoringLocationIdentifier != "MNPCA-69-0429-00-100") %>% #Embarrass Mine
  filter(MonitoringLocationIdentifier != "MNPCA-69-0428-00-201") %>% #St. James Pit
  filter(DOW != "18044001")%>% #Mahnomen Mine
  filter(DOW != "69129700")%>% #Sherman Pit
  filter(DOW != "18043900")%>% #Pennington Mine
  filter(DOW != "31132504")%>% #Canisteo Mine Pit
  filter(DOW != "69129800")%>% #South Twin City Pit
  filter(DOW != "18009301")%>% #Rabbit Lake
  filter(DOW != "18004101")%>% #Crooked Lake - Silver Bay
  filter(DOW != "69042900")%>% #Embarrass Mine
  filter(DOW != "69042800")%>% #St. James Pit
  filter(DOW != "25000100") #Lake Pepin
# # Summary #492712 obs
# filter6.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,293 Sample Site
# filter6.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,327 Lakes
# filter6.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #49 Years
# filter6.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #5,733 Lake-Years
# filter6.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #45,918 Profiles

filter7.1mn = filter6.mn %>%                #Profiles must have at least 3 reads in them
  group_by(Profile)%>%
  summarise(n_depths = n_distinct(Depth))%>%
  filter(n_depths >= 3) #Jane et al 2021

filter7.mn = filter6.mn %>%
  semi_join(filter7.1mn, by = "Profile")

# #  Summary #485927 obs
# filter7.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,119 Lakes
# filter7.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,263 Lakes
# filter7.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #48 Years
# filter7.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #5,159 Lake-Years
# filter7.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #40,245 Profiles

# Calculate the top and bottom of the metalimnion and remove profiles where it did not work
filter8.mn = filter7.mn %>%
  group_by(Profile)%>%
  mutate(Meta = list(meta.depths(wtr = Temperature, depths = Depth, slope = 0.1, seasonal = F)))%>%
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(Depth))%>%
  drop_na(top.hypo)%>%  #Jane et al 2021
  filter(top.hypo != "0") 

# #Summary #463578 obs
# filter8.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #2,072 Sample Sites
# filter8.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,238 Lakes
# filter8.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #48 Years
# filter8.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #5,064 Lake-Years
# filter8.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #36,722 Profiles

#Make a ratio across all lakes of the top of the hypolimnion to the max depth
filter9.1mn = filter8.mn %>%
  group_by(Location_Year)%>%
  filter(DOY == max(DOY))%>%
  distinct(DOY, .keep_all = T)%>%
  mutate(ratio = top.hypo/Max_Depth)%>%
  filter(ratio < 1)    #Remove profiles where the ratio is greater than one (Also would mean that the meta depths function didn't work)
#Extract the median of these ratios
ratio = skim(filter9.1mn$ratio)%>%
  pull(numeric.p50)

filter9.mn = filter8.mn %>%
  semi_join(filter9.1mn, by = "Location_Year")


# #Summary #444057 obs
# filter9.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #1,986 Sample Sites
# filter9.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,177 Lakes
# filter9.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #48 Years
# filter9.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #4,703 Lake-Years
# filter9.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #34,448 Profiles

#See that each lake has a profile depth in the median hypolimnion
filter10.mn = filter9.mn %>%
  group_by(Profile)%>%
  mutate(Max_Profile = max(Depth))%>%
  filter(Max_Profile > Max_Depth * (ratio)) 

# #Summary #390099 obs
# filter10.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #1,708 Sample Sites
# filter10.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #1,105 Lakes
# filter10.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #48 Years
# filter10.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #4,320 Lake-Years
# filter10.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #28,820 Profiles


# Check start and end date again after losing profiles
filter11.1mn = filter10.mn %>%
  filter(DOY >= 121 &
           DOY <= 258)%>%
  group_by(Year, MonitoringLocationIdentifier)%>% 
  summarise(Early=min(DOY),Late=max(DOY), N=n_distinct(DOY))%>% 
  filter(N>1)%>%
  filter(Early >= 121 & 
           Early <= 166 &
           Late >= 196 & 
           Late <= 258)%>%
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")))
filter11.mn = filter10.mn %>%
  semi_join(filter11.1mn, by = "Location_Year")%>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))


# #Summary #376380 obs
# filter11.mn %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #1,459 Sample Sites
# filter11.mn %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #980 Lakes
# filter11.mn %>%
#   group_by(Year)%>%
#   summarise(n = n()) #48 Years
# filter11.mn %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #3,894 Lake-Years
# filter11.mn %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #27,582 Profiles

#  Filter for most sampled site at each lake
filter12.1mn = filter11.mn %>%
  group_by(DOW, MonitoringLocationIdentifier)%>%
  summarise(n = n())%>%
  group_by(DOW)%>%
  mutate(MostSampled = max(n))%>%
  ungroup()%>%
  filter(n == MostSampled)%>%
  distinct(DOW, .keep_all = T)
MN.Filtered.Obs = filter11.mn %>%
  semi_join(filter12.1mn)

# #  Summary #298027 obs
# MN.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #980 Sample Sites
# MN.Filtered.Obs %>%
#   group_by(DOW)%>%
#   summarise(n = n()) #980 Lakes
# MN.Filtered.Obs %>%
#   group_by(Year)%>%
#   summarise(n = n()) #48 Years
# MN.Filtered.Obs %>%
#   group_by(DOW, Year)%>%
#   summarise(n = n()) #3,591 Lake-Years
# MN.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #22,060 Profiles




####  Vis check
# 
# MN.presample = filter11.mn %>%
#   mutate(SampleID = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
#   distinct(SampleID)
# Random.sample = tibble(SampleID = sample(MN.presample$SampleID, size = 100))
# 
# filter11.mn%>%
#   mutate(SampleID = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
#   semi_join(Random.sample)%>%
#   mutate(Depth = as.numeric(Depth),
#          Temperature = as.numeric(Temperature),
#          top.hypo = as.numeric(top.hypo),
#          top.meta = as.numeric(top.meta))%>%
#   ggplot(aes(x = Depth, y = Temperature, group = SampleID))+
#   geom_point()+
#   geom_line()+
#   geom_vline(aes(group = SampleID, xintercept = top.hypo), color = "blue")+
#   scale_x_reverse()+
#   coord_flip()+
#   labs(title = "Uses Ratio = 0.571")+
#   facet_wrap(~SampleID)

#### Filter check
# filtereffect = read_xlsx("filtereffects.xlsx")
# fe.obsperbegin=filtereffect %>%
#   ggplot(aes(x = FilterNo, y = ObsPerFromBegin, color = Filter.Description, id = No.ofObs))+
#   geom_point()+
#   theme(legend.position = "none")+
#   ylab("Percent of Observations From Beginning")
# ggplotly(fe.obsperbegin)
# 
# fe.ObsPerFromPrevious = filtereffect %>%
#   ggplot(aes(x = FilterNo, y = ObsPerFromPrevious, color = Filter.Description, id = No.ofObs))+
#   geom_point()+
#   theme(legend.position = "none")+
#   ylab("Percent of Observations From Previous")
# ggplotly(fe.ObsPerFromPrevious)
# 
# fe.No.ofObs = filtereffect %>%
#   ggplot(aes(x = FilterNo, y = No.ofObs, id = Filter.Description))+
#   geom_point()+
#   geom_line()
# ggplotly(fe.No.ofObs)
# 
# fe.lakesperbegin=filtereffect %>%
#   ggplot(aes(x = FilterNo, y = LakesPerFromBegin, color = Filter.Description, id = Lakes))+
#   geom_point()+
#   theme(legend.position = "none")+
#   ylab("Percent of Lakes From Beginning")
# ggplotly(fe.lakesperbegin)
# 
# fe.profilesperbegin=filtereffect %>%
#   ggplot(aes(x = FilterNo, y = ProfilePerFromBegin, color = Filter.Description, id = Profiles))+
#   geom_point()+
#   theme(legend.position = "none")+
#   ylab("Percent of Profiles From Beginning")
# ggplotly(fe.profilesperbegin)

#Clean
rm(filter1.mn, filter2.mn, filter3.mn,filter4.mn,filter5.mn, filter6.mn,
   filter7.1mn, filter7.mn, filter8.mn, filter9.1mn, filter9.mn,
   filter10.mn, filter11.1mn, filter11.mn, filter12.1mn, ratio)
gc()



###### Interpolate the Data Frame ===============================================
#  In this section linear interpolation is preformed on DO and Temperature 
#  observations. Then, the interpolation are rejoined to the metadata before
#  being sent off to calculate AHOD and VHOD
#  Run time of about 2 minutes


### The next step is to interpolate data at 1 m increments seq(3, 10, 0.5)  
Depth.df = tibble(Depth = seq(0, 68, 0.5)) #This creates a data frame to make sure we have data for the bottom of larger lakes
#  First DO
#  Pivot Wide
Wide.MN.DO.Obs = MN.Filtered.Obs %>% #There are more rows than profiles...
  ungroup()%>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  select(ID, Depth, DO) %>%
  pivot_wider(names_from = ID, values_from = DO, values_fn = mean)%>%
  right_join(Depth.df)%>%
  arrange(Depth)

#  Interpolate
Wide.Inter.MN.DO = na.approx(Wide.MN.DO.Obs, x = Wide.MN.DO.Obs$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.MN.DO = Wide.Inter.MN.DO %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  mutate(Max_Depth = as.numeric(Max_Depth))%>%
  filter(Depth < Max_Depth) # Makes sure that filled in values are less than the max depth of the lake


### Next is Temperature
#  Pivot Wide
Wide.MN.Temp.Obs = MN.Filtered.Obs %>%
  ungroup()%>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  select(ID, Depth, Temperature) %>%
  pivot_wider(names_from = ID, values_from = Temperature, values_fn = mean)%>%
  right_join(Depth.df)%>%
  arrange(Depth)

#  Interpolate
Wide.Inter.MN.Temp = na.approx(Wide.MN.Temp.Obs, x = Wide.MN.Temp.Obs$Depth)%>% #interpolates data - 75 was chosen to make sure every lake was fully interpolated
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.MN.Temp = Wide.Inter.MN.Temp %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  mutate(Max_Depth = as.numeric(Max_Depth))%>%
  filter(Depth < Max_Depth) # Makes sure that filled in values are less than the max depth of the lake

### Join our results together
MN.joined.inter = inner_join(Long.Inter.MN.Temp, Long.Inter.MN.DO, by=c('Depth','ID',
  'MonitoringLocationIdentifier','Year','DOY','Max_Depth'))

#  We need to get the Lat and Lon
MN.Depth.Specific = MN.Filtered.Obs %>%
  ungroup()%>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(c(MonitoringLocationIdentifier, Latitude, Longitude, DOW))%>%
  inner_join(MN.joined.inter)%>%
  mutate(DOY = as.numeric(DOY))%>%
  group_by(MonitoringLocationIdentifier, Year, DOY)%>%
  distinct(Depth, .keep_all = T)



###  Next we will thermofilter the data set

MN.DS.Thermo = MN.Depth.Specific %>%
  group_by(MonitoringLocationIdentifier, Year, DOY)%>%
  mutate(Meta = list(meta.depths(wtr = Temperature, depths = Depth, slope = 0.1, seasonal = F)))%>% #Following the methods of Jane et al, 2021
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(Depth))

#write file for UMD students to use
#MN.DS.Thermo<-ungroup(MN.DS.Thermo)
#write.csv(MN.DS.Thermo, "MNprofiles_0.5.csv")

#see if how many lakes we have trend data for
#MN.DS.Thermo$Year<-as.integer(MN.DS.Thermo$Year)
#MN.DS.Thermo$DOW<-as.factor(MN.DS.Thermo$DOW)
df1<-MN.DS.Thermo%>%filter(DOY>196 & DOY<248)%>%
  group_by(Year, DOW)%>%
  summarise(min=min(DOY),max=max(DOY))%>%
  group_by(DOW)%>%
  summarise(min=min(Year),max=max(Year),range=max-min)%>%
  filter(range>14)
#  VIS Check of the porfiles remaining
# MN.presample = MN.DS.Thermo %>%
#   mutate(SampleID = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
#   distinct(SampleID)
# Random.sample = tibble(SampleID = sample(MN.presample$SampleID, size = 100))
#   
# MN.DS.Thermo%>%
#   mutate(SampleID = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/"))%>%
#   semi_join(Random.sample)%>%
#   mutate(Depth = as.numeric(Depth),
#          Temperature = as.numeric(Temperature),
#          top.hypo = as.numeric(top.hypo),
#          top.meta = as.numeric(top.meta))%>%
#   ggplot(aes(x = Depth, y = Temperature, group = SampleID))+
#   geom_point()+
#   geom_line()+
#   geom_vline(aes(group = SampleID, xintercept = top.hypo), color = "blue")+
#   scale_x_reverse()+
#   coord_flip()+
#   facet_wrap(~SampleID)



#  Data Summary #343391
# MN.DS.Thermo %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n=n()) #980 Sample Site
# MN.DS.Thermo %>%
#   group_by(DOW)%>%
#   summarise(n=n()) #980 Lakes
# MN.DS.Thermo %>%
#   group_by(Year)%>%
#   summarise(n=n()) #48 Years
# MN.DS.Thermo %>%
#   group_by(DOW, Year)%>%
#   summarise(n=n()) #3,591 Lake-Years
# MN.DS.Thermo %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n=n()) #22,058 Profiles

#Clean up 
rm(MN.Filtered.Obs,MN.joined.inter,Long.Inter.MN.Temp,Wide.Inter.MN.Temp,
   Wide.MN.Temp.Obs,Long.Inter.MN.DO,Wide.Inter.MN.DO,Wide.MN.DO.Obs, Depth.df)
gc()


