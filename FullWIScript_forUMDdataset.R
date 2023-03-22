#MGLP Project
#Jacob Angus

#  Script for Wisconsin that can be run as a job
#  It can also be run in individual sections

library("tidyverse")
library("lubridate")
library("zoo")
library("compiler")
library("rLakeAnalyzer")
library("readxl")
library("broom")
library("skimr")

##### Data Cleaning, Preparation, and Joining ==================================
#  The goal of this section of the script is to filter lakes for
#  geometric ratio, combine data sets with metadata, and join Wisconsin
#  data sets together. It is split into three sections: WQP , NTL, and DNR
#  Run time of less than 1 minute
setwd("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/Jacob")
#  Load files
#  WQP
allWI.do = read_csv("./Wisconsin.Data/Profiles/WIDataRetrievalCombined.csv") #All WI Observations of Temp and DO from 1940 to 2020
WI.allsites = read_csv("./Wisconsin.Data/Metadata/WIDataRetrievalMetadata.csv") #Site data for all the observations above
LakeDepthArea = read_csv("./Wisconsin.Data/Metadata/lake_depth.csv") #Dataset containing the lake depth and area for all lakes in the US
Link = read_csv("./Wisconsin.Data/Metadata/lake_link.csv") #Dataset that can link the Lagos dataset to the STORET dataset

#  NTL Data
#  The NTL Data needs a bit of cleaning while loading it in
WINTL = read_csv("./Wisconsin.Data/Profiles/WIntl29_v11.csv") %>% #Data from 1984-2021 on the main NTL Lakes
  mutate(Date = ymd(sampledate),
         Year = year(Date),
         DOY = yday(Date),
         Depth = depth,
         Temperature = wtemp,
         DO = o2)%>%
  dplyr::select(lakeid, Date, Year, DOY, Depth, Temperature, DO)
NTL = read_csv("./Wisconsin.Data/Profiles/NTLTER_data.csv") %>% #Data from UNDERC from 1984-2016
  mutate(Year = year4,
         DOY = daynum, 
         Date = mdy(sampledate),
         Depth = depth,
         Temperature = temperature_C,
         DO = dissolvedOxygen)%>%
  dplyr::select(c(lakeid, Year, DOY, Date, Depth, Temperature, DO))
NTL.Mendota = read_csv("./Wisconsin.Data/Profiles/mendota_2017_2020.csv") %>% #Data from 2017-2020 for Lake Mendota
  mutate(Date = mdy(sampledate),
         Year = year(Date),
         DOY = yday(Date),
         Depth = depth,
         Temperature = wtemp,
         DO = do_raw)%>%
  dplyr::select(c(lakeid, Year, DOY, Date, Depth, Temperature, DO))
NTL.Crystal = read_csv("./Wisconsin.Data/Profiles/NTLTER_crystal_2011_2014.csv") %>% #Data from 2011-2014 for Crystal Lake
  separate(sampledate, into = c("sampledate", "time"), sep = " ") %>%
  mutate(Year = year4,
         DOY = daynum, 
         Date = mdy(sampledate),
         Depth = depth_calculated,
         Temperature = water_temp,
         DO = opt_do2, 
         lakeid = "CR") %>%
  dplyr::select(c(Year, DOY, Date, Depth, Temperature, DO, lakeid))
NTL.Landscape = read_csv("./Wisconsin.Data/Profiles/NTLTER_landscape_1998_1999.csv")%>% #Data from 1998-1999 for a landscape survey
  mutate(Date = mdy(sampledate),
         Year = year(Date),
         DOY = yday(Date),
         Depth = depth,
         Temperature = wtemp,
         DO = o2,
         lakename = lake)%>%
  dplyr::select(c(Year, DOY, Date, Depth, Temperature, DO, lakename))
NTL.Meta = read_xlsx("./Wisconsin.Data/Metadata/WINTLMetadata.xlsx")%>% #I created a metadata file to link NTL "lakeid" to wibic
  mutate(MonitoringLocationIdentifier = paste("WIDNR_WQX", wibic, sep = "-"))
WI.meta = readxl:: read_xlsx("./Wisconsin.Data/Metadata/WI.Lakes.xlsx") #WIDNR supplemented lake finder data
names(WI.meta) = c("WIBIC", "lakename", "surfacearea", 
                   "maxdepth", "meandepth", "Latitude", "Longitude", 
                   "PublicLanding", "PublicBeach", "PublicPark", 
                   "Fish", "LakeType", "WaterClarity", "County")

#  DNR
WI.DNR = read_csv("./Wisconsin.Data/Profiles/WIProfileData.csv")%>% #This only contains observations useful for TDO3 caluclations
  mutate(wibic = WBIC,
         Date = mdy(START_DATETIME),
         Depth = Depth_m,
         Temperature = Temp_C,
         DO = DO_mgL,
         lakename = OFFICIAL_NAME)%>%
  dplyr::select(c(wibic,Date,Depth,Temperature,DO, lakename))


#### Filter Metadata
#  WQP
#  To remove lakes that don't stratify
WI.LakeDepthArea = LakeDepthArea %>% 
  filter(lake_states == "WI")%>% #We are only looking at WI lakes right now
  mutate(lake_area_m2 = lake_waterarea_ha * 10000,
         Max_Depth = lake_maxdepth_m)%>% #Fang and Stefan 2009 uses the As^0.25:Z with As in square meters
  mutate(GR = (lake_area_m2^0.25)/Max_Depth)%>%
  filter(GR < 4) #Lakes with a GR greater than 4 do not stratify
#  Connect it all together
WI.WQP = Link %>%
  dplyr::select(c("lagoslakeid","wqp_monitoringlocationidentifier")) %>% #get rid of extra columns in the data to keep it simpler
  inner_join(WI.LakeDepthArea, by = "lagoslakeid") %>% #lagoslakeid also contains all the basins for lakes
  mutate(MonitoringLocationIdentifier = wqp_monitoringlocationidentifier) %>% #Making the metadata files share columns
  inner_join(WI.allsites, by = "MonitoringLocationIdentifier") %>% #Joining by wqp monitoring location identifier
  inner_join(allWI.do, by = "MonitoringLocationIdentifier") #Join to Observation

#  Prepare for joining of the Wisconsin Data
#  Convert from feet to meters
WI.feet.WQP = WI.WQP %>% 
  filter(ActivityDepthHeightMeasure.MeasureUnitCode == "feet"|
           ActivityDepthHeightMeasure.MeasureUnitCode == "ft")%>%
  mutate(Depth = round(ActivityDepthHeightMeasure.MeasureValue * 0.3038))
#  Join them together
WI.meters.WQP = WI.WQP %>%
  filter(ActivityDepthHeightMeasure.MeasureUnitCode == "m" |
           ActivityDepthHeightMeasure.MeasureUnitCode == "meters") %>%
  mutate(Depth = round(ActivityDepthHeightMeasure.MeasureValue))%>%
  full_join(WI.feet.WQP)%>%
  dplyr::select(!ActivityDepthHeightMeasure.MeasureUnitCode)

#  Convert Fahrenheit to Celsius
WI.fah.WQP = WI.meters.WQP %>% 
  filter(CharacteristicName == "Temperature, water")%>%
  filter(ResultMeasure.MeasureUnitCode == "deg F")%>%
  mutate(Temperature = (ResultMeasureValue - 32) * 5/9)
#  Join them together and get proper columns
WI.cel.WQP = WI.meters.WQP %>% 
  filter(CharacteristicName == "Temperature, water")%>%
  filter(ResultMeasure.MeasureUnitCode == "deg C")%>%
  mutate(Temperature = ResultMeasureValue)%>%
  full_join(WI.fah.WQP) %>%
  mutate(Latitude = lake_lat_decdeg                                    ,
         Longitude = lake_lon_decdeg                                   ,
         Max_Depth = lake_maxdepth_m                                   ,
         MonitoringLocationIdentifier                                  ,
         Date = ActivityStartDate                                      ,
         DOY = yday(Date),
         Year = year(Date))%>%
  dplyr::select(c(MonitoringLocationIdentifier, Year, Date, DOY, 
           Depth, Temperature, Latitude, Longitude, Max_Depth, OrganizationIdentifier.y))

#  Separate DO from Temperature, then pull them back together
WI.WQP.join = WI.meters.WQP %>%
  filter(ResultMeasure.MeasureUnitCode == "mg/l")%>%
  mutate(DO = ResultMeasureValue,
         Latitude = as.double(lake_lat_decdeg)                         ,
         Longitude = as.double(lake_lon_decdeg)                        ,
         Max_Depth = lake_maxdepth_m                                   ,
         MonitoringLocationIdentifier                                  ,
         Date = ActivityStartDate                                      ,
         DOY = yday(Date),
         Year = year(Date)) %>%
  dplyr::select(c(MonitoringLocationIdentifier, Year, Date, DOY, 
           Depth, DO, Latitude, Longitude, Max_Depth)) %>%
  inner_join(WI.cel.WQP) %>%
  group_by(MonitoringLocationIdentifier, Year, Date, DOY, 
           Depth, Latitude, Longitude, Max_Depth)%>%
  summarise(DO = mean(DO), #This will remove duplicates
            Temperature = mean(Temperature))%>%
  ungroup()


#  Clean
rm(WI.meters.WQP, WI.fah.WQP, WI.cel.WQP, WI.feet.WQP, WI.WQP, WI.LakeDepthArea)
gc()



#  NTL
#  Join the observations together
NTL.data = full_join(NTL, WINTL)
#  Need to clean up this metadata file
WI.NTL.forjoin = WI.meta %>%
  drop_na(maxdepth) %>%
  separate(maxdepth, into = c("Max_Depthft", "Feet"), sep = " ")%>%
  mutate(Max_Depth = as.numeric(Max_Depthft) * 0.3038,#convert feet to meters
         wibic = as.double(WIBIC),
         waterarea_m2 = 4046.856 * as.numeric(surfacearea),
         GR = (waterarea_m2^0.25)/Max_Depth)%>%
  filter(GR < 4) %>%
  dplyr::select(c(wibic, Max_Depth))%>%
  inner_join(NTL.Meta) %>%
  inner_join(NTL.data) %>%
  mutate(Profile = paste(wibic, Year, DOY, sep = "/"))

#  Add in Mendota Data
NTL.Mendota.Meta = WI.meta %>% #  Join to metadata
  drop_na(maxdepth) %>%
  separate(maxdepth, into = c("Max_Depthft", "Feet"), sep = " ")%>%
  mutate(Max_Depth = as.numeric(Max_Depthft) * 0.3038,#convert feet to meters
         wibic = as.double(WIBIC),
         waterarea_m2 = 4046.856 * as.numeric(surfacearea),
         GR = (waterarea_m2^0.25)/Max_Depth)%>%
  filter(GR < 4) %>%
  dplyr::select(c(wibic, Max_Depth))%>%
  inner_join(NTL.Meta) %>%
  inner_join(NTL.Mendota)

NTL.Mendota.Profiles = NTL.Mendota.Meta %>%
  mutate(Profile = paste(wibic, Year, DOY, sep = "/"))%>%
  distinct(Profile)
Mendota.DuplicateProfiles = WI.NTL.forjoin %>%
  distinct(Profile)%>%
  inner_join(NTL.Mendota.Profiles)
WI.NTL_Mendota = NTL.Mendota.Meta %>%
  mutate(Profile = paste(wibic, Year, DOY, sep = "/"))%>%
  anti_join(Mendota.DuplicateProfiles)%>%
  full_join(WI.NTL.forjoin)

#  Clean
rm(NTL.Mendota, NTL.Mendota.Meta, NTL.Mendota.Profiles, Mendota.DuplicateProfiles,
   NTL, WINTL, WI.NTL.forjoin)

#  Add in Crystal Lake data
NTL.Crystal.Meta = WI.meta %>% #  Join to metadata
  drop_na(maxdepth) %>%
  separate(maxdepth, into = c("Max_Depthft", "Feet"), sep = " ")%>%
  mutate(Max_Depth = as.numeric(Max_Depthft) * 0.3038,#convert feet to meters
         wibic = as.double(WIBIC),
         waterarea_m2 = 4046.856 * as.numeric(surfacearea),
         GR = (waterarea_m2^0.25)/Max_Depth)%>%
  filter(GR < 4) %>%
  dplyr::select(c(wibic, Max_Depth))%>%
  inner_join(NTL.Meta) %>%
  inner_join(NTL.Crystal)

NTL.Crystal.Profiles = NTL.Crystal.Meta %>%
  mutate(Profile = paste(wibic, Year, DOY, sep = "/"))%>%
  distinct(Profile)
Crystal.DuplicateProfiles = WI.NTL_Mendota %>%
  distinct(Profile)%>%
  inner_join(NTL.Crystal.Profiles)
WI.NTL_Mendota_Crystal = NTL.Crystal.Meta %>%
  mutate(Profile = paste(wibic, Year, DOY, sep = "/"))%>%
  anti_join(Crystal.DuplicateProfiles)%>%
  full_join(WI.NTL_Mendota)

#  Clean 
rm(NTL.Crystal, NTL.Crystal.Meta, NTL.Crystal.Profiles, Crystal.DuplicateProfiles, 
   WI.NTL_Mendota)
gc()

#  Add Landscape Data
NTL.Landscape.Meta = WI.meta %>% #  Join to metadata
  drop_na(maxdepth) %>%
  separate(maxdepth, into = c("Max_Depthft", "Feet"), sep = " ")%>%
  mutate(Max_Depth = as.numeric(Max_Depthft) * 0.3038,#convert feet to meters
         wibic = as.double(WIBIC),
         waterarea_m2 = 4046.856 * as.numeric(surfacearea),
         GR = (waterarea_m2^0.25)/Max_Depth)%>%
  filter(GR < 4) %>%
  dplyr::select(c(wibic, Max_Depth))%>%
  inner_join(NTL.Meta) %>%
  inner_join(NTL.Landscape)

NTL.Landscape.Profiles = NTL.Landscape.Meta %>%
  mutate(Profile = paste(wibic, Year, DOY, sep = "/"))%>%
  distinct(Profile)
Landscape.DuplicateProfiles = WI.NTL_Mendota_Crystal %>%
  distinct(Profile)%>%
  inner_join(NTL.Landscape.Profiles)
WI.NTL_Mendota_Crystal_Landscape = NTL.Landscape.Meta %>%
  mutate(Profile = paste(wibic, Year, DOY, sep = "/"))%>%
  anti_join(Landscape.DuplicateProfiles)%>%
  full_join(WI.NTL_Mendota_Crystal)

#  Clean 
rm(NTL.Landscape, NTL.Landscape.Meta, NTL.Landscape.Profiles, Landscape.DuplicateProfiles,
   WI.NTL_Mendota_Crystal)
gc()

WI.NTL_Mendota_Crystal_Landscape%>%
  filter(Longitude == "NaN")

#  Round Depth
WI.NTL.Join = WI.NTL_Mendota_Crystal_Landscape%>%
  mutate(Depth = round(Depth),
         Longitude = as.double(Longitude),
         Latitude = as.double(Latitude),
         Max_Depth = round(Max_Depth))%>%
  group_by(wibic, Year, DOY, Depth, Latitude, Longitude, MonitoringLocationIdentifier, Date, Max_Depth)%>%
  summarise(DO = median(DO),
            Temperature = median(Temperature))



#  DNR
#  Clean up and filter the metadata
WI.DNR.META = WI.meta %>%
  drop_na(maxdepth) %>%
  separate(maxdepth, into = c("Max_Depthft", "Feet"), sep = " ")%>%
  mutate(Max_Depth = as.numeric(Max_Depthft) * 0.3038,#convert feet to meters
         wibic = as.double(WIBIC),
         waterarea_m2 = 4046.856 * as.numeric(surfacearea),
         GR = (waterarea_m2^0.25)/Max_Depth,
         Latitude = as.double(Latitude),
         Longitude = as.double(Longitude))%>%
  filter(GR < 4)%>%
  dplyr::select(c(wibic, Max_Depth, Latitude, Longitude))%>%
  inner_join(WI.DNR)%>%
  mutate(MonitoringLocationIdentifier = paste("WIDNR_WQX", wibic, sep = "-"))

### Join Data sets together
WI.Obs = full_join(WI.NTL.Join, WI.WQP.join)%>%
  full_join(WI.DNR.META)%>%
  mutate(Year = year(Date))

#Data Summary #154088 obs
# WI.Obs %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #1,211 Lakes
# WI.Obs %>%
#   group_by(Year)%>%
#   summarise(n = n()) #41 Years
# WI.Obs %>%
#   group_by(wibic, Year)%>%
#   summarise(n = n()) #2,499 Lake-Years
# WI.Obs %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #16,154 Profiles

#write_csv(WI.Obs, "./Wisconsin.Data/WI.ALL.DATA.csv")


#Clean
rm(allWI.do, LakeDepthArea, Link, NTL, NTL.data, NTL.Meta, WI.allsites,
   WI.LakeDepthArea, WI.NTL_Mendota_Crystal_Landscape, WI.NTL.Join,#removed'WI.meta' from this string
   WI.WQP,WI.WQP.join, WINTL, WI.NTL.Meta, WI.DNR, WI.DNR.META)
gc()



###### Filtering the Data in Preparation for VHOD Calculations =================
#  The goal of this section is to filter the data by several factors:
#  Lakes must have a profile at the start of stratification (121 <= DOY <= 166)
#  Lakes must have a profile at the end of stratification (196 <= DOY <= 258)
#  Profiles must be taken from glacial lakes
#  Profiles will be interpolated at 1m depths 
#  Data points where DO < 2 mg/L will be discarded due to non-linearity 
#  Run time is about 2 minutes

#  load data
#WI.Obs = read_csv("./Wisconsin.Data/WI.ALL.DATA.csv")


#Filtering
filter1.WI <- WI.Obs %>% #Remove impossible values ~ These likely came from input error
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")),
         Profile = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/")) %>%
  filter(Temperature < 40) #Jane et al 2021

#  Summary of filter #153237 obs
# filter1.WI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #1,209 Lakes
# filter1.WI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #41 Years
# filter1.WI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #5,128 Lake-Years
# filter1.WI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #16,110 Profiles

filter2.1WI = filter1.WI %>%            #Profiles must have a minimum depth less than 3 meters; 
  group_by(Profile)%>%                 #otherwise the profiles shallowest read could be in 
  summarise(min.depth = min(Depth))%>% #the metalimnion and capture none of the epilimnion
  filter(min.depth < 3) #Jane et al 2021

filter2.WI = filter1.WI %>%
  semi_join(filter2.1WI, by = "Profile") 

# # Summary #152602 obs
# filter2.WI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #1,197 Lakes
# filter2.WI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #41 Years
# filter2.WI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #5047 Lake-Years
# filter2.WI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #15,648 Profiles


#  We want to make sure that lakes were sampled in both the spring post stratification
#  and later in the summer. This ensures that we can calculate VHOD
filter3.1.WI = filter2.WI %>%
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
filter3.WI = filter2.WI %>%
  semi_join(filter3.1.WI, by = "Location_Year") %>% 
  filter(DOY >= 121 &
           DOY <= 258) #Removes profiles where it may have been sampled while ice is on or during turnover

# Summary #70776 obs
# filter3.WI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #435 Lakes
# filter3.WI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #41 Years
# filter3.WI %>%
#   group_by(wibic, Year)%>%
#   summarise(n = n()) #298 Lake-Years
# filter3.WI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #6,281 Profiles


filter4.1WI = filter3.WI %>%                #Profiles must have at least 3 reads in them
  group_by(Profile)%>%
  summarise(n_depths = n_distinct(Depth))%>%
  filter(n_depths >= 3) #Jane et al 2021

filter4.WI = filter3.WI %>%
  semi_join(filter4.1WI, by = "Profile")

# #  Summary #67524 obs
# filter4.WI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #148 Lakes
# filter4.WI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #41 Years
# filter4.WI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #475 Lake-Years
# filter4.WI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,737 Profiles

# Calculate the top and bottom of the metalimnion and remove profiles where it did not work
filter5.WI = filter4.WI %>%
  group_by(Profile)%>%
  mutate(Meta = list(meta.depths(wtr = Temperature, depths = Depth, slope = 0.1, seasonal = F)))%>%
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(Depth))%>%
  drop_na(top.hypo)%>%  #Jane et al 2021
  filter(top.hypo != "0") 

# #Summary #66075 obs
# filter5.WI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #147 Lakes
# filter5.WI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #41 Years
# filter5.WI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #474 Lake-Years
# filter5.WI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,640 Profiles

#Make a ratio across all lakes of the top of the hypolimnion to the max depth
filter6.1WI = filter5.WI %>%
  group_by(Location_Year)%>%
  filter(DOY == max(DOY))%>%
  distinct(DOY, .keep_all = T)%>%
  mutate(ratio = top.hypo/Max_Depth)%>%
  filter(ratio < 1)    #Remove profiles where the ratio is greater than one (Also would mean that the meta depths function didn't work)
#Extract the median of these ratios
ratio = skim(filter6.1WI$ratio)%>%
  pull(numeric.p50)

filter6.WI = filter5.WI %>%
  semi_join(filter6.1WI, by = "Location_Year")

# #Summary #65781 obs
# filter6.WI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #146 Lakes
# filter6.WI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #41 Years
# filter6.WI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #468 Lake-Years
# filter6.WI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,601 Profiles

#See that each lake has a profile depth in the median hypolimnion
filter7.WI = filter6.WI %>%
  group_by(Profile)%>%
  mutate(Max_Profile = max(Depth))%>%
  filter(Max_Profile > Max_Depth * (ratio)) 

# #Summary #64720 obs
# filter7.WI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #118 Lakes
# filter7.WI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #41 Years
# filter7.WI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #430 Lake-Years
# filter7.WI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,406 Profiles


# Check start and end date again after losing profiles
filter8.1WI = filter7.WI %>%
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
WI.Filtered.Obs = filter7.WI %>%
  semi_join(filter8.1WI, by = "Location_Year")%>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  ungroup()%>%
  mutate(Depth = floor(Depth))%>%
  group_by(ID, MonitoringLocationIdentifier,Year,DOY,Max_Depth,Latitude,
           Longitude, Date, Depth)%>%
  summarise(DO = median(DO),
            Temperature = median(Temperature))%>%
  ungroup()%>%
  filter(MonitoringLocationIdentifier != "LCOWIS_WQX-RND-1")%>% #Bay of Round Lake
  filter(MonitoringLocationIdentifier != "LCOWIS_WQX-LCO-3")%>% #Not the Deep hole of Lac Court
  filter(MonitoringLocationIdentifier != "LCOWIS_WQX-LCO-2")%>% #Not the Deep hole of Lac Court
  filter(MonitoringLocationIdentifier != "LCOWIS_WQX-RND-1")%>%
  filter(Depth>=0)


# #Summary #63608 obs
# WI.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #82 Lakes
# WI.Filtered.Obs %>%
#   group_by(Year)%>%
#   summarise(n = n()) #41 Years
# WI.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #357 Lake-Years
# WI.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,204 Profiles


#  Clean
rm(filter1.WI, filter2.WI, filter3.WI,filter4.WI,filter5.WI, filter6.WI,
   filter7.WI, filter2.1WI, filter3.1.WI, filter4.1WI,
   filter6.1WI, filter8.1WI, ratio)
gc()

###### Interpolate the Data Frame ===============================================
#  In this section linear interpolation is preformed on DO and Temperature 
#  observations. Then, the interpolation are rejoined to the metadata before
#  being sent off to calculate AHOD and VHOD
#  Run time of about 2 minutes

### The next step is to interpolate data at 1 m increments
Depth.df = tibble(Depth = 0:90) #This creates a data frame to make sure we have data for the bottom of larger lakes
#  First DO
#  Pivot Wide
Wide.WI.DO.Obs = WI.Filtered.Obs %>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  dplyr::select(ID, Depth, DO) %>%
  pivot_wider(names_from = ID, values_from = DO, values_fn = mean)%>%
  full_join(Depth.df)

#  Interpolate
Wide.Inter.WI.DO = na.approx(Wide.WI.DO.Obs, x = Wide.WI.DO.Obs$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.WI.DO = Wide.Inter.WI.DO %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)



### Next is Temperature
#  Pivot Wide
Wide.WI.Temp.Obs = WI.Filtered.Obs %>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  dplyr::select(ID, Depth, Temperature) %>%
  pivot_wider(names_from = ID, values_from = Temperature, values_fn = mean)%>%
  full_join(Depth.df)

#  Interpolate
Wide.Inter.WI.Temp = na.approx(Wide.WI.Temp.Obs, x = Wide.WI.Temp.Obs$Depth)%>% #interpolates data - 75 was chosen to make sure every lake was fully interpolated
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.WI.Temp = Wide.Inter.WI.Temp %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)

### Join our results together
WI.joined.inter = inner_join(Long.Inter.WI.Temp, Long.Inter.WI.DO)

#  We need to get the Lat and Lon
WI.Depth.Specific = WI.Filtered.Obs %>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  dplyr::select(c(MonitoringLocationIdentifier, Latitude, Longitude))%>%
  inner_join(WI.joined.inter)

#  Summary #60990 obs
# WI.Depth.Specific %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n=n()) #82 Lakes
# WI.Depth.Specific %>%
#   group_by(Year)%>%
#   summarise(n=n()) #41 Years
# WI.Depth.Specific %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n=n()) #357 Lake-Years
# WI.Depth.Specific %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n=n()) #3,174 Profiles


#Clean up 
rm(WI.joined.inter,Long.Inter.WI.Temp,Wide.Inter.WI.Temp,Depth.df,
   Wide.WI.Temp.Obs,Long.Inter.WI.DO,Wide.Inter.WI.DO,Wide.WI.DO.Obs)
gc()


###  Next we will thermofilter the data set

WI.DS.Thermo = WI.Depth.Specific %>%
  group_by(MonitoringLocationIdentifier, Year, DOY)%>%
  mutate(Meta = list(meta.depths(wtr = Temperature, depths = Depth, slope = 0.1, seasonal = F)))%>% #Following the methods of Jane et al, 2021
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(Depth))

WI.DS.Thermo<-WI.DS.Thermo%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)
#  Summary # 60990 obs
# WI.DS.Thermo %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n=n()) #82 Lakes
# WI.DS.Thermo %>%
#   group_by(Year)%>%
#   summarise(n=n()) #41 Years
# WI.DS.Thermo %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n=n()) #357 Lake-Years
# WI.DS.Thermo %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n=n()) #3,174 Profiles


#write file for UMD students to use
#WI.DS.Thermo<-ungroup(WI.DS.Thermo)
#write.csv(WI.DS.Thermo, "WIprofiles.csv")

#see if how many lakes we have trend data for

df1<-WI.DS.Thermo
df1$Year<-as.integer(df1$Year)
df1$ID<-as.factor(df1$ID)
df1$DOY<-as.integer(df1$DOY)

df2<-df1%>%  filter(DOY>196 & DOY<248)%>%
group_by(Year, MonitoringLocationIdentifier)%>%
summarise(min=min(DOY),max=max(DOY))%>%
 group_by(MonitoringLocationIdentifier)%>%
  summarise(min=min(Year),max=max(Year),range=max-min)%>%
 filter(range>9)
