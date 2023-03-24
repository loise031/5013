#MGLP Project
#Jacob Angus

#  Script for Michigan that can be run as a job
#  It can also be run in individual sections

library("tidyverse")
library("lubridate")
library("zoo")
library("compiler")
library("rLakeAnalyzer")
library("readxl")
library("skimr")

setwd("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/Jacob")

##### Data Cleaning, Preparation, and Joining ==================================
#  The goal of this section of the script is to filter lakes for
#  geometric ratio, combine data sets with metadata, and join Minnesota
#  data sets together.  
#  Run time 

#  Load files
#  WQP
allMI.do = read.csv("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/Jacob/Michigan.Data/Profiles/MIDataRetrievalCombined.csv",
                    fileEncoding = "latin1") #All MI Observations of Temp and DO from 1940 to 2020
MI.allsites = read_csv("./Michigan.Data/Metadata/MIDataRetrievalMetadata.csv") #Site data for all the observations above
LakeDepthArea = read_csv("./Michigan.Data/Metadata/lake_depth.csv") #Dataset containing the lake depth and area for all lakes in the US
Link = read_csv("./Michigan.Data/Metadata/lake_link.csv") #Dataset that can link the Lagos dataset to the STORET dataset



#  DNR Data
MI.DNR = read_csv("./Michigan.Data/Profiles/MichiganProfiles.csv")
names(MI.DNR) = c("Lake.Name","County", "Date.Sampled",
               "CONCATENATE", "Latitude", "Longitude", "STORETID",
               "Township", "Section", "Site.ID", "Watershed", "Surface.Area",
               "Datum", "GPS.Source", "Collecting.Organization","Nothing", "Time.Sampled",
               "Weather.Conditions", "Sampling.Depth..feet.", "Meter.Type",
               "Meter.ID", "Calibration.DO...air.saturation.", "Calibration.Temp...C.",
               "Lake.Altitude.Value", "Unusual.Conditions", "Comments", "Depth..feet.",
               "Temp...C.", "DO.Level..mg.L.","Tier")
MI.DNR = MI.DNR %>%
  mutate(MonitoringLocationIdentifier = paste("21MICH", STORETID, sep = "-"))%>%
  select(!c(Latitude, Longitude))



### Filter Metadata
#  WQP
#  To remove lakes that don't stratify
MI.LakeDepthArea = LakeDepthArea %>% 
  filter(lake_states == "MI")%>% #We are only looking at MI lakes right now
  mutate(lake_area_m2 = lake_waterarea_ha * 10000,
         Max_Depth = lake_maxdepth_m)%>% #Fang and Stefan 2009 uses the As^0.25:Z with As in square meters
  mutate(GR = (lake_area_m2^0.25)/Max_Depth)%>%
  filter(GR < 4) #Lakes with a GR greater than 4 do not stratify
#  Connect it all together
MI.WQP = Link %>%
  select(c("lagoslakeid","wqp_monitoringlocationidentifier")) %>% #get rid of extra columns in the data to keep it simpler
  inner_join(MI.LakeDepthArea, by = "lagoslakeid") %>% #lagoslakeid also contains all the basins for lakes
  mutate(MonitoringLocationIdentifier = wqp_monitoringlocationidentifier) %>% #Making the metadata files share columns
  inner_join(MI.allsites, by = "MonitoringLocationIdentifier") %>% #Joining by wqp monitoring location identifier
  inner_join(allMI.do, by = "MonitoringLocationIdentifier") #Join to Observation


#  Prepare for joining of the Michigan Data
#  Convert from feet to meters
MI.feet.WQP = MI.WQP %>% 
  filter(ActivityDepthHeightMeasure.MeasureUnitCode == "feet"|
           ActivityDepthHeightMeasure.MeasureUnitCode == "ft")%>%
  mutate(Depth = round(ActivityDepthHeightMeasure.MeasureValue * 0.3038))
#  Join them together
MI.meters.WQP = MI.WQP %>%
  filter(ActivityDepthHeightMeasure.MeasureUnitCode == "m" |
           ActivityDepthHeightMeasure.MeasureUnitCode == "meters") %>%
  mutate(Depth = round(ActivityDepthHeightMeasure.MeasureValue))%>%
  full_join(MI.feet.WQP)%>%
  select(!ActivityDepthHeightMeasure.MeasureUnitCode)

#  Convert Fahrenheit to Celsius
MI.fah.WQP = MI.meters.WQP %>% 
  filter(CharacteristicName == "Temperature, water")%>%
  filter(ResultMeasure.MeasureUnitCode == "deg F")%>%
  mutate(Temperature = (ResultMeasureValue - 32) * 5/9)
#  Join them together and get proper columns
MI.cel.WQP = MI.meters.WQP %>% 
  filter(CharacteristicName == "Temperature, water")%>%
  filter(ResultMeasure.MeasureUnitCode == "deg C")%>%
  mutate(Temperature = ResultMeasureValue)%>%
  full_join(MI.fah.WQP) %>%
  mutate(Latitude = lake_lat_decdeg                                    ,
         Longitude = lake_lon_decdeg                                   ,
         Max_Depth = round(lake_maxdepth_m)                            ,
         MonitoringLocationIdentifier                                  ,
         Date = ActivityStartDate                                      ,
         DOY = yday(Date),
         Year = year(Date))%>%
  select(c(MonitoringLocationIdentifier, Year, Date, DOY, 
           Depth, Temperature, Latitude, Longitude, Max_Depth))

#  Separate DO from Temperature, then pull them back together
MI.WQP.join = MI.meters.WQP %>%
  filter(ResultMeasure.MeasureUnitCode == "mg/l")%>%
  mutate(DO = ResultMeasureValue,
         Latitude = lake_lat_decdeg                                    ,
         Longitude = lake_lon_decdeg                                   ,
         Max_Depth = round(lake_maxdepth_m)                            ,
         MonitoringLocationIdentifier                                  ,
         Date = ActivityStartDate                                      ,
         DOY = yday(Date),
         Year = year(Date)) %>%
  select(c(MonitoringLocationIdentifier, Year, Date, DOY, 
           Depth, DO, Latitude, Longitude, Max_Depth)) %>%
  inner_join(MI.cel.WQP) %>%
  group_by(MonitoringLocationIdentifier, Year, Date, DOY, 
           Depth, Latitude, Longitude, Max_Depth)%>%
  summarise(DO = median(DO), #This will remove duplicates
            Temperature = median(Temperature))%>%
  ungroup()

#Clean
rm(MI.allsites, MI.cel.WQP, MI.fah.WQP, MI.feet.WQP,
   MI.meters.WQP, MI.WQP, allMI.do)
gc()

# Data Summary #7269 obs
# MI.WQP.join %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #42 sites
# MI.WQP.join %>%
#   group_by(Year)%>%
#   summarise(n = n()) #17 Years
# MI.WQP.join %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #136 Site-Years
# MI.WQP.join %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #649 Profiles




#DNR 
#  To remove lakes that don't stratify
MI.LakeDepthArea = LakeDepthArea %>% 
  filter(lake_states == "MI")%>% #We are only looking at MI lakes right now
  mutate(lake_area_m2 = lake_waterarea_ha * 10000,
         Max_Depth = lake_maxdepth_m)%>% #Fang and Stefan 2009 uses the As^0.25:Z with As in square meters
  mutate(GR = (lake_area_m2^0.25)/Max_Depth)%>%
  filter(GR < 4) #Lakes with a GR greater than 4 do not stratify
#  Connect it all together
MI.DNR.META = Link %>%
  select(c("lagoslakeid","wqp_monitoringlocationidentifier")) %>% #get rid of extra columns in the data to keep it simpler
  inner_join(MI.LakeDepthArea, by = "lagoslakeid") %>% #lagoslakeid also contains all the basins for lakes
  mutate(MonitoringLocationIdentifier = wqp_monitoringlocationidentifier,
         Latitude = as.double(lake_lat_decdeg),
         Longitude = as.double(lake_lon_decdeg)) %>%#Making the files share columns
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(c(MonitoringLocationIdentifier, Max_Depth, Latitude, Longitude))%>%
  inner_join(MI.DNR, by = "MonitoringLocationIdentifier") %>% #Join to observations
  mutate(Depth = round(Depth..feet. * 0.3038),
         Temperature = Temp...C.,
         DO = DO.Level..mg.L.,
         Date = ymd(Date.Sampled),
         Year = year(Date),
         DOY = yday(Date))%>%
  select(c(MonitoringLocationIdentifier, Date, Year, DOY, 
           Depth, Temperature, DO, Max_Depth, Latitude, Longitude))

#Clean
rm(Link, LakeDepthArea, MI.LakeDepthArea, MI.DNR)
gc()

####Join the Complete Observations Together
MI.WQP.join$Date<-as.Date(MI.WQP.join$Date, format = "%Y-%m-%d")
MI.ALL.Data = MI.WQP.join %>% 
  full_join(MI.DNR.META)%>%
  mutate(Max_Depth = round(Max_Depth))%>%
  group_by(MonitoringLocationIdentifier, Date)%>%
  distinct(Depth, .keep_all = T)%>%
  ungroup()
  

# Data Summary #60949 obs
# MI.ALL.Data %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #143 sites
# MI.ALL.Data %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# MI.ALL.Data %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #677 Site-Years
# MI.ALL.Data %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #4,417 Profiles



#write_csv(MI.ALL.Data, "./Michigan.Data/MI.ALL.DATA.csv")


#Clean
rm(MI.DNR.META, MI.WQP.join)
gc()


###### Filtering the Data in Preparation for VHOD Calculations =================
#  The goal of this section is to filter the data by several factors:
#  Lakes must have a profile at the start of stratification (121 <= DOY <= 166)
#  Lakes must have a profile at the end of stratification (196 <= DOY <= 258)
#  Profiles must be taken from glacial lakes
#  Profiles will be interpolated at 1m depths 
#  Data points where DO < 2 mg/L will be discarded due to non-linearity 
#  Run time is about 2 minutes

#  Load Data
#MI.ALL.Data = read_csv("./Michigan.Data/MI.ALL.DATA.csv")


#Filtering
filter1.MI <- MI.ALL.Data %>% #Remove impossible values ~ These likely came from input error
  mutate(Location_Year = as.character(paste(MonitoringLocationIdentifier,Year, sep = "/")),
         Profile = paste(MonitoringLocationIdentifier, Year, DOY, sep = "/")) %>%
  filter(Temperature < 40) #Jane et al 2021

#  Summary of filter #60934 obs
# filter1.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #143 Lakes
# filter1.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter1.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #677 Lake-Years
# filter1.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #4,414 Profiles

filter2.1MI = filter1.MI %>%            #Profiles must have a minimum depth less than 3 meters; 
  group_by(Profile)%>%                 #otherwise the profiles shallowest read could be in 
  summarise(min.depth = min(Depth))%>% #the metalimnion and capture none of the epilimnion
  filter(min.depth < 3) #Jane et al 2021

filter2.MI = filter1.MI %>%
  semi_join(filter2.1MI, by = "Profile") 

# # Summary #60917 obs
# filter2.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #143 Lakes
# filter2.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter2.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #677 Lake-Years
# filter2.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #4,410 Profiles


#  We want to make sure that lakes were sampled in both the spring post stratification
#  and later in the summer. This ensures that we can calculate VHOD
filter3.1.MI = filter2.MI %>%
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
filter3.MI = filter2.MI %>%
  semi_join(filter3.1.MI, by = "Location_Year") %>% 
  filter(DOY >= 121 &
           DOY <= 258) #Removes profiles where it may have been sampled while ice is on or during turnover

# Summary #50494 obs
# filter3.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #108 Sites
# filter3.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter3.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #545 Lake-Years
# filter3.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,582 Profiles


filter4.1MI = filter3.MI %>%                #Profiles must have at least 3 reads in them
  group_by(Profile)%>%
  summarise(n_depths = n_distinct(Depth))%>%
  filter(n_depths >= 3) #Jane et al 2021

filter4.MI = filter3.MI %>%
  semi_join(filter4.1MI, by = "Profile")

# #  Summary #50474 obs
# filter4.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #107 Lakes
# filter4.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter4.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #542 Lake-Years
# filter4.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,564 Profiles

# Calculate the top and bottom of the metalimnion and remove profiles where it did not work
filter5.MI = filter4.MI %>%
  group_by(Profile)%>%
  mutate(Meta = list(meta.depths(wtr = Temperature, depths = Depth, slope = 0.1, seasonal = F)))%>%
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(Depth))%>%
  drop_na(top.hypo)%>%  #Jane et al 2021
  filter(top.hypo != "0") 

# #Summary #49388 obs
# filter5.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #107 Lakes
# filter5.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter5.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #542 Lake-Years
# filter5.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,458 Profiles

#Make a ratio across all lakes of the top of the hypolimnion to the max depth
filter6.1MI = filter5.MI %>%
  group_by(Location_Year)%>%
  filter(DOY == max(DOY))%>%
  distinct(DOY, .keep_all = T)%>%
  mutate(ratio = top.hypo/Max_Depth)%>%
  filter(ratio < 1)    #Remove profiles where the ratio is greater than one (Also would mean that the meta depths function didn't work)
#Extract the median of these ratios
ratio = skim(filter6.1MI$ratio)%>%
  pull(numeric.p50)

filter6.MI = filter5.MI %>%
  semi_join(filter6.1MI, by = "Location_Year")

# #Summary #47740 obs
# filter6.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #104 Lakes
# filter6.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter6.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #468 Lake-Years
# filter6.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,341 Profiles

#See that each lake has a profile depth in the median hypolimnion
filter7.MI = filter6.MI %>%
  group_by(Profile)%>%
  mutate(Max_Profile = max(Depth))%>%
  filter(Max_Profile > Max_Depth * (ratio)) 

# #Summary #46604 obs
# filter7.MI %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #100 Lakes
# filter7.MI %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# filter7.MI %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #501 Lake-Years
# filter7.MI %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,176 Profiles


# Check start and end date again after losing profiles
filter8.1MI = filter7.MI %>%
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
MI.Filtered.Obs = filter7.MI %>%
  semi_join(filter8.1MI, by = "Location_Year")%>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  ungroup()%>%
  mutate(Depth = floor(Depth))%>%
  group_by(ID, MonitoringLocationIdentifier,Year,DOY,Max_Depth,Latitude,
           Longitude, Date, Depth)%>%
  summarise(DO = median(DO),
            Temperature = median(Temperature))%>%
  ungroup()


# #Summary #46054 obs
# MI.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n = n()) #98 Lakes
# MI.Filtered.Obs %>%
#   group_by(Year)%>%
#   summarise(n = n()) #22 Years
# MI.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n = n()) #483 Lake-Years
# MI.Filtered.Obs %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n = n()) #3,133 Profiles


#  Clean
rm(filter1.MI, filter2.MI, filter3.MI,filter4.MI,filter5.MI, filter6.MI,
   filter7.MI, filter2.1MI, filter3.1.MI, filter4.1MI,
   filter6.1MI, filter8.1MI, ratio)
gc()
###### Interpolate the Data Frame ===============================================
#  In this section linear interpolation is preformed on DO and Temperature 
#  observations. Then, the interpolation are rejoined to the metadata before
#  being sent off to calculate AHOD and VHOD
#  Run time of about 2 minutes

### The next step is to interpolate data at 1 m increments
Depth.df = tibble(Depth = seq(0,40,0.5)) #This creates a data frame to make sure we have data for the bottom of larger lakes
#  First DO
#  Pivot Wide
Wide.MI.DO.Obs = MI.Filtered.Obs %>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  select(ID, Depth, DO) %>%
  pivot_wider(names_from = ID, values_from = DO, values_fn = mean)%>%
  right_join(Depth.df)%>%#this used to be full_join
  arrange(Depth)#Heidi added this

#  Interpolate
Wide.Inter.MI.DO = na.approx(Wide.MI.DO.Obs, x = Wide.MI.DO.Obs$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.MI.DO = Wide.Inter.MI.DO %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  mutate(Max_Depth = as.numeric(Max_Depth))%>% #added this. Max_Depth was character otherwise
  filter(Depth <= Max_Depth)



### Next is Temperature
#  Pivot Wide
Wide.MI.Temp.Obs = MI.Filtered.Obs %>%
  mutate(ID = paste(MonitoringLocationIdentifier, Year, DOY, Max_Depth, sep = "/"))%>%
  select(ID, Depth, Temperature) %>%
  pivot_wider(names_from = ID, values_from = Temperature, values_fn = mean)%>%
  right_join(Depth.df)%>% #Heidi added-was full_join before
  arrange(Depth)#put in order before interpolating

#  Interpolate
Wide.Inter.MI.Temp = na.approx(Wide.MI.Temp.Obs, x = Wide.MI.Temp.Obs$Depth)%>% #interpolates data - 75 was chosen to make sure every lake was fully interpolated
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()

#  Pivot Long
Long.Inter.MI.Temp = Wide.Inter.MI.Temp %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  mutate(Max_Depth = as.numeric(Max_Depth))%>% #added this. Max_Depth was character otherwise
  filter(Depth <= Max_Depth)

### Join our results together
MI.joined.inter = inner_join(Long.Inter.MI.Temp, Long.Inter.MI.DO, by=c('Depth','ID',
    'MonitoringLocationIdentifier','Year','DOY','Max_Depth'))

#  We need to get the Lat and Lon
MI.Depth.Specific = MI.Filtered.Obs %>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(c(MonitoringLocationIdentifier, Latitude, Longitude))%>%
  inner_join(MI.joined.inter)

#  Summary #54100 obs
# MI.Depth.Specific %>%
#   group_by(MonitoringLocationIdentifier)%>%
#   summarise(n=n()) #98 Lakes
# MI.Depth.Specific %>%
#   group_by(Year)%>%
#   summarise(n=n()) #22 Years
# MI.Depth.Specific %>%
#   group_by(MonitoringLocationIdentifier, Year)%>%
#   summarise(n=n()) #483 Lake-Years
# MI.Depth.Specific %>%
#   group_by(MonitoringLocationIdentifier, Year, DOY)%>%
#   summarise(n=n()) #3,133 Profiles


#Clean up 
rm(MI.joined.inter,Long.Inter.MI.Temp,Wide.Inter.MI.Temp,Depth.df,
   Wide.MI.Temp.Obs,Long.Inter.MI.DO,Wide.Inter.MI.DO,Wide.MI.DO.Obs)
gc()


###  Next we will thermofilter the data set

MI.DS.Thermo = MI.Depth.Specific %>%
  group_by(MonitoringLocationIdentifier, Year, DOY)%>%
  mutate(Meta = list(meta.depths(wtr = Temperature, depths = Depth, slope = 0.1, seasonal = F)))%>% #Following the methods of Jane et al, 2021
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(Depth))%>%
  filter(Depth<as.numeric(Max_Depth))

#see if how many lakes we have trend data for
MI.DS.Thermo$Year<-as.integer(MI.DS.Thermo$Year)
MI.DS.Thermo$MonitoringLocationIdentifier<-as.factor(MI.DS.Thermo$MonitoringLocationIdentifier)
df1<-MI.DS.Thermo%>%filter(DOY>196 & DOY<248)%>%
 group_by(Year, MonitoringLocationIdentifier)%>%
 summarise(min=min(DOY),max=max(DOY))%>%
 group_by(MonitoringLocationIdentifier)%>%
  summarise(min=min(Year),max=max(Year),range=max-min)%>%
 filter(range>9)
df1.1<-df1%>%select(MonitoringLocationIdentifier)
df2<-left_join(df1.1,MI.DS.Thermo, by="MonitoringLocationIdentifier")
#write file for UMD students to use
#MI.DS.Thermo<-ungroup(MI.DS.Thermo)
#write.csv(MI.DS.Thermo, "MIprofiles_0.5.csv")
