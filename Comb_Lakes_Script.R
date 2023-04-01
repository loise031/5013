library(readr)
MN_Filtered_Obs <- read_csv("MN.Filtered.Obs.csv")
View(MN_Filtered_Obs)

library(readr)
WI_Filtered_Obs <- read_csv("WI.Filtered.Obs.csv")
View(WI_Filtered_Obs)

library(readr)
MI_Filtered_Obs <- read_csv("MI.Filtered.Obs.csv")
View(MI_Filtered_Obs)

library(readr)
WIprofiles_0_5 <- read_csv("WIprofiles_0.5.csv")
View(WIprofiles_0_5)

library(readr)
MIprofiles_0_5 <- read_csv("MIprofiles_0.5.csv")
View(MIprofiles_0_5)

library(readr)
MNprofiles_0_5 <- read_csv("MNprofiles_0.5.csv")
View(MNprofiles_0_5)


MNprofiles_0_5 <- MNprofiles_0_5 %>% select(-DOW, everything()) #Adds DOW to the end of the data frame

MIprofiles_0_5$DOW <- NA   #adds a DOW column
WIprofiles_0_5$DOW <- NA

Added_MN <- MNprofiles_0_5 %>%
  mutate("State" = "MN")

Added_WI <- WIprofiles_0_5 %>%
  mutate("State" = "WI")

Added_MI <- MIprofiles_0_5 %>%
  mutate("State" = "MI")


Comb_Lakes <- rbind(Added_MI, Added_MN, Added_WI)

#Specifies where within the water column the depth measurement is taken
Comb_Lakes$Layer <-  ifelse(Comb_Lakes$Depth < Comb_Lakes$top.meta, "Epilimnion",
                                ifelse(Comb_Lakes$Depth < Comb_Lakes$top.hypo, 
                                       "Hypolimnion", "Metalimnion"))


#Next step is to filter for only August values

#Creates a POSIX date column in the df
Comb_Lakes$Date <- as.POSIXct(paste(Comb_Lakes$Year, Comb_Lakes$DOY, sep = "-"), format = "%Y-%j")

library(lubridate)        #Allows me to extract a month out of my POSIX value 

start_date <- ymd("2022-07-15") #Beginning of compariable measurements
end_date <- ymd("2022-08-31") #End of compariable measurements
Comb_Lakes_Aug <- Comb_Lakes[month(Comb_Lakes$Date) == month(start_date) & day(Comb_Lakes$Date) >= day(start_date) | 
                                  month(Comb_Lakes$Date) == month(end_date) & day(Comb_Lakes$Date) <= day(end_date), ]


#Now follow methods from Jane et al

#Removing all temp and DO values outside of 0-40

summary(Comb_Lakes_Aug$DO) #This column has data outside of both bounds

summary(Comb_Lakes$Temperature) #This column has data below 0º C

#Subset for temp and DO to be within 0 and 40

Comb_Lakes_Aug <- subset(Comb_Lakes_Aug, Temperature >= 0 & Temperature <= 40)
summary(Comb_Lakes_Aug$Temperature) #Confirming

Comb_Lakes_Aug <- subset(Comb_Lakes_Aug, DO >= 0 & DO <= 40)
summary(Comb_Lakes_Aug$DO) #Confirming

#Filtering so that locations are only included if they have a unique value for a minimum of 15 years:
#Code is written so the years do not have to be consecutive 

Comb_Lakes_Aug_Counts <- Comb_Lakes_Aug %>%
  group_by(MonitoringLocationIdentifier, Year) %>% 
  summarize(n_temps = sum(Temperature > 0)) %>% #calculates the number of temperature measurements that are greater than 0 for each combination of location and year
  ungroup()


Comb_Lakes_Aug_Counts <- Comb_Lakes_Aug_Counts %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarize(n_years = sum(n_temps > 0)) %>% #calculates the number of years with at least one temperature measurement for each location
  filter(n_years >= 15) %>% #Only include locations with >= 15 years
  select(MonitoringLocationIdentifier) # Resulting df only lists locations that have 15 years of data

#Only includes lakes with 15 years of data as determined by previous sub setting
Comb_Lakes_AugY <- subset(Comb_Lakes_Aug,
                         MonitoringLocationIdentifier %in% Comb_Lakes_Aug_Counts$MonitoringLocationIdentifier)

#Rename for the filtered comb lakes
Comb_Lakes <- Comb_Lakes_AugY


#Now I need to add elevation data:

library(readr)
lake_information_rantala <- read_csv("lake_information_rantala.csv")
View(lake_information_rantala)

Lake_Info <- lake_information_rantala


Comb_Lakes <- merge(Comb_Lakes, select(Lake_Info, lake_lat_decdeg, lake_elevation_m), 
                          by.x = "Latitude", by.y = "lake_lat_decdeg", all.x = TRUE)

# Rename the column
Comb_Lakes <- Comb_Lakes %>%
  rename(Elevation_m = lake_elevation_m)

#Seeing if there are any lakes that did not get assigned an elevation
table(is.na(Comb_Lakes$Elevation_m))

#261880 rows did not receive an elevation, adding the average elevation for the respective state:
# MN: 370 m MI: 270 m  WI: 320 m per: https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_elevation

#Overwrites the elevation NAs with the average state elevation
Comb_Lakes$Elevation_m[is.na(Comb_Lakes$Elevation_m) & Comb_Lakes$State == "MN"] <- 370
Comb_Lakes$Elevation_m[is.na(Comb_Lakes$Elevation_m) & Comb_Lakes$State == "MI"] <- 270
Comb_Lakes$Elevation_m[is.na(Comb_Lakes$Elevation_m) & Comb_Lakes$State == "WI"] <- 320

table(is.na(Comb_Lakes$Elevation_m))

#Calculate DO saturation for each value




library(rMR) #Package used to calculate DO Saturation

#Adds at DO saturation column to the data frame
Comb_Lakes$DO_saturation <- DO.saturation(DO = Comb_Lakes$DO, 
                                           temp = Comb_Lakes$Temperature, 
                                           elev = Comb_Lakes$Elevation_m)

summary(Comb_Lakes$DO_saturation)
#Pretty wild saturation values here....

#Analyzing DO calcualtions:

sum(Comb_Lakes$DO_saturation > 1)


#Final output for Comb_Lakes, 147653 obs of 19 variables

#write.csv(Comb_Lakes, "Comb_Lakes.csv")





