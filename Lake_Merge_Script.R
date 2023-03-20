#Script to merge the three state's of lake data together

library(tidyverse)

#Upload the files from wd:

library(readr)
MIprofiles <- read_csv("MIprofiles.csv")
View(MIprofiles)

library(readr)
WIprofiles <- read_csv("WIprofiles.csv")
View(WIprofiles)

library(readr)
MNprofiles <- read_csv("MNprofiles.csv")
View(MNprofiles)


#Turns out the columns are not in the correct order and MN has an extra column in DOW

#removeding DOW

Cleaned_MN <- MNprofiles[, -which(names(MNprofiles) == "DOW")]
Cleaned_MN
MIprofiles
WIprofiles #Monitoring Location is in the wrong position in the df

OrderedWI <- WIprofiles[, c("...1", "MonitoringLocationIdentifier", "Latitude",
                            "Longitude", "Depth", "ID", "Year", "DOY", "Max_Depth",
                            "Temperature", "DO", "top.meta", "top.hypo")]

OrderedWI

#Ready to bind

All_Lakes <- rbind(Cleaned_MN, MIprofiles, OrderedWI)

All_Lakes

#I am realizing having state data would be helpful, I am going to add a column to each data frame to label the state
#This solution is not reproducable, but it'll work for three times

Cleaned_Added_MN <- Cleaned_MN %>%
  mutate("State" = "MN")

Cleaned_Added_WI <- OrderedWI %>%
  mutate("State" = "WI")

Cleaned_Added_MI <- MIprofiles %>%
  mutate("State" = "Mi")

#Now creating all lakes:

All_Lakes <- rbind(Cleaned_Added_MN, Cleaned_Added_WI, Cleaned_Added_MI)

#Create a date column from year and DOY columns and add it to my dataframe

All_Lakes$Date <- as.POSIXct(paste(All_Lakes$Year, All_Lakes$DOY, sep = "-"), format = "%Y-%j")

#Remove any inputs which contain NA's in the top.hypo columns:
#!!!!!!!!    Be sure to undo this if we want to include lakes with NAs here

All_Lakes_NoNA <- All_Lakes[!is.na(All_Lakes$top.hypo), ]

#Creating a layer filter which writes which layer the measurement is in
All_Lakes_NoNA$Layer <-  ifelse(All_Lakes_NoNA$Depth < All_Lakes_NoNA$top.meta, "Epilimnion",
                                ifelse(All_Lakes_NoNA$Depth < All_Lakes_NoNA$top.hypo, 
                                       "Hypolimnion", "Metalimnion"))


#Next step is to filter for only August values

library(lubridate)        #Allows me to extract a month out of my POSIX value 

All_Lakes_Aug <- All_Lakes_NoNA %>%
  filter(month(Date) == 8)

#Now follow methods from Jane et al

#Removing all temp and DO values outside of 0-40

summary(All_Lakes_Aug$DO) #This column has data outside of both bounds

summary(All_Lakes$Temperature) #This column has data below 0º C

#Subset for temp and DO to be within 0 and 40

All_Lakes_Aug <- subset(All_Lakes_Aug, Temperature >= 0 & Temperature <= 40)
summary(All_Lakes_Aug$Temperature) #Confirming

All_Lakes_Aug <- subset(All_Lakes_Aug, DO >= 0 & DO <= 40)
summary(All_Lakes_Aug$DO) #Confirming

#Filtering so that locations are only included if they have a unique value for a minimum of 15 years:
#Code is written so the years do not have to be consecutive 

All_Lakes_Aug_Counts <- All_Lakes_Aug %>%
  group_by(MonitoringLocationIdentifier, Year) %>% 
  summarize(n_temps = sum(Temperature > 0)) %>% #calculates the number of temperature measurements that are greater than 0 for each combination of location and year
  ungroup()


All_Lakes_Aug_Counts <- All_Lakes_Aug_Counts %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarize(n_years = sum(n_temps > 0)) %>% #calculates the number of years with at least one temperature measurement for each location
  filter(n_years >= 15) %>% #Only include locations with >= 15 years
  select(MonitoringLocationIdentifier) # Resulting df only lists locations that have 15 years of data


All_Lakes_AugY <- subset(All_Lakes_Aug,
                         MonitoringLocationIdentifier %in% All_Lakes_Aug_Counts$MonitoringLocationIdentifier)

#The above code subsets All_Lakes_Aug by only locations that are also in the All_Lakes_Aug_Counts list 

table(All_Lakes_AugY$State)
#Remaining Locations:
#Mi    MN    WI 
#2065 30051 10651 












