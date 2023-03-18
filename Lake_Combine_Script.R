#Script to merge the three state's of lake data together

library(tidyverse)

#Upload the files from wd:

library(readr)
MIprofiles <- read_csv("5013_Correct/MIprofiles.csv")
View(MIprofiles)

library(readr)
WIprofiles <- read_csv("5013_Correct/WIprofiles.csv")
View(WIprofiles)

library(readr)
MNprofiles <- read_csv("5013_Correct/MNprofiles.csv")
View(MNprofiles)

#Combining the three lakes into an All_Lakes dataframe

All_Lakes <- rbind(MNprofiles, MIprofiles, WIprofiles)

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




