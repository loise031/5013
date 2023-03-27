#DO trends in the Comb_Lakes dataframe
#Requires previously running the Comb_Lakes_Script for the df
#Should have 946949 obs of 16 variables


#Calculate mean surface water and deep water depths for each lakes
#surface is defined as the epilimnion
#Deep water is defined as the hypolimnion, both present categorized in the layer column

# Grouping new df to still include relevant lake info
#Take the mean of each group of DO by layer
#Note, all of the other variables included here should be the same for each unique profiles,
#so even though the are included, they should not be impacting the sort
Daily_avg_comb <- Comb_Lakes %>%
  group_by(MonitoringLocationIdentifier, Latitude, Longitude, ID,
           Year, Layer, State, Max_Depth, top.meta, top.hypo) %>%
  summarise(DO_mean = mean(DO), Temp_mean = mean(Temperature))

#Add Layer at the end of the df
Daily_avg_comb <- Daily_avg_comb %>% select(-Layer, everything()) 

# Only include the epilimnion and hypolimnion layers
Daily_avg_comb <- Daily_avg_comb %>%
  filter(Layer == "Epilimnion" | Layer == "Hypolimnion")

#Averaging each profile per year: 

Annual_Comb <- Daily_avg_comb %>%
  group_by(Year, ID) %>%
  summarize(DO_mean_avg = mean(DO_mean), 
            Temp_mean_avg = mean(Temp_mean), 
            Latitude = first(Latitude),
            Longitude = first(Longitude),
            State = first(State),
            Max_Depth = first(Max_Depth),
            top.meta = first(top.meta),
            top.hypo = first(top.hypo),
            Layer = first(Layer),
            MonitoringLocationIdentifier = first(MonitoringLocationIdentifier)) %>%
  ungroup()

#Clean the name of the annual DO and Temperature columns

Annual_Comb <- Annual_Comb %>%
  rename(Annual_DO = DO_mean_avg) %>%
  rename(Annual_Temp = Temp_mean_avg)

#Now I need to get elevation data for both lakes

library(readr)
lake_information_rantala <- read_csv("lake_information_rantala.csv")
View(lake_information_rantala)

Lake_Info <- lake_information_rantala


Merged_Lake_Elev <- merge(Annual_Comb, select(Lake_Info, lake_lat_decdeg, lake_elevation_m), 
                     by.x = "Latitude", by.y = "lake_lat_decdeg", all.x = TRUE)

#Adding this column to the master df 
Annual_Comb <- Merged_Lake_Elev

# Rename the column
Annual_Comb <- Annual_Comb %>%
  rename(Elevation_m = lake_elevation_m)

#Seeing if there are any lakes that did not get assigned an elevation
table(is.na(Annual_Comb))

#6613 lakes did not receive an elevation, adding the average elevation for the respective state:
# MN: 370 m MI: 270 m  WI: 320 m per: https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_elevation

#Overwrites the elevation NAs with the average state elevation
Annual_Comb$Elevation_m[is.na(Annual_Comb$Elevation_m) & Annual_Comb$State == "MN"] <- 370
Annual_Comb$Elevation_m[is.na(Annual_Comb$Elevation_m) & Annual_Comb$State == "MI"] <- 270
Annual_Comb$Elevation_m[is.na(Annual_Comb$Elevation_m) & Annual_Comb$State == "WI"] <- 320

table(is.na(Annual_Comb$Elevation_m)) #All NAs substituted with average elevation

library(rMR) #Package used to calculate DO Saturation

#Adds at DO saturation column to the data frame
Annual_Comb$DO_saturation <- DO.saturation(DO = Annual_Comb$Annual_DO, 
                                           temp = Annual_Comb$Annual_Temp, 
                                           elev = Annual_Comb$Elevation_m)


###############################################################################
#############################-Begin-DO-Analysis-############################### 
###############################################################################

#Lots of problems here, A, I am not convinced I calculated DO sat correctly due to 1/3rd of 
#the values being > 1 and, B, I cannot get the TheilSen() function to work for the df... ugh

library(openair)

plot(Annual_Comb$Year, Annual_Comb$DO_saturation,
     ylim= c(0:1))
scatter.smooth(Annual_Comb$DO_saturation ~ Annual_Comb$Year,
               ylim= c(0:1))


?TheilSen()

Annual_Comb$Date_est <- as.Date(paste(Annual_Comb$Year, "08", "01", sep = "-"))

Annual_Comb$Date_est <- as.Date(Annual_Comb$Date_est)

TheilSen(as.Date(Annual_Comb$Date_est), pollutant = Annual_Comb$Annual_DO, deseason = FALSE, xlab = "Year",
         ylab = "DO Concentration (mg/l)")




