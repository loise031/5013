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
           Year, Layer, State, Max_Depth, top.meta, top.hypo, Elevation_m) %>%
  summarise(DO_con_mean = mean(DO), Temp_mean = mean(Temperature), 
            DO_sat_mean = mean(DO_saturation))

#Add Layer at the end of the df
Daily_avg_comb <- Daily_avg_comb %>% select(-Layer, everything()) 

# Only include the epilimnion and hypolimnion layers
Daily_avg_comb <- Daily_avg_comb %>%
  filter(Layer == "Epilimnion" | Layer == "Hypolimnion")

#Averaging each profile per year: 

Annual_Comb <- Daily_avg_comb %>%
  group_by(Year, MonitoringLocationIdentifier) %>%
  summarize(DO_con_mean_avg = mean(DO_con_mean),
            DO_sat_mean_avg = mean(DO_sat_mean),
            Temp_mean_avg = mean(Temp_mean), 
            Latitude = first(Latitude),
            Longitude = first(Longitude),
            State = first(State),
            Max_Depth = first(Max_Depth),
            top.meta = first(top.meta),
            top.hypo = first(top.hypo),
            Elevation_m = first(Elevation_m),
            Layer = first(Layer),
            ID = first(ID)) %>%
  ungroup()

#Clean the name of the annual DO and Temperature columns

Annual_Comb <- Annual_Comb %>%
  rename(Annual_DO_Con = DO_con_mean_avg) %>%
  rename(Annual_DO_Sat = DO_sat_mean_avg) %>%
  rename(Annual_Temp = Temp_mean_avg)


###### Final Annual_Comb df should have 4431 obs of 14 variables
  ####  with each MonitoringLocationIdentifier occurring >=1 time per year

###############################################################################
#############################-Begin-DO-Analysis-############################### 
###############################################################################

#Lots of problems here, A, I am not convinced I calculated DO sat correctly due to 1/3rd of 
#the values being > 1 and, B, I cannot get the TheilSen() function to work for the df... ugh
install.packages("openair")
library(openair)

#Visualization 

plot(Annual_Comb$Annual_DO_Sat)

summary(Annual_Comb$Annual_DO_Sat)

## ROB CODE ##############

Annual_Comb$date <- as.Date(paste(Annual_Comb$Year, "08", "01", sep = "-"))
Annual_Comb$date <- as.Date(Annual_Comb$date, format = "%d/%m/%Y")

##now need to make one more change to the date format
library(lubridate) 
Annual_Comb$date <- lubridate::ymd_hms(paste(Annual_Comb$date, "00:00:00"))

##do Sen's slope
test_ts <- TheilSen(Annual_Comb, pollutant = "Annual_DO_Con", deseason = FALSE)

##see results
test_ts$data[[2]]
head(test_ts$data[[1]])


test_ts <- TheilSen(Annual_Comb, pollutant = "Annual_DO_Con", deseason = FALSE, xlab = "Year",
         ylab = "DO Concentration (mg/l)")

test_ts$data[[2]]
head(test_ts$data[[1]])


TheilSen(Annual_Comb, pollutant = Annual_Comb$Annual_DO_Con)

#Calculating Sen's Slope for Temperature

Annual_Comb$date <- as.Date(paste(Annual_Comb$Year, "08", "01", sep = "-"))
Annual_Comb$date <- as.Date(Annual_Comb$date, format = "%d/%m/%Y")

##now need to make one more change to the date format
library(lubridate) 
Annual_Comb$date <- lubridate::ymd_hms(paste(Annual_Comb$date, "00:00:00"))

##do Sen's slope
test_ts_temp <- TheilSen(Annual_Comb, pollutant = "Annual_Temp", deseason = FALSE)

##see results
test_ts_temp$data[[2]]
head(test_ts_temp$data[[1]])


#Seems like there's almost no trend, what if I separate layers:
# Create and Epilimnion value df
Annual_Comb_Epi <- subset(Annual_Comb, Layer == "Epilimnion")

# Create a Hypolimnion value df
Annual_Comb_Hypo <- subset(Annual_Comb, Layer == "Hypolimnion")

#Epi DO Con Sens
Sens_Epi_DOcon <- TheilSen(Annual_Comb_Epi, pollutant = "Annual_DO_Con", deseason = FALSE)
Sens_Epi_DOcon$data[[2]]
head(Sens_Epi_DOcon$data[[1]])
#This has a slope of 0.0173 and an intercept of 6.500, with a *** p value

#Hypo DO Con Sens
Sens_Hypo_DOcon <- TheilSen(Annual_Comb_Hypo, pollutant = "Annual_DO_Con", deseason = FALSE)
Sens_Hypo_DOcon$data[[2]]
head(Sens_Hypo_DOcon$data[[1]])
#Slope of 0.01159 with an intercept of 6.220, but a p-value of 0.26

#Epi Temp Sens
Sens_Epi_Temp <- TheilSen(Annual_Comb_Epi, pollutant = "Annual_Temp", deseason = FALSE)
Sens_Epi_Temp$data[[2]]
head(Sens_Epi_Temp$data[[1]])
#Slope of 0.00348 with an intercept of 18.63, but a p-value of 0.160

#Hypo Temp Sens
Sens_Hypo_Temp <- TheilSen(Annual_Comb_Hypo, pollutant = "Annual_Temp", deseason = FALSE)
Sens_Hypo_Temp$data[[2]]
head(Sens_Hypo_Temp$data[[1]])
#Slope of -0.0131 with an intercept of 19.41, but a p value of 0.194

#Epi DO Sat Sens
Sens_Epi_DOsat <- TheilSen(Annual_Comb_Epi, pollutant = "Annual_DO_Sat", deseason = FALSE)
Sens_Epi_DOsat$data[[2]]
head(Sens_Epi_DOsat$data[[1]])
#This has a slope of 0.0173 and an intercept of 6.500, with a *** p value

#Hypo DO Sat Sens
Sens_Hypo_DOsat <- TheilSen(Annual_Comb_Hypo, pollutant = "Annual_DO_Sat", deseason = FALSE)
Sens_Hypo_DOsat$data[[2]]
head(Sens_Hypo_DOsat$data[[1]])
#Slope of 0.0011 with an intercept of 0.72, but a p-value of 0.3


## PETER CODE ########################################################
##making required "date" field in as.Date format
##openair package TheilSen function needs a "date" field in YYYY-mm-dd
Annual_Comb$date <- as.Date(paste(Annual_Comb$Year, "08", "01", sep = "-"))
Annual_Comb$date <- as.Date(Annual_Comb$date, format = "%d/%m/%Y")

##now need to make one more change to the date format
library(lubridate) 
Annual_Comb$date <- lubridate::ymd_hms(paste(Annual_Comb$date, "00:00:00"))

##do Sen's slope
test_ts <- TheilSen(Annual_Comb, pollutant = "Annual_DO", deseason = FALSE)

##see results
test_ts$data[[2]]
head(test_ts$data[[1]])
 
