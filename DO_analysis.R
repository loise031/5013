#DO trends in the Comb_Lakes dataframe
#Requires previously running the Comb_Lakes_Script for the df
#Should have 147653 obs of 19 variables


#Calculate mean surface water and deep water depths for each lakes
#surface is defined as the epilimnion
#Deep water is defined as the hypolimnion, both present categorized in the layer column
library(dplyr)
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

mean(Annual_Comb_Epi$Annual_Temp)
mean(Annual_Comb_Hypo$Annual_Temp)

###### Final Annual_Comb df should have 1424 obs of 14 variables
  ####  with each MonitoringLocationIdentifier occurring =<1 time per year
#Lots of problems here: I am not convinced I calculated DO sat correctly due to 1/3rd of 
#the values being > 1

  ####  with each MonitoringLocationIdentifier occurring >=1 time per year

###############################################################################
#############################-Begin-Analysis-##################################
###############################################################################

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
#p value of 0.628 slope of 0.0015, intercept of 5.40

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
#p value = 0, slope = 0.0186, intercept: 20.67503

##do Sen's slope
test_ts_DO_Sat <- TheilSen(Annual_Comb, pollutant = "Annual_DO_Sat", deseason = FALSE)

##see results
test_ts_DO_Sat$data[[2]]
head(test_ts_DO_Sat$data[[1]])
#p value = 0.260, slope = 0.000049, intercept: 0.656

###############################################################################
############################-Layer-Separation-#################################
###############################################################################

#Seems like there's almost no trend, what if I separate layers:
# Create and Epilimnion value df
Annual_Comb_Epi <- subset(Annual_Comb, Layer == "Epilimnion")

# Create a Hypolimnion value df
Annual_Comb_Hypo <- subset(Annual_Comb, Layer == "Hypolimnion")

#Epi DO Con Sens
Sens_Epi_DOcon <- TheilSen(Annual_Comb_Epi, pollutant = "Annual_DO_Con", deseason = FALSE)
Sens_Epi_DOcon$data[[2]]
head(Sens_Epi_DOcon$data[[1]])
#This has a slope of 0.0021 and an intercept of 5.40, with a p value of 0.604

#Hypo DO Con Sens
Sens_Hypo_DOcon <- TheilSen(Annual_Comb_Hypo, pollutant = "Annual_DO_Con", deseason = FALSE)
Sens_Hypo_DOcon$data[[2]]
head(Sens_Hypo_DOcon$data[[1]])
#Slope of 0.00366with an intercept of 4.7720, but a p-value of 0.841

#Epi Temp Sens
Sens_Epi_Temp <- TheilSen(Annual_Comb_Epi, pollutant = "Annual_Temp", deseason = FALSE)
Sens_Epi_Temp$data[[2]]
head(Sens_Epi_Temp$data[[1]])
#Slope of 0.020 with an intercept of 20.305, with a p-value of 0

#Hypo Temp Sens
Sens_Hypo_Temp <- TheilSen(Annual_Comb_Hypo, pollutant = "Annual_Temp", deseason = FALSE)
Sens_Hypo_Temp$data[[2]]
head(Sens_Hypo_Temp$data[[1]])
#Slope of -0.00321 with an intercept of 21.07, but a p value of 0.804

#Epi DO Sat Sens
Sens_Epi_DOsat <- TheilSen(Annual_Comb_Epi, pollutant = "Annual_DO_Sat", deseason = FALSE)
Sens_Epi_DOsat$data[[2]]
head(Sens_Epi_DOsat$data[[1]])
#This has a slope of 0.00055 and an intercept of 0.6500, with a 0.234 p value

#Hypo DO Sat Sens
Sens_Hypo_DOsat <- TheilSen(Annual_Comb_Hypo, pollutant = "Annual_DO_Sat", deseason = FALSE)
Sens_Hypo_DOsat$data[[2]]
head(Sens_Hypo_DOsat$data[[1]])
#Slope of 0.0009 with an intercept of 0.58, but a p-value of 0.715


################################################################################
##############################-Subsetting Years-################################
################################################################################

table(Annual_Comb$Year)
#Subsetting from 1974-Present (Analysis above) and 1990-Present (Analysis here) per Jane et al.

Annual_Comb_Epi_Sub <- subset(Annual_Comb_Epi, Year >= 1990) #Creates an df of all Epilimnion measurements post 1989
Annual_Comb_Hypo_Sub <- subset(Annual_Comb_Hypo, Year >= 1990) #Same for the Hypolimnion

#Post "89 Epilimnion DO conc:

Sens_Epi_Sub_DOcon <- TheilSen(Annual_Comb_Epi_Sub, pollutant = "Annual_DO_Con", deseason = FALSE)
Sens_Epi_Sub_DOcon$data[[2]]
head(Sens_Epi_Sub_DOcon$data[[1]])
# p-value: 0.734 Slope: -0.0107 Intercept: 5.538

#Post "89 Hypolimnion DO conc:

Sens_Hypo_Sub_DOcon <- TheilSen(Annual_Comb_Hypo_Sub, pollutant = "Annual_DO_Con", deseason = FALSE)
Sens_Hypo_Sub_DOcon$data[[2]]
head(Sens_Hypo_Sub_DOcon$data[[1]])
# p-value: 0.98 Slope: 0.00017 Intercept: 4.95

#Post "89 Epilimnion DO sat:

Sens_Epi_Sub_DOsat <- TheilSen(Annual_Comb_Epi_Sub, pollutant = "Annual_DO_Sat", deseason = FALSE)
Sens_Epi_Sub_DOsat$data[[2]]
head(Sens_Epi_Sub_DOsat$data[[1]])
# p-value: 0.80 Slope: 0.00017 Intercept: 0.6744

#Post "89 Hypolimnion DO sat:

Sens_Hypo_Sub_DOsat <- TheilSen(Annual_Comb_Hypo_Sub, pollutant = "Annual_DO_Sat", deseason = FALSE)
Sens_Hypo_Sub_DOsat$data[[2]]
head(Sens_Hypo_Sub_DOsat$data[[1]])
# p-value: 0.905 Slope: 0.00030 Intercept: 0.602

#Post "89 Hypolimnion Temp:

Sens_Epi_Sub_Temp <- TheilSen(Annual_Comb_Epi_Sub, pollutant = "Annual_Temp", deseason = FALSE)
Sens_Epi_Sub_Temp$data[[2]]
head(Sens_Epi_Sub_Temp$data[[1]])
# p-value: 0.0068 Slope: 0.0132 Intercept: 30.589

#Post "89 Hypolimnion Temp:

Sens_Hypo_Sub_Temp <- TheilSen(Annual_Comb_Hypo_Sub, pollutant = "Annual_Temp", deseason = FALSE)
Sens_Hypo_Sub_Temp$data[[2]]
head(Sens_Hypo_Sub_Temp$data[[1]])
# p-value:0.755 Slope: -0.0007 Intercept: 21.22

mean(Annual_Comb_Epi_Sub$Annual_Temp) #Mean: 21.03
mean(Annual_Comb_Hypo_Sub$Annual_Temp) #Mean: 20.81

###################################################################################
##Peter code for Sen's Slope ##
###################################################################################

##making required "date" field in as.Date format 
##(DONT RUN THIS BLOCK if you already ran the same code above in ROB CODE section)
##openair package TheilSen function needs a "date" field in YYYY-mm-dd
##Annual_Comb now will have 15 variables
Annual_Comb$date <- as.Date(paste(Annual_Comb$Year, "08", "01", sep = "-"))
Annual_Comb$date <- as.Date(Annual_Comb$date, format = "%d/%m/%Y")
##now need to make one more change to the date format for TheilSen to run without errors
library(lubridate) 
Annual_Comb$date <- lubridate::ymd_hms(paste(Annual_Comb$date, "00:00:00"))

##TEST SENS SLOPE (old code, don't need to use)
##do Sen's slope
##test_ts <- TheilSen(Annual_Comb, pollutant = "Annual_DO", deseason = FALSE)
##see results
##test_ts$data[[2]]
##head(test_ts$data[[1]])

##SEN'S SLOPE
 ##temp sens slope for each layer
  temp_sens_epihypo <- TheilSen(Annual_Comb, pollutant = "Annual_Temp", type = "Layer", deseason = FALSE, ylab = "Temperature (C)")
  ##results:
    view(temp_sens_epihypo)
    head(temp_sens_epihypo$data[[1]])
    temp_sens_epihypo$data[[2]]
    ## epi slope = 0.019753831 p=0.00000 (***), hypo slope = -0.003304753 p=0.80 ()
    
 ##DO concentration sens slope for each layer
  DOconc_sens_epihypo <- TheilSen(Annual_Comb, pollutant = "Annual_DO_Con", type = "Layer", deseason = FALSE, ylab = "Dissolved Oxygen (mg/L)")
  ##results:
  view(DOconc_sens_epihypo)
  head(DOconc_sens_epihypo$data[[1]])
  DOconc_sens_epihypo$data[[2]]
  ## epi slope = 0.002072882 p=0.60(), hypo slope = 0.004304802 p=0.86 ()
  ## setting filter from 40 mgL to 20 mgL removed 17 readings from the 40mgL filtered dataset, or 17 readings >20 and <40,
      ## which were affecting annual means, now there is no significant positive trend in the epi as there was in the unfiltered dataset
  
 ##DO saturation sens slope for each layer
  DOsat_sens_epihypo <- TheilSen(Annual_Comb, pollutant = "Annual_DO_Sat", type = "Layer", deseason = FALSE, ylab = "Dissolved Oxygen Percent Saturation (%)")
  ##results:
  view(DOsat_sens_epihypo)
  head(DOsat_sens_epihypo$data[[1]])
  DOsat_sens_epihypo$data[[2]]
  ## epi slope = 0.0005538740 p=0.23(), hypo slope = 0.0008482614 p=0.72()
  ## setting filter from 40 mgL to 20 mgL removed 17 readings from the 40mgL filtered dataset, or 17 readings >20 and <40,
  ##   which were affecting annual means, now there is no significant positive trend in the epi as there was in the unfiltered dataset
  
## Now breaking up by layer and by state after using code at top of morans i script to make
 ## Annual_Comb_Epi and Annual_Comb_Hypo dataframes.
 ## Mainly interested in finding if there are MN specific long term trends
  
 ## Epi temp sens separated by state
   temp_sens_epi_state <- TheilSen(Annual_Comb_Epi, pollutant = "Annual_Temp", type = "State", deseason = FALSE, ylab = "Epilimnion Temperature (C)")
   ## Results:
   view(temp_sens_epi_state)
   head(temp_sens_epi_state$data[[1]])
   temp_sens_epi_state$data[[2]]
   ##MI: p=0.92
   ##MN: p = 0.01 (*), slope = 0.010760215
   ##WI: p = 0.00000 (***), slope = 0.045095150
   ##According to this, MN and WI epi both significantly warming, WI at a ~4x faster rate
   
 ##Hypo temp sens separated by state
   temp_sens_hypo_state <- TheilSen(Annual_Comb_Hypo, pollutant = "Annual_Temp", type = "State", deseason = FALSE, ylab = "Hypolimnion Temperature (C)")
   ## Results:
   view(temp_sens_hypo_state)
   head(temp_sens_hypo_state$data[[1]])
   temp_sens_hypo_state$data[[2]]
   ##Not enough data for MI or WI, slope and p NaN
   ##For MN, p = 0.73 ()
   
 ##Epi DO conc sens separated by state
   doconc_sens_epi_state <- TheilSen(Annual_Comb_Epi, pollutant = "Annual_DO_Con", type = "State", deseason = FALSE, ylab = "Epilimnion [DO] (mg/L)")
   ## Results:
   view(doconc_sens_epi_state)
   head(doconc_sens_epi_state$data[[1]])
   doconc_sens_epi_state$data[[2]]
   ##MI: p=0.47
   ##MN: p = 0.007 (**), slope = 0.007589949
   ##WI: p = 0.00000 (***), slope = -0.031027407
   ##According to this, MN epi DO conc on significant slightly increasing trend,
   ##and WI epi DO conc on significant decreasing trend
   ##Interesting, because no significant epi DO conc trend in combined tristate dataset - WI and MN cancel out??
   
 ##Hypo DO conc sens separated by state
   doconc_sens_hypo_state <- TheilSen(Annual_Comb_Hypo, pollutant = "Annual_DO_Con", type = "State", deseason = FALSE, ylab = "Hypolimnion [DO] (mg/L)")
   ## Results:
   view(doconc_sens_hypo_state)
   head(doconc_sens_hypo_state$data[[1]])
   doconc_sens_hypo_state$data[[2]]
   ##Not enough data for MI or WI, slope and p NaN
   ##For MN, p = 0.47 ()
   
 ##Epi DO sat sens separated by state
   dosat_sens_epi_state <- TheilSen(Annual_Comb_Epi, pollutant = "Annual_DO_Sat", type = "State", deseason = FALSE, ylab = "Epilimnion DO % Saturation")
   ## Results:
   view(dosat_sens_epi_state)
   head(dosat_sens_epi_state$data[[1]])
   dosat_sens_epi_state$data[[2]]
   ##MI: p=0.58
   ##MN: p = 0.00000 (***), slope = 0.001218194
   ##WI: p = 0.00000 (***), slope = -0.003100565
   ##According to this, MN epi DO sat on significant slightly increasing trend,
   ##and WI epi DO sat on significant decreasing trend
   ##Interesting, because no significant epi DO sat trend in combined tristate dataset - WI and MN cancel out??
   
#################################
## PB 4/5 - Adding Sen's Slope separated by layer, but
## on the Comb_Lakes dataframe, not the Annual_Comb data frame as above in the "Peter
## code for Sen's Slope" section. Other than the dataframe being analyzed, the code is the same,
## and all outputs will have "_cl" added to them

   ##temp sens slope for each layer
   temp_sens_epihypo_cl <- TheilSen(Comb_Lakes, pollutant = "Temperature", type = "Layer", deseason = FALSE, ylab = "Temperature (C)")
   ##results:
   view(temp_sens_epihypo_cl)
   head(temp_sens_epihypo_cl$data[[1]])
   temp_sens_epihypo_cl$data[[2]]
   ##epi: slope = 0.03482696, p = 0.003 (**)
   ##hypo: slope = 0.02519415, p = 0.003 (**)
   
   ##do_con sens slope for each layer
   docon_sens_epihypo_cl <- TheilSen(Comb_Lakes, pollutant = "DO_Con", type = "Layer", deseason = FALSE, ylab = "DO Concentration (mg/L)")
   ##results:
   view(docon_sens_epihypo_cl)
   head(docon_sens_epihypo_cl$data[[1]])
   docon_sens_epihypo_cl$data[[2]]
   ##epi: slope = 0.008, p = 0.01 (*)
   ##hypo: insignificant trend (p = 0.30)
   
   ##do_sat sens slope for each layer
   dosat_sens_epihypo_cl <- TheilSen(Comb_Lakes, pollutant = "DO_Sat", type = "Layer", deseason = FALSE, ylab = "DO % Saturation")
   ##results:
   view(dosat_sens_epihypo_cl)
   head(dosat_sens_epihypo_cl$data[[1]])
   dosat_sens_epihypo_cl$data[[2]]
   ##epi: slope = 0.0017, p = 0.00000 (***) 
   ##hypo: insignificant trend (p = 0.57)
   
   ##so, in annual comb, only significant trend was epi temp (+)
   ##whereas in comb_lakes, significant trends are epi and hypo temp (both +), and epi docon and dosat (+)
   ##on the plots, it looks like the analysis on the comb_lakes (which is not annually averaged) is still 
   ##calculating and plotting the annual average for each year and then calculating the slope as the median
   ##of slopes between all of the points on the plot. Confirm if the function bins by year and then calculates trend. 
   ##however, I dont think this is taking the annual average of each layer's average in each lake, I think this is 
   ##just averaging all epi, meta, and hypo observations by year and then calculating trend between years.
   ##so, I think the annual_comb analysis is what we are interested in, as that takes a dataframe with each lake's average 
   ##epi and average hypo for each year for each parameter, and then the function is looking for the trend in the 
   ##annual averages of all lake epi averages for each year and annual averages of all hypo averages for each year
   
   
   