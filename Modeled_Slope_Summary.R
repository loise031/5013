#Running Sen's Slope on the Filtered_Read_Link df, requires previously running:
# Comb_Lakes_Script -> Individual_Lake_Analysis, and the Merging_Pre_Regression, all .R files



#Now I want to run Sen's slope on all relevant parameters and integrate it with Individual_Comb_Sens



###############################################################################
###########################-Individual-Trend-Analysis-#########################
###############################################################################
library(openair)

#Adding a data column to the filtered Read data

Filtered_Read_Link$date <- paste(Filtered_Read_Link$year, "-08-01", sep="")

#Changing Format for the openair package to Read
library(lubridate) 
Filtered_Read_Link$date <- lubridate::ymd_hms(paste(Filtered_Read_Link$date, "00:00:00"))



#Temp Trends
Sens_Mod_Temp <- TheilSen(Filtered_Read_Link, pollutant = "peak_temp", 
                                   type = "lake_name", deseason = FALSE,
                                   xlab = "year", ylab = "Modeled Temp")
Sens_Mod_Temp$data[[2]]
head(Sens_Mod_Temp$data[[1]])


Sens_Mod_Temp_df <- data.frame(Sens_Mod_Temp$data[[2]])


#Strat onset Trends
Sens_Mod_Strat_Onset <- TheilSen(Filtered_Read_Link, pollutant = "stratification_onset_yday", 
                          type = "lake_name", deseason = FALSE,
                          xlab = "year", ylab = "Modeled Strat Onset")
Sens_Mod_Strat_Onset$data[[2]]
head(Sens_Mod_Strat_Onset$data[[1]])


Sens_Mod_Strat_Onset_df <- data.frame(Sens_Mod_Strat_Onset$data[[2]])


#Strat Duration Trends
Sens_Mod_Strat_Dur <- TheilSen(Filtered_Read_Link, pollutant = "stratification_duration", 
                                 type = "lake_name", deseason = FALSE,
                                 xlab = "year", ylab = "Modeled Strat Duration")
Sens_Mod_Strat_Dur$data[[2]]
head(Sens_Mod_Strat_Dur$data[[1]])


Sens_Mod_Strat_Dur_df <- data.frame(Sens_Mod_Strat_Dur$data[[2]])


#Schmidt Stability Daily Annual Sum Trends
Sens_Mod_Strat_Schmidt <- TheilSen(Filtered_Read_Link, pollutant = "stratification_duration", 
                               type = "lake_name", deseason = FALSE,
                               xlab = "year", ylab = "Modeled Schmidt Stability Daily Annual Sum")
Sens_Mod_Strat_Schmidt$data[[2]]
head(Sens_Mod_Strat_Schmidt$data[[1]])


Sens_Mod_Strat_Schmidt_df <- data.frame(Sens_Mod_Strat_Schmidt$data[[2]])


#Strat Duration Trends
Sens_Mod_Wint_Dur <- TheilSen(Filtered_Read_Link, pollutant = "winter_dur_0_4", 
                                   type = "lake_name", deseason = FALSE,
                                   xlab = "year", ylab = "Modeled Winter Duration")
Sens_Mod_Wint_Dur$data[[2]]
head(Sens_Mod_Wint_Dur$data[[1]])


Sens_Mod_Wint_Dur_df <- data.frame(Sens_Mod_Wint_Dur$data[[2]])


#Merging all of this data into one data frame
#################################################################

#Removing the odd rows (Duplicates with NA's)

df_list_mod <- list(Sens_Mod_Temp_df, Sens_Mod_Strat_Onset_df,
                    Sens_Mod_Strat_Dur_df, Sens_Mod_Strat_Schmidt_df,
                    Sens_Mod_Wint_Dur_df) # List of data frames

for (i in 1:length(df_list_mod)) {
  df_list_mod[[i]] <- df_list_mod[[i]][seq(1, nrow(df_list_mod[[i]]), by=2),] # Remove even rows
  rownames(df_list_mod[[i]]) <- 1:nrow(df_list_mod[[i]]) # Renumber rows
}

#I visually confirmed this worked how I wanted it to.


Mod_Sens <- do.call(rbind, df_list_mod) #Combine each df together

source_analysis_mod <- c("Mod_Temp", "Mod_Strat_Onset",
                     "Mod_Strat_Dur", "Mod_Strat_Schmidt", "Mod_Wint_Dur") #Creates column relevant to which analysis type was done
Mod_Sens$Analysis <- rep(source_analysis_mod, sapply(df_list_mod, nrow)) #Inserts them via their source df

#Reordering the column names
Mod_Sens <- Mod_Sens %>%
  select(Analysis, lake_name, p.stars, p, slope,intercept, everything())

sum(Mod_Sens$p < 0.05) #There are 21 significant trends

Interim_Data_ID_Mod <- Filtered_Read_Link[, c("lake_nhdid", "lake_name", "state", "group_id", "centroid_lon", "centroid_lat")]

Interim_Data_ID_Mod <- distinct(Interim_Data_ID_Mod) #remove duplicates

merged_trends_2 <- merge(Mod_Sens, Interim_Data_ID_Mod, by = "lake_name")

Mod_Sens <- merged_trends_2

#Now I have to edit Mod_Sens to be exactly the same, structurally 

#Adding a MonitoringLocationIdentifier column via a lookup table via nhdid

# create the lookup table with MonitoringLocationIdentifier and Max_Depth columns
lookup_table_ID <- unique(Comb_Lakes_Link[, c("MonitoringLocationIdentifier", "lake_nhdid")])

# create a new column in a different data frame based on the lookup table
Mod_Sens$MonitoringLocationIdentifier <- lookup_table_ID[match(Mod_Sens$lake_nhdid, 
                                                           lookup_table_ID$lake_nhdid), 
                                                     "MonitoringLocationIdentifier"]

#Renaming columns for merging
colnames(Mod_Sens)[colnames(Mod_Sens) == "centroid_lat"] <- "Latitude"

colnames(Mod_Sens)[colnames(Mod_Sens) == "centroid_lon"] <- "Longitude"

colnames(Mod_Sens)[colnames(Mod_Sens) == "state"] <- "State"

# add a new column indicating the origin of each row
Individual_Comb_Sens$Origin <- "Measured"
Mod_Sens$Origin <- "Modeled"

Master_Slopes <- bind_rows(Individual_Comb_Sens, Mod_Sens)

#Reordering the column names
Master_Slopes <- Master_Slopes %>%
  select(MonitoringLocationIdentifier, everything())

#This is an almost completed data set to model.










