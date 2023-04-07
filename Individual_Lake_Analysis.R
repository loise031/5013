#Script for measuring trends within individual lakes.

#This df is a result of Comb_Lakes_Script and DO_analysis:
Comb_Lakes
#Should have 147636 obs. of 19 variables

#Importing the csv needed to add lake names to the Comb_Lakes df

library(readr)
lake_link_rantala <- read_csv("lake_link_rantala.csv")
View(lake_link_rantala)


# Get the unique values of lake_lat_decdeg from the lake_link_rantala data frame
unique_latitudes_comb <- unique(lake_link_rantala$lake_lat_decdeg)

# Extract the the lake name along with the latitutde
relevant_rows_comb <- lake_link_rantala[lake_link_rantala$lake_lat_decdeg %in% unique_latitudes_comb, 
                                   c("lake_lat_decdeg", "lake_namelagos")]

# Use match() to create an index of the matching rows
comb_index <- match(Comb_Lakes$Latitude, relevant_rows_comb$lake_lat_decdeg)

# Merge the data frames using the index
merged_comb_lakes_test <- cbind(Comb_Lakes, relevant_rows_comb[comb_index, "lake_namelagos"])

# Rename the merged column to "Lagos_Name"
colnames(merged_comb_lakes_test)[colnames(merged_comb_lakes_test) == "lake_namelagos"] <- "Lagos_Name"

#confirming if the merge worked
summary(is.na(merged_comb_lakes_test$Lagos_Name))
#There are still 40664 rows that do not have a lake name. 

###############################################################################
### Attempting to insert "Unknown" lake names for all of the NAs
###############################################################################


#The following code is chatGPT or helped from chatGPT generated code
###############################################################################
# Create a vector to store the unique identifiers for each MonitoringLocationIdentifier
#unique_ids <- unique(merged_comb_lakes_test$MonitoringLocationIdentifier)
#id_map <- setNames(1:length(unique_ids), unique_ids)

# Define a function to generate the new Lagos_Name values
#gen_new_name <- function(name, id, state) {
 # if (is.na(name)) {
    # Generate a new name based on the given format
 #   unique_id <- id_map[id]
  #  new_name <- paste0("Unidentified (", state, ") ", unique_id)
#  } else {
 #   new_name <- name
 # }
#  return(new_name)
#}

# Apply the gen_new_name function to the Lagos_Name column
#merged_comb_lakes_test$Lagos_Name <- mapply(gen_new_name, 
                                           # merged_comb_lakes_test$Lagos_Name, 
                                           # merged_comb_lakes_test$MonitoringLocationIdentifier, 
                                           # merged_comb_lakes_test$State)

# Print the updated data frame
#print(merged_comb_lakes_test)
###############################################################################

#Any NAs?
summary(is.na(merged_comb_lakes_test$Lagos_Name))
#None, now seeing how well the Unique Names Generated

sort(unique(merged_comb_lakes_test$Lagos_Name))

length(unique(merged_comb_lakes_test$MonitoringLocationIdentifier))
#57 Lakes in this dataset
length(unique(merged_comb_lakes_test$Lagos_Name))
#Only 53 Lakes here, there' an error in the code...

#There are 10 unidentified lakes when merging by latitude

#chatGPT Troubleshooting
###############################################################################
# Create a data frame of unique MonitoringLocationIdentifier and Lagos_Name pairings
unique_lagos_monitoring <- unique(merged_comb_lakes_test[, c("MonitoringLocationIdentifier", "Lagos_Name")])

# Print each unique pairing of MonitoringLocationIdentifier and Lagos_Name
for (i in 1:nrow(unique_lagos_monitoring)) {
  monitoring_id <- unique_lagos_monitoring[i, "MonitoringLocationIdentifier"]
  lagos_name <- unique_lagos_monitoring[i, "Lagos_Name"]
  cat("MonitoringLocationIdentifier:", monitoring_id, "\tLagos_Name:", lagos_name, "\n")
}

#Attempt 1 Correction:
# Find the indices where the Lagos_Name is the long string
long_name_indices <- merged_comb_lakes_test$Lagos_Name == "Lake Minnetonka; Black Lake; Forest Lake; Lower Lake; Seton Lake; Emerald Lake"

# Replace the Lagos_Name with "Lake Minnetonka" for these indices
merged_comb_lakes_test$Lagos_Name[long_name_indices] <- "Lake Minnetonka"

#Seperating the Minnetonka's
merged_comb_lakes_test$Lagos_Name[merged_comb_lakes_test$Lagos_Name == "Lake Minnetonka"] <- "Minnetonka"
minnetonka_count <- 1
for (id in unique(merged_comb_lakes_test$MonitoringLocationIdentifier[merged_comb_lakes_test$Lagos_Name == "Minnetonka"])) {
  merged_comb_lakes_test$Lagos_Name[merged_comb_lakes_test$MonitoringLocationIdentifier == id] <- paste0("Minnetonka ", minnetonka_count)
  minnetonka_count <- minnetonka_count + 1
}

length(unique(merged_comb_lakes_test$MonitoringLocationIdentifier))
#Still 93 Values

length(unique(merged_comb_lakes_test$Lagos_Name))
# Now 70 unique Names

#Comparing each unique value for both columns
unique_pairs <- unique(merged_comb_lakes_test[c("MonitoringLocationIdentifier", "Lagos_Name")])
print(unique_pairs)

#There are two fish lakes....
merged_comb_lakes_test$Lagos_Name[merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-27-0118-00-404"
                                  & merged_comb_lakes_test$Lagos_Name == "Fish Lake"] <- "Fish Lake 2"

#Comparing each unique value for both columns
unique_pairs <- unique(merged_comb_lakes_test[c("Lagos_Name", "MonitoringLocationIdentifier")])
print(unique_pairs)

#And two Round lakes....
merged_comb_lakes_test$Lagos_Name[merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-62-0012-00-201"
                                  & merged_comb_lakes_test$Lagos_Name == "Round Lake"] <- "Round Lake 2"

length(unique(merged_comb_lakes_test$Lagos_Name))
# Now 72 unique names!


merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-82-0166-00-101",
                                            "Carver Lake", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-82-0115-00-101",
                                            "Tanners Lake", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "WIDNR_WQX-804600",
                                            "Lake Monona", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "WIDNR_WQX-805400",
                                            "Lake Mendota", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "WIDNR_WQX-985100",
                                            "Fish Lake 3", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-82-0167-00-234",
                                            "White Bear Lake", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "WIDNR_WQX-1881900",
                                            "Sparking Lake", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "WIDNR_WQX-1835300",
                                            "Big Muskellunge Lake", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "WIDNR_WQX-2331600",
                                            "Trout Lake", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-82-0092-00-201",
                                            "Powers Lake", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-82-0049-00-204",
                                            "Big Carnelian Lake", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-82-0046-00-203",
                                            "Square Lake", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-82-0044-00-202",
                                            "West Boot Lake", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-82-0052-04-203",
                                            "Big Marine Lake", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-86-0297-00-201",
                                            "Scott Lake", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-86-0282-00-201",
                                            "Lake Louisa", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-86-0282-00-201",
                                            "Lake Louisa", merged_comb_lakes_test$Lagos_Name)

merged_comb_lakes_test$Lagos_Name <- ifelse(merged_comb_lakes_test$MonitoringLocationIdentifier == "MNPCA-82-0021-00-201",
                                            "Long Lake", merged_comb_lakes_test$Lagos_Name)


length(unique(merged_comb_lakes_test$MonitoringLocationIdentifier))
#Still 93 Values

#Comparing each unique value for both columns
unique_pairs <- unique(merged_comb_lakes_test[c("Lagos_Name", "MonitoringLocationIdentifier")])
print(unique_pairs)


length(unique(merged_comb_lakes_test$Lagos_Name))
#Only 86 Values, Some mismatched must have happened, will fix later.


#It seems this code works, but not entirely how I intended, the Unidentified numbers are not
#consecutive, but they are unique to each lake, so it works for our purposes/time. 

###############################################################################
#Inserting Names End
###############################################################################

#Replacing the Comb_Lakes now that I am confident my code worked

Comb_Lakes_2 <- merged_comb_lakes_test

#Removing the extra Lagos_Name

#Reordering the Lake Names
Comb_Lakes_2 <- Comb_Lakes_2 %>%
  select(...1, MonitoringLocationIdentifier, State, Lagos_Name, everything())

#Renaming the DO column so it is DO concentration

colnames(Comb_Lakes_2)[colnames(Comb_Lakes_2)=="DO"] <- "DO_Con"
colnames(Comb_Lakes_2)[colnames(Comb_Lakes_2)=="DO_saturation"] <- "DO_Sat"
colnames(Comb_Lakes_2)[colnames(Comb_Lakes_2)=="Date"] <- "date"



#Comb_Lakes$date <- as.Date(Comb_Lakes$date, format = "%d/%m/%Y")
##now need to make one more change to the date format for TheilSen to run without errors
#library(lubridate) 
#Comb_Lakes$date <- lubridate::ymd_hms(paste(Comb_Lakes$date, "00:00:00"))


###############################################################################
###########################-Individual-Trend-Analysis-#########################
###############################################################################
library(openair)

#DO Concentration Trends
Sens_Individual_DO_Con <- TheilSen(Comb_Lakes_2, pollutant = "DO_Con", 
                              type = "Lagos_Name", deseason = FALSE,
                              xlab = "year", ylab = "DO Con (whole column)")
Sens_Individual_DO_Con$data[[2]]
head(Sens_Individual_DO_Con$data[[1]])


Sens_Individual_DO_Con_df <- data.frame(Sens_Individual_DO_Con$data[[2]])


#DO Saturation Trends
Sens_Individual_DO_Sat <- TheilSen(Comb_Lakes_2, pollutant = "DO_Sat", 
                                   type = "Lagos_Name", deseason = FALSE,
                                   xlab = "year", ylab = "DO Sat (whole column)")
Sens_Individual_DO_Sat$data[[2]]
head(Sens_Individual_DO_Sat$data[[1]])


Sens_Individual_DO_Sat_df <- data.frame(Sens_Individual_DO_Sat$data[[2]])


#Temp Trends
Sens_Individual_Temp <- TheilSen(Comb_Lakes_2, pollutant = "Temperature", 
                                   type = "Lagos_Name", deseason = FALSE,
                                   xlab = "year", ylab = "Temperature (whole column)")
Sens_Individual_Temp$data[[2]]
head(Sens_Individual_Temp$data[[1]])


Sens_Individual_Temp_df <- data.frame(Sens_Individual_Temp$data[[2]])

#Layer Breakdown
##########################################################################

# Create and Epilimnion value df
Comb_Lakes_Epi <- subset(Comb_Lakes_2, Layer == "Epilimnion")
# Create a Hypolimnion value df
Comb_Lakes_Hypo <- subset(Comb_Lakes_2, Layer == "Hypolimnion")

#DO Concentration Trends Epi
Sens_Individual_DO_Con_Epi <- TheilSen(Comb_Lakes_Epi, pollutant = "DO_Con", 
                                   type = "Lagos_Name", deseason = FALSE,
                                   xlab = "year", ylab = "DO Con Epi")
Sens_Individual_DO_Con_Epi$data[[2]]
head(Sens_Individual_DO_Con_Epi$data[[1]])


Sens_Individual_DO_Con_Epi_df <- data.frame(Sens_Individual_DO_Con_Epi$data[[2]])


#DO Saturation Epi Trends
Sens_Individual_DO_Sat_Epi <- TheilSen(Comb_Lakes_Epi, pollutant = "DO_Sat", 
                                   type = "Lagos_Name", deseason = FALSE,
                                   xlab = "year", ylab = "DO Sat Epi")
Sens_Individual_DO_Sat_Epi$data[[2]]
head(Sens_Individual_DO_Sat_Epi$data[[1]])


Sens_Individual_DO_Sat_Epi_df <- data.frame(Sens_Individual_DO_Sat_Epi$data[[2]])


#Temp Epi Trends
Sens_Individual_Temp_Epi <- TheilSen(Comb_Lakes_Epi, pollutant = "Temperature", 
                                 type = "Lagos_Name", deseason = FALSE,
                                 xlab = "year", ylab = "Temperature Epi")
Sens_Individual_Temp_Epi$data[[2]]
head(Sens_Individual_Temp_Epi$data[[1]])


Sens_Individual_Temp_Epi_df <- data.frame(Sens_Individual_Temp_Epi$data[[2]])

###############Hypo

#DO Concentration Hypo Trends
Sens_Individual_DO_Con_Hypo <- TheilSen(Comb_Lakes_Hypo, pollutant = "DO_Con", 
                                       type = "Lagos_Name", deseason = FALSE,
                                       xlab = "year", ylab = "DO Con Hypo")
Sens_Individual_DO_Con_Hypo$data[[2]]
head(Sens_Individual_DO_Con_Hypo$data[[1]])


Sens_Individual_DO_Con_Hypo_df <- data.frame(Sens_Individual_DO_Con_Hypo$data[[2]])


#DO Saturation Hypo Trends
Sens_Individual_DO_Sat_Hypo <- TheilSen(Comb_Lakes_Hypo, pollutant = "DO_Sat", 
                                       type = "Lagos_Name", deseason = FALSE,
                                       xlab = "year", ylab = "DO Sat Hypo")
Sens_Individual_DO_Sat_Hypo$data[[2]]
head(Sens_Individual_DO_Sat_Hypo$data[[1]])


Sens_Individual_DO_Sat_Hypo_df <- data.frame(Sens_Individual_DO_Sat_Hypo$data[[2]])


#Temp Hypo Trends
Sens_Individual_Temp_Hypo <- TheilSen(Comb_Lakes_Hypo, pollutant = "Temperature", 
                                     type = "Lagos_Name", deseason = FALSE,
                                     xlab = "year", ylab = "Temperature Hypo")
Sens_Individual_Temp_Hypo$data[[2]]
head(Sens_Individual_Temp_Hypo$data[[1]])


Sens_Individual_Temp_Hypo_df <- data.frame(Sens_Individual_Temp_Hypo$data[[2]])

#Merging all of this data into one data frame
#################################################################

#Removing the odd rows (Duplicates with NA's)

df_list <- list(Sens_Individual_DO_Con_df, Sens_Individual_DO_Sat_df,
                Sens_Individual_Temp_df, Sens_Individual_DO_Con_Epi_df,
                Sens_Individual_DO_Sat_Epi_df, Sens_Individual_Temp_Epi_df,
                Sens_Individual_DO_Con_Hypo_df, Sens_Individual_DO_Sat_Hypo_df,
                Sens_Individual_Temp_Hypo_df) # List of data frames

for (i in 1:length(df_list)) {
  df_list[[i]] <- df_list[[i]][seq(1, nrow(df_list[[i]]), by=2),] # Remove even rows
  rownames(df_list[[i]]) <- 1:nrow(df_list[[i]]) # Renumber rows
}

#I visually confirmed this worked how I wanted it to.


Individual_Comb_Sens <- do.call(rbind, df_list) #Combine each df together

source_analysis <- c("DO_Con_T", "DO_Sat_T", "Temp_T", "DO_Con_Epi", "DO_Sat_Epi",
              "Temp_Epi", "DO_Con_Hypo", "DO_Sat_Hypo", "Temp_Hypo") #Creates column relevant to which analysis type was done
Individual_Comb_Sens$Analysis <- rep(source_analysis, sapply(df_list, nrow)) #Inserts them via their source df

#Reordering the column names
Individual_Comb_Sens <- Individual_Comb_Sens %>%
  select(Analysis, Lagos_Name, p.stars, p, slope,intercept, everything())

sum(Individual_Comb_Sens$p < 0.05) #There are 115 significant trends

Interim_Data_ID <- Comb_Lakes_2[, c("MonitoringLocationIdentifier", "State", "Lagos_Name", "Latitude", "Longitude")]

Interim_Data_ID <- distinct(Interim_Data_ID) #remove duplicates

merged_trends_1 <- merge(Individual_Comb_Sens, Interim_Data_ID, by = "Lagos_Name")

Individual_Comb_Sens <- merged_trends_1

