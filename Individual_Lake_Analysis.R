#Script for measuring trends within individual lakes.

#This df is a result of Comb_Lakes_Script and DO_analysis:
Comb_Lakes
#Should have 147653 obs. of 19 variables

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


#The following code is chatGPT generated
###############################################################################
# Create a vector to store the unique identifiers for each MonitoringLocationIdentifier
unique_ids <- unique(merged_comb_lakes_test$MonitoringLocationIdentifier)
id_map <- setNames(1:length(unique_ids), unique_ids)

# Define a function to generate the new Lagos_Name values
gen_new_name <- function(name, id, state) {
  if (is.na(name)) {
    # Generate a new name based on the given format
    unique_id <- id_map[id]
    new_name <- paste0("Unidentified (", state, ") ", unique_id)
  } else {
    new_name <- name
  }
  return(new_name)
}

# Apply the gen_new_name function to the Lagos_Name column
merged_comb_lakes_test$Lagos_Name <- mapply(gen_new_name, 
                                            merged_comb_lakes_test$Lagos_Name, 
                                            merged_comb_lakes_test$MonitoringLocationIdentifier, 
                                            merged_comb_lakes_test$State)

# Print the updated data frame
print(merged_comb_lakes_test)
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
#Still 57 Values

length(unique(merged_comb_lakes_test$Lagos_Name))
# Now 55 unique Names

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
# Now 57 unique names!!!!!

#It seems this code works, but not entirely how I intended, the Unidentified numbers are not
#consecutive, but they are unique to each lake, so it works for our purposes/time. 

###############################################################################
#Inserting Names End
###############################################################################

#Replacing the Comb_Lakes now that I am confident my code worked

Comb_Lakes <- merged_comb_lakes_test























