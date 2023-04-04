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
I 
# Use match() to create an index of the matching rows
comb_index <- match(Comb_Lakes$Latitude, relevant_rows_comb$lake_lat_decdeg)

# Merge the data frames using the index
merged_comb_lakes_test <- cbind(Comb_Lakes, relevant_rows_comb[comb_index, "lake_namelagos"])

# Rename the merged column to "Lagos_Name"
colnames(merged_comb_lakes_test)[colnames(merged_comb_lakes_test) == "lake_namelagos"] <- "Lagos_Name"

#confirming if the merge worked
summary(is.na(merged_comb_lakes_test$Lagos_Name))
#There are still 40664 rows that do not have a lake name. 

table(merged_comb_lakes_test$State)

?table()

