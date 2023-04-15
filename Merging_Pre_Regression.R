#This script is designed to filter all of our outsourced data and to filter for lakes on intrest.
#Requires running Comb_Lakes_Script.R and Individual_Lake_Analysis before dfs are correct to work with


#Comb_Lakes_2 Is the master df for all of the observed data analysis and will be used for filtering
#At time of wringing, it has 182932 obs of 23 variables.
library(tidyverse)

#df used for filtering
library(readr)
lake_link_rantala <- read_csv("lake_link_rantala.csv")



colnames(lake_link_rantala)[colnames(lake_link_rantala) == "wqp_monitoringlocationidentifier"] <- "MonitoringLocationIdentifier"


#filtering so lake_link_rantala only contains columns in Comb_Lakes_2
filtered_lake_link_rantala <- lake_link_rantala[lake_link_rantala$MonitoringLocationIdentifier %in% Comb_Lakes_2$MonitoringLocationIdentifier, ]

#Confirming that each has the same number of MLIs

n_distinct(filtered_lake_link_rantala$MonitoringLocationIdentifier)
#86 unique values
n_distinct(Comb_Lakes_2$MonitoringLocationIdentifier)
#93 unique values

#What's going on? Troubleshooting:

# create a vector of MonitoringLocationIdentifiers that are unique to either data frame
unique_ids_comb <- setdiff(Comb_Lakes_2$MonitoringLocationIdentifier, filtered_lake_link_rantala$MonitoringLocationIdentifier)
unique_ids_link <- setdiff(filtered_lake_link_rantala$MonitoringLocationIdentifier, Comb_Lakes_2$MonitoringLocationIdentifier)

# print the unique MonitoringLocationIdentifiers
cat("Unique MonitoringLocationIdentifiers:\n")
if (length(unique_ids_comb) > 0) {
  cat("From Comb_Lakes_2:\n")
  cat(paste(unique_ids_comb, collapse = "\n"), "\n")
}
if (length(unique_ids_link) > 0) {
  cat("From filtered_lake_link_rantala:\n")
  cat(paste(unique_ids_link, collapse = "\n"), "\n")
}

#These lakes are not in the lake_link_rantala df

#From Comb_Lakes_2:
 # WIDNR_WQX-804600
#WIDNR_WQX-805400
#WIDNR_WQX-985100
#WIDNR_WQX-1842400
#WIDNR_WQX-1881900
#WIDNR_WQX-1835300
#WIDNR_WQX-2331600 


# Remove duplicates from filtered_lake_link_rantala
filtered_lake_link_rantala_2 <- distinct(filtered_lake_link_rantala, MonitoringLocationIdentifier, .keep_all = TRUE)

# left join filtered_lake_link_rantala to Comb_Lakes_2 by MonitoringLocationIdentifier
Comb_Lakes_Link <- Comb_Lakes_2 %>% left_join(filtered_lake_link_rantala_2, by = "MonitoringLocationIdentifier")

dim(Comb_Lakes_2)
dim(Comb_Lakes_Link)

#Converting the lake_nhdid to a character string for joining
Comb_Lakes_Link$lake_nhdid <- as.character(Comb_Lakes_Link$lake_nhdid)


#Now to link the link with the Read data to only include matches in the Read dataset

Read_filter <- read_csv("~/Desktop/Big Files/Read_filtered_4_14_23.csv")

colnames(Read_filter)[colnames(Read_filter) == "site_id"] <- "lake_nhdid"

#Now I need to remove "ndhdr_" from each cell in the Read nhdid before I can join

# Load the stringr package
library(stringr)

# Remove the "nhdhr_" prefix from the identifying number column in Read_filter
Read_filter$lake_nhdid <- str_remove(Read_filter$lake_nhdid, "nhdhr_")

# Count the number of unique values in Read_filter$lake_nhdid that match with Comb_Lakes_Link$lake_nhdid
num_matches <- length(intersect(Read_filter$lake_nhdid, Comb_Lakes_Link$lake_nhdid))

# Print the result
cat("The number of unique values in both columns that match is:", num_matches)


#filtering so Read_filter only contains rows in Comb_Lakes_Link
Filtered_Read_Link<- Read_filter[Read_filter$lake_nhdid %in% Comb_Lakes_Link$lake_nhdid, ]

#Okay, so now this df is all of the Read Data, annually summarized, to pull trends from:
#It may be an issue that we don't have the raw data available and we'll be running trends on annual summaries









