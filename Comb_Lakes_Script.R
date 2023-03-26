library(readr)
MN_Filtered_Obs <- read_csv("MN.Filtered.Obs.csv")
View(MN_Filtered_Obs)

library(readr)
WI_Filtered_Obs <- read_csv("WI.Filtered.Obs.csv")
View(WI_Filtered_Obs)

library(readr)
MI_Filtered_Obs <- read_csv("MI.Filtered.Obs.csv")
View(MI_Filtered_Obs)

library(readr)
WIprofiles_0_5 <- read_csv("WIprofiles_0.5.csv")
View(WIprofiles_0_5)

library(readr)
MIprofiles_0_5 <- read_csv("MIprofiles_0.5.csv")
View(MIprofiles_0_5)

library(readr)
MNprofiles_0_5 <- read_csv("MNprofiles_0.5.csv")
View(MNprofiles_0_5)


MNprofiles_0_5 <- MNprofiles_0_5 %>% select(-DOW, everything()) #Adds DOW to the end of the data frame

MIprofiles_0_5$DOW <- NA   #adds a DOW column
WIprofiles_0_5$DOW <- NA

Added_MN <- MNprofiles_0_5 %>%
  mutate("State" = "MN")

Added_WI <- WIprofiles_0_5 %>%
  mutate("State" = "WI")

Added_MI <- MIprofiles_0_5 %>%
  mutate("State" = "MI")



Comb_Lakes <- rbind(Added_MI, Added_MN, Added_WI)







