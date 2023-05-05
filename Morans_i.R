#### This commented out first section is old, and not usable, because I was
#### looking for autocorrelation among the measurements and not the trends
###########################################################################
##Testing for spatial autocorrelation with Morans I from lctools package
  ##testing for each parameter/layer combination
  ##use the Annual_Comb produced by the "Comb_Lakes_Script" and the "DO_analysis" script as starting data
##Separate epi and hypo data
 # Epilimnion data only
## Annual_Comb_Epi <- subset(Annual_Comb, Layer == "Epilimnion")
 # Hypolimnion data only
## Annual_Comb_Hypo <- subset(Annual_Comb, Layer == "Hypolimnion")
## MORANS I TESTS FOR SPATIAL AUTOCORRELATION ###############################################################
  ## install and load lctools package
##  install.packages('lctools')
##  library(lctools)
 ## IMPORTANT notes on bandwidth input setting:
   ##Bandwidth is # of nearest neighbors taken into account for each point for weighting,
   ##setting to 100 in all below but we should discuss, as changing this changes the results.
   ## Epi Temp ##
##   epitemp_mori <- moransI(cbind(Annual_Comb_Epi$Longitude, Annual_Comb_Epi$Latitude), 100, Annual_Comb_Epi$Annual_Temp)
##   View(epitemp_mori)
##   print(epitemp_mori)
   ## results: $Morans.I
   ##          [1] 0.3241519
   ##          $Expected.I
   ##          [1] -0.0007581501
   ##          $z.resampling
   ##          [1] 104.3962
   ##          $z.randomization
   ##          [1] 104.4109
   ##          $p.value.resampling
   ##          [1] 0
   ##          $p.value.randomization
   ##          [1] 0
   ## Hypo Temp ##
 ##  hypotemp_mori <- moransI(cbind(Annual_Comb_Hypo$Longitude, Annual_Comb_Hypo$Latitude), 100, Annual_Comb_Hypo$Annual_Temp)
 ##  View(hypotemp_mori)
 ##  print(hypotemp_mori)
   ## results: $Morans.I
   ##          [1] -0.01057147
   ##          $Expected.I
   ##          [1] -0.009708738
   ##          $z.resampling
   ##          [1] -1.332208
   ##          $z.randomization
   ##          [1] -1.037633
   ##          $p.value.resampling
   ##          [1] 0.182792
   ##          $p.value.randomization
   ##          [1] 0.2994411
   ## Epi DO conc ##
  ## epidoconc_mori <- moransI(cbind(Annual_Comb_Epi$Longitude, Annual_Comb_Epi$Latitude), 100, Annual_Comb_Epi$Annual_DO_Con)
  ## View(epidoconc_mori)
  ## print(epidoconc_mori)
   ## results: $Morans.I
   ##          [1] 0.5547985
   ##          $Expected.I
   ##          [1] -0.0007581501
   ##          $z.resampling
   ##          [1] 178.5047
   ##          $z.randomization
   ##          [1] 178.5407
   ##          $p.value.resampling
   ##          [1] 0
   ##          $p.value.randomization
   ##          [1] 0
   ## Hypo DO conc ##
 ##  hypodoconc_mori <- moransI(cbind(Annual_Comb_Hypo$Longitude, Annual_Comb_Hypo$Latitude), 100, Annual_Comb_Hypo$Annual_DO_Con)
 ##  View(hypodoconc_mori)
  ## print(hypodoconc_mori)
   ## results: $Morans.I
   ##          [1] -0.003757756
   ##          $Expected.I
   ##          [1] -0.009708738
   ##          $z.resampling
   ##          [1] 9.189315
   ##          $z.randomization
   ##          [1] 8.216028
   ##          $p.value.resampling
   ##          [1] 3.953507e-20
   ##          $p.value.randomization
   ##          [1] 2.10354e-16
   ## Epi DO sat ##
##   epidosat_mori <- moransI(cbind(Annual_Comb_Epi$Longitude, Annual_Comb_Epi$Latitude), 100, Annual_Comb_Epi$Annual_DO_Sat)
##   View(epidosat_mori)
 ##  print(epidosat_mori)
   ## results: $Morans.I
   ##          [1] 0.4584723
   ##          $Expected.I
   ##          [1] -0.0007581501
   ##          $z.resampling
   ##          [1] 147.5544
   ##          $z.randomization
   ##          [1] 147.5683
   ##          $p.value.resampling
   ##          [1] 0
   ##          $p.value.randomization
   ##          [1] 0
   ## Hypo DO sat ##
##   hypodosat_mori <- moransI(cbind(Annual_Comb_Hypo$Longitude, Annual_Comb_Hypo$Latitude), 100, Annual_Comb_Hypo$Annual_DO_Sat)
##   View(hypodosat_mori)
##   print(hypodosat_mori)
   ## results: $Morans.I
   ##          [1] -0.003878252
   ##          $Expected.I
   ##          [1] -0.009708738
   ##          $z.resampling
   ##          [1] 9.003249
   ##          $z.randomization
   ##          [1] 8.766376
   ##          $p.value.resampling
   ##          [1] 2.191353e-19
   ##          $p.value.randomization
   ##          [1] 1.845089e-18
   
   ## So, the epilimnion temp appears to be more spatially autocorrelated
   ## than the hypolimnion temp, and same trend for the DO conc and sat.
   ## Hypolimnion parameters have extremely close to zero moran's i values
   ## Epilimnion parameters all have moran's i value 0.32-0.66
   ## REVISIT THE BANDWIDTH INPUT SETTING, as we may have reasons to use a number that is not 100.
   ## Suspiciously low # of lake years in the Annual_Comb_Hypo df (104)
###########################################################################################################################
###########################################################################################################################

## FINAL MORANS I ANALYSIS ##
  ## first need to load in the Master_Slops csv and make a dataframe for each lyr+parameter(6 total)
  ## then will use lctools moransI.v function to compute outputs for multiple
  ## bandwidths and plot results
library(readxl)
Master_Slopes <- read_excel("Master_Slopes.xlsx")
View(Master_Slopes)

library(dplyr)
masterslopes_epi_temp <- filter(Master_Slopes, Analysis == "Temp_Epi")
masterslopes_hypo_temp <- filter(Master_Slopes, Analysis == "Temp_Hypo")
masterslopes_epi_docon <- filter(Master_Slopes, Analysis == "DO_Con_Epi")
masterslopes_hypo_docon <- filter(Master_Slopes, Analysis == "DO_Con_Hypo")
masterslopes_epi_dosat <- filter(Master_Slopes, Analysis == "DO_Sat_Epi")
masterslopes_hypo_dosat <- filter(Master_Slopes, Analysis == "DO_Sat_Hypo")

  ## Next need to do Moran's I analysis for each layer/parameter combo
  ## create bandwidths (5-70, intervals of 5)
library(lctools)
bws <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
  ## EPILIMNION TEMPERATURE ##
moransiv_epitemp <- moransI.v(cbind(masterslopes_epi_temp$Longitude, masterslopes_epi_temp$Latitude), bws, masterslopes_epi_temp$slope, WType='Binary', family='adaptive', plot = TRUE)
summary(moransiv_epitemp)
print(moransiv_epitemp)

  ## HYPOLIMNION TEMPERATURE ##
moransiv_hypotemp <- moransI.v(cbind(masterslopes_hypo_temp$Longitude, masterslopes_hypo_temp$Latitude), bws, masterslopes_hypo_temp$slope, WType='Binary', family='adaptive', plot = TRUE)
summary(moransiv_hypotemp)
print(moransiv_hypotemp)

  ## EPILIMNION DO CONCENTRATION ##
moransiv_epidocon <- moransI.v(cbind(masterslopes_epi_docon$Longitude, masterslopes_epi_docon$Latitude), bws, masterslopes_epi_docon$slope, WType='Binary', family='adaptive', plot = TRUE)
summary(moransiv_epidocon)
print(moransiv_epidocon)

  ## HYPOLIMNION DO CONCENTRATION ##
moransiv_hypodocon <- moransI.v(cbind(masterslopes_hypo_docon$Longitude, masterslopes_hypo_docon$Latitude), bws, masterslopes_hypo_docon$slope, WType='Binary', family='adaptive', plot = TRUE)
summary(moransiv_hypodocon)
print(moransiv_hypodocon)

  ## EPILIMNION DO SATURATION ##
moransiv_epidosat <- moransI.v(cbind(masterslopes_epi_dosat$Longitude, masterslopes_epi_dosat$Latitude), bws, masterslopes_epi_dosat$slope, WType='Binary', family='adaptive', plot = TRUE)
summary(moransiv_epidosat)
print(moransiv_epidosat)

  ## HYPOLIMNION DO SATURATION ##
moransiv_hypodosat <- moransI.v(cbind(masterslopes_hypo_dosat$Longitude, masterslopes_hypo_dosat$Latitude), bws, masterslopes_hypo_dosat$slope, WType='Binary', family='adaptive', plot = TRUE)
summary(moransiv_hypodosat)
print(moransiv_hypodosat)

##########################################################################
## NOW REMOVING ALL INSIG TRENDS FROM THE 6 DFs USED FOR mI IN LAST SECTION
## then re-running the Moran's I analyses
## Will include results of BOTH analyses and explain rationale in rough draft

  ##filter out all lakes with insignificant trends from above 6 dfs
masterslopes_epi_temp_sig <- filter(Master_Slopes, Analysis == "Temp_Epi" & p < 0.05)
masterslopes_hypo_temp_sig <- filter(Master_Slopes, Analysis == "Temp_Hypo" & p < 0.05)
masterslopes_epi_docon_sig <- filter(Master_Slopes, Analysis == "DO_Con_Epi" & p < 0.05)
masterslopes_hypo_docon_sig <- filter(Master_Slopes, Analysis == "DO_Con_Hypo" & p < 0.05)
masterslopes_epi_dosat_sig <- filter(Master_Slopes, Analysis == "DO_Sat_Epi" & p < 0.05)
masterslopes_hypo_dosat_sig <- filter(Master_Slopes, Analysis == "DO_Sat_Hypo" & p < 0.05)

  ## EPILIMNION TEMPERATURE ##
moransiv_epitemp_sig <- moransI.v(cbind(masterslopes_epi_temp_sig$Longitude, masterslopes_epi_temp_sig$Latitude), bws, masterslopes_epi_temp_sig$slope, WType='Binary', family='adaptive', plot = TRUE)
summary(moransiv_epitemp_sig)
print(moransiv_epitemp_sig)

  ## HYPOLIMNION TEMPERATURE ##
moransiv_hypotemp_sig <- moransI.v(cbind(masterslopes_hypo_temp_sig$Longitude, masterslopes_hypo_temp_sig$Latitude), bws, masterslopes_hypo_temp_sig$slope, WType='Binary', family='adaptive', plot = TRUE)
summary(moransiv_hypotemp_sig)
print(moransiv_hypotemp_sig)

  ## EPILIMNION DO CONCENTRATION ##
moransiv_epidocon_sig <- moransI.v(cbind(masterslopes_epi_docon_sig$Longitude, masterslopes_epi_docon_sig$Latitude), bws, masterslopes_epi_docon_sig$slope, WType='Binary', family='adaptive', plot = TRUE)
summary(moransiv_epidocon_sig)
print(moransiv_epidocon_sig)

  ## HYPOLIMNION DO CONCENTRATION ##
moransiv_hypodocon_sig <- moransI.v(cbind(masterslopes_hypo_docon_sig$Longitude, masterslopes_hypo_docon_sig$Latitude), bws, masterslopes_hypo_docon_sig$slope, WType='Binary', family='adaptive', plot = TRUE)
summary(moransiv_hypodocon_sig)
print(moransiv_hypodocon_sig)

  ## EPILIMNION DO SATURATION ##
moransiv_epidosat_sig <- moransI.v(cbind(masterslopes_epi_dosat_sig$Longitude, masterslopes_epi_dosat_sig$Latitude), bws, masterslopes_epi_dosat_sig$slope, WType='Binary', family='adaptive', plot = TRUE)
summary(moransiv_epidosat_sig)
print(moransiv_epidosat_sig)

  ## HYPOLIMNION DO SATURATION ##
moransiv_hypodosat_sig <- moransI.v(cbind(masterslopes_hypo_dosat_sig$Longitude, masterslopes_hypo_dosat_sig$Latitude), bws, masterslopes_hypo_dosat_sig$slope, WType='Binary', family='adaptive', plot = TRUE)
summary(moransiv_hypodosat_sig)
print(moransiv_hypodosat_sig)

##################################################################
## Now making dataframes with trend values weighted to do Moran's I
## on all 93 lakes but with individual lake trends that are not significant
## reduced by an order of magnitude to contribute less strong values.

## First, make strings of weighted trend values
masterslopes_epi_temp_weighted <- ifelse(masterslopes_epi_temp$p < 0.05, masterslopes_epi_temp$slope, masterslopes_epi_temp$slope/10)
masterslopes_hypo_temp_weighted <- ifelse(masterslopes_hypo_temp$p < 0.05, masterslopes_hypo_temp$slope, masterslopes_hypo_temp$slope/10)
masterslopes_epi_docon_weighted <- ifelse(masterslopes_epi_docon$p < 0.05, masterslopes_epi_docon$slope, masterslopes_epi_docon$slope/10)
masterslopes_hypo_docon_weighted <- ifelse(masterslopes_hypo_docon$p < 0.05, masterslopes_hypo_docon$slope, masterslopes_hypo_docon$slope/10)
masterslopes_epi_dosat_weighted <- ifelse(masterslopes_epi_dosat$p < 0.05, masterslopes_epi_dosat$slope, masterslopes_epi_dosat$slope/10)
masterslopes_hypo_dosat_weighted <- ifelse(masterslopes_hypo_dosat$p < 0.05, masterslopes_hypo_dosat$slope, masterslopes_hypo_dosat$slope/10)

## Next, make new master weighted df with lakename, lat, long, and all 6 weighted trend strings
masterslopes_weighted_epitemp <- data.frame(masterslopes_epi_temp$Lagos_Name, masterslopes_epi_temp$Latitude, masterslopes_epi_temp$Longitude, masterslopes_epi_temp_weighted)
masterslopes_weighted_hypotemp <- data.frame(masterslopes_hypo_temp$Lagos_Name, masterslopes_hypo_temp$Latitude, masterslopes_hypo_temp$Longitude, masterslopes_hypo_temp_weighted)
masterslopes_weighted_epidocon <- data.frame(masterslopes_epi_docon$Lagos_Name, masterslopes_epi_docon$Latitude, masterslopes_epi_docon$Longitude, masterslopes_epi_docon_weighted)
masterslopes_weighted_hypodocon <- data.frame(masterslopes_hypo_docon$Lagos_Name, masterslopes_hypo_docon$Latitude, masterslopes_hypo_docon$Longitude, masterslopes_hypo_docon_weighted)
masterslopes_weighted_epidosat <- data.frame(masterslopes_epi_dosat$Lagos_Name, masterslopes_epi_dosat$Latitude, masterslopes_epi_dosat$Longitude, masterslopes_epi_dosat_weighted)
masterslopes_weighted_hypodosat <- data.frame(masterslopes_hypo_dosat$Lagos_Name, masterslopes_hypo_dosat$Latitude, masterslopes_hypo_dosat$Longitude, masterslopes_hypo_dosat_weighted)

## Now, finally, do Moran's I for each
library(lctools)

  ## Epi temp ##
epitemp_weighted_mori <- moransI(cbind(masterslopes_weighted_epitemp$masterslopes_epi_temp.Longitude, masterslopes_weighted_epitemp$masterslopes_epi_temp.Latitude), 5, masterslopes_epi_temp_weighted)
print(epitemp_weighted_mori)

  ## Hypo temp ##
hypotemp_weighted_mori <- moransI(cbind(masterslopes_weighted_hypotemp$masterslopes_hypo_temp.Longitude, masterslopes_weighted_hypotemp$masterslopes_hypo_temp.Latitude), 5, masterslopes_hypo_temp_weighted)
print(hypotemp_weighted_mori)

 ## Epi docon ##
epidocon_weighted_mori <- moransI(cbind(masterslopes_weighted_epidocon$masterslopes_epi_docon.Longitude, masterslopes_weighted_epidocon$masterslopes_epi_docon.Latitude), 5, masterslopes_epi_docon_weighted)
print(epidocon_weighted_mori)

 ## Hypo docon ##
hypodocon_weighted_mori <- moransI(cbind(masterslopes_weighted_hypodocon$masterslopes_hypo_docon.Longitude, masterslopes_weighted_hypodocon$masterslopes_hypo_docon.Latitude), 5, masterslopes_hypo_docon_weighted)
print(hypodocon_weighted_mori)

  ## Epi dosat ##
epidosat_weighted_mori <- moransI(cbind(masterslopes_weighted_epidosat$masterslopes_epi_dosat.Longitude, masterslopes_weighted_epidosat$masterslopes_epi_dosat.Latitude), 5, masterslopes_epi_dosat_weighted)
print(epidosat_weighted_mori)

  ## Hypo dosat ##
hypodosat_weighted_mori <- moransI(cbind(masterslopes_weighted_hypodosat$masterslopes_hypo_dosat.Longitude, masterslopes_weighted_hypodosat$masterslopes_hypo_dosat.Latitude), 5, masterslopes_hypo_dosat_weighted)
print(hypodosat_weighted_mori)

## Looking for autocorrel among lake depths
depths <- masterslopes_epi_temp$Max_Depth
depths_num <- as.numeric(depths)
lakedepths_mori <- moransI(cbind(masterslopes_weighted_epitemp$masterslopes_epi_temp.Longitude, masterslopes_weighted_epitemp$masterslopes_epi_temp.Latitude), 5, depths_num)
print(lakedepths_mori)

## Looking for autocorrel among lake ag
ag <- masterslopes_epi_temp$Cult_Crop_Pct
ag_num <- as.numeric(ag)
ag_mori <- moransI(cbind(masterslopes_weighted_epitemp$masterslopes_epi_temp.Longitude, masterslopes_weighted_epitemp$masterslopes_epi_temp.Latitude), 5, ag_num)
print(ag_mori)

## Looking for autocorrel among lake depth plus ag
depth_plus_ag <- depths_num + ag_num
depth_plus_ag_mori <- moransI(cbind(masterslopes_weighted_epitemp$masterslopes_epi_temp.Longitude, masterslopes_weighted_epitemp$masterslopes_epi_temp.Latitude), 5, depth_plus_ag)
print(depth_plus_ag_mori)

## Looking for autocorrel among lake dev%
dev <- masterslopes_epi_temp$Total_Dev_Pct
dev_num <- as.numeric(dev)
dev_mori <- moransI(cbind(masterslopes_weighted_epitemp$masterslopes_epi_temp.Longitude, masterslopes_weighted_epitemp$masterslopes_epi_temp.Latitude), 5, dev_num)
print(dev_mori)

## Looking for autocorrel among lake dev% + ag%
devag <- masterslopes_epi_temp$Ag_Plus_Dev
devag_num <- as.numeric(devag)
devag_mori <- moransI(cbind(masterslopes_weighted_epitemp$masterslopes_epi_temp.Longitude, masterslopes_weighted_epitemp$masterslopes_epi_temp.Latitude), 5, devag_num)
print(devag_mori)
