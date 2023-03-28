##Testing for spatial autocorrelation with Morans I from lctools package
  ##testing for each parameter/layer combination
  ##use the Annual_Comb produced by the "Comb_Lakes_Script" and the "DO_analysis" script as starting data

##Separate epi and hypo data
 # Epilimnion data only
 Annual_Comb_Epi <- subset(Annual_Comb, Layer == "Epilimnion")
 # Hypolimnion data only
 Annual_Comb_Hypo <- subset(Annual_Comb, Layer == "Hypolimnion")
 
## MORANS I TESTS FOR SPATIAL AUTOCORRELATION ###############################################################
  ## install and load lctools package
  install.packages('lctools')
  library(lctools)
 
 ## IMPORTANT notes on bandwidth input setting:
   ##Bandwidth is # of nearest neighbors taken into account for each point for weighting,
   ##setting to 20 in all below but we should discuss, as changing this changes the results.
   
   ## Epi Temp ##
   epitemp_mori <- moransI(cbind(Annual_Comb_Epi$Longitude, Annual_Comb_Epi$Latitude), 100, Annual_Comb_Epi$Annual_Temp)
   View(epitemp_mori)
   ## results: moran's i = 0.266
 
   ## Hypo Temp ##
   hypotemp_mori <- moransI(cbind(Annual_Comb_Hypo$Longitude, Annual_Comb_Hypo$Latitude), 100, Annual_Comb_Hypo$Annual_Temp)
   View(hypotemp_mori)
   ## results: moran's i = 0.040
   
   ## Epi DO conc ##
   epidoconc_mori <- moransI(cbind(Annual_Comb_Epi$Longitude, Annual_Comb_Epi$Latitude), 100, Annual_Comb_Epi$Annual_DO_Con)
   View(epidoconc_mori)
   ## results: moran's i = 0.253
   
   ## Hypo DO conc ##
   hypodoconc_mori <- moransI(cbind(Annual_Comb_Hypo$Longitude, Annual_Comb_Hypo$Latitude), 100, Annual_Comb_Hypo$Annual_DO_Con)
   View(hypodoconc_mori)
   ## results: moran's i = 0.010
   
   ## Epi DO sat ##
   epidosat_mori <- moransI(cbind(Annual_Comb_Epi$Longitude, Annual_Comb_Epi$Latitude), 100, Annual_Comb_Epi$Annual_DO_Sat)
   View(epidosat_mori)
   ## results: moran's i = 0.204
   
   ## Hypo DO sat ##
   hypodosat_mori <- moransI(cbind(Annual_Comb_Hypo$Longitude, Annual_Comb_Hypo$Latitude), 100, Annual_Comb_Hypo$Annual_DO_Sat)
   View(hypodosat_mori)
   ## results: moran's i = 0.009
   
   ## So, the epilimnion temp appears to be more spatially autocorrelated
   ## than the hypolimnion temp, and same trend for the DO conc and sat.
   ## Hypolimnion parameters have extremely close to zero moran's i values
   ## Epilimnion parameters all have moran's i value between 0.204 and 0.266
   ## REVISIT THE BANDWIDTH INPUT SETTING, as we may have reasons to use a nummber that is not 100.
   
   
   
 