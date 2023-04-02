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
   ##setting to 100 in all below but we should discuss, as changing this changes the results.
   
   ## Epi Temp ##
   epitemp_mori <- moransI(cbind(Annual_Comb_Epi$Longitude, Annual_Comb_Epi$Latitude), 100, Annual_Comb_Epi$Annual_Temp)
   View(epitemp_mori)
   print(epitemp_mori)
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
   hypotemp_mori <- moransI(cbind(Annual_Comb_Hypo$Longitude, Annual_Comb_Hypo$Latitude), 100, Annual_Comb_Hypo$Annual_Temp)
   View(hypotemp_mori)
   print(hypotemp_mori)
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
   epidoconc_mori <- moransI(cbind(Annual_Comb_Epi$Longitude, Annual_Comb_Epi$Latitude), 100, Annual_Comb_Epi$Annual_DO_Con)
   View(epidoconc_mori)
   print(epidoconc_mori)
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
   hypodoconc_mori <- moransI(cbind(Annual_Comb_Hypo$Longitude, Annual_Comb_Hypo$Latitude), 100, Annual_Comb_Hypo$Annual_DO_Con)
   View(hypodoconc_mori)
   print(hypodoconc_mori)
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
   epidosat_mori <- moransI(cbind(Annual_Comb_Epi$Longitude, Annual_Comb_Epi$Latitude), 100, Annual_Comb_Epi$Annual_DO_Sat)
   View(epidosat_mori)
   print(epidosat_mori)
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
   hypodosat_mori <- moransI(cbind(Annual_Comb_Hypo$Longitude, Annual_Comb_Hypo$Latitude), 100, Annual_Comb_Hypo$Annual_DO_Sat)
   View(hypodosat_mori)
   print(hypodosat_mori)
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
   
   
   