## Making one plot per parameter; scatterplot of epi trend x vs hypo trend y.
## This will reveal where the majority of lakes lie with regard to directions 
## of both trends and enables us to find outliers. Perhaps thinking about 
## what could be causing lakes to have consistent paired trends in the epi and 
## hypo could be interesting, such as a big cluster of lakes with increasing 
## epi do and decreasing hypo do being increasingly eutrophic, or a cluster of
## lakes with strong hypo positive DO trends being lakes with zebra mussels 
## introduced.

## Will first plot with a point for each lake, then will plot with a point for 
## each lake with all lakes with significant epi and hypo trends highlighted. 

## TEMPERATURE ##
##have to delete one row for SPRING lake, missing hypo data and cant plot with
##different number of rows for the x and y variables
hypoedit_tempbins <- Sens_Individual_Temp_Hypo_df[-c(155),]

plot(Sens_Individual_Temp_Epi_df$slope, hypoedit_tempbins$slope, pch = 20, xlab = "Epilimnion Temp Annual Trend (C/yr)", ylab = "Hypolimnion Temp Annual Trend (C/yr)")+
  abline(v=0)+
  abline(h = 0)

## DO CON ##
hypoedit_doconbins <- Sens_Individual_DO_Con_Hypo_df[-c(155),]

plot(Sens_Individual_DO_Con_Epi_df$slope, hypoedit_doconbins$slope, pch = 20, xlab = "Epilimnion DO Concentration Annual Trend (mg/L/yr)", ylab = "Hypolimion DO Concentration Annual Trend (C/yr)")+
  abline(v=0)+
  abline(h = 0)

## DO SAT ##
hypoedit_dosatbins <- Sens_Individual_DO_Sat_Hypo_df[-c(155),]

plot(Sens_Individual_DO_Sat_Epi_df$slope, hypoedit_dosatbins$slope, pch = 20, xlab = "Epilimnion DO Saturation Annual Trend (% change/yr)", ylab = "Hypolimion DO Saturation Annual Trend (% change/yr)")+
  abline(v=0)+
  abline(h = 0)
