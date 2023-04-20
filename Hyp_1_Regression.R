#Multiple Linear Regression to test drivers of increasing epilimnetic temperatures

library(sensemakr)
library(car)
library(carData)

#Dataframe dependent for the MLR generated from the Master_Slopes_Script
#Master_Slopes 1174 obs of 33 variables

#Editing Master_Slopes to have an equal number of total measurements for the modeled/measured data

# Get the unique MonitoringLocationIdentifier values from the Modeled data
Modeled_Locations <- unique(Master_Slopes$MonitoringLocationIdentifier[Master_Slopes$Origin == "Modeled"])

# Subset the Measured data to only include the locations that are in the Modeled data
Measured_Slopes <- subset(Master_Slopes, Origin == "Measured" & MonitoringLocationIdentifier %in% Modeled_Locations)

# Combine the Modeled and Measured data
Combined_Slopes <- rbind(Master_Slopes[Master_Slopes$Origin == "Modeled",], Measured_Slopes)



#The first hypothesis to test is how the trends for summer mean temp and stratification duration
#impact the response variable of the trend in epilimnetic temperature. 

#My first step is to assess multicolinearity between the three variables using
#a scatterplot matrix

#Scatterplot Matrix:

#Renaming variables for easier calling

Mod_Temp <- Combined_Slopes$slope[Combined_Slopes$Analysis == "Mod_Temp"]
Mod_Strat_Dur <- Combined_Slopes$slope[Combined_Slopes$Analysis == "Mod_Strat_Dur"]
Temp_Epi <- Combined_Slopes$slope[Combined_Slopes$Analysis == "Temp_Epi"]
Mod_Schmidt <- Combined_Slopes$slope[Combined_Slopes$Analysis == "Mod_Strat_Schmidt"]
Ag_Plus_Dev <- Combined_Slopes$Ag_Plus_Dev[Combined_Slopes$Analysis == "Temp_Epi"]
Cult_Crop <- Combined_Slopes$Cult_Crop_Pct[Combined_Slopes$Analysis == "Temp_Epi"]
Total_Dev <- Combined_Slopes$Total_Dev_Pct[Combined_Slopes$Analysis == "Temp_Epi"]
Max_Depth <- Combined_Slopes$Max_Depth[Combined_Slopes$Analysis == "Temp_Epi"]
pairs(~ Mod_Temp + Mod_Strat_Dur)


# We also want to add correlation coefficients in the opposite panels so the data is not redundant:

panel.cor <- function(x, y, digits=2, cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = "spearman"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x, y)
  p_value_text <- paste0("p=", format(test$p.value, digits=digits))
  text(0.5, 0.5, paste("r=", txt, p_value_text))
}

pairs(~ Temp_Epi + Mod_Temp + Mod_Strat_Dur,
      lower.panel = panel.smooth, upper.panel = panel.cor) 

#There do not appear to be any incredibly strong correlations between the three variables,
#But Mod_Temp and Mod_Strat_Dur are almost significant

#Checking if an interaction would create a stable modle
vif(lm(Temp_Epi ~  Mod_Temp*Mod_Strat_Dur))

#Mod_Temp          Mod_Strat_Dur Mod_Temp:Mod_Strat_Dur 
#1.094651               5.957097               5.748028 

#Without the interaction:
vif(lm(Temp_Epi ~  Mod_Temp + Mod_Strat_Dur))

#Mod_Temp.       Mod_Strat_Dur 
#1.049655          1.049655

#No interaction has much lower VIF values so I think I will exclude this interaction

#################################################################################

#Now we need to test the linearity assumption: saying that the response variable, 
#in relations to all of the other variables held constant, if it is linear.


mod1 <- lm(Temp_Epi ~  Mod_Temp + Mod_Strat_Dur)


avPlots(mod1)

#I am convinced these two variables are linear without needing to transform them:

#Now we need to assess assumptions:

par(mfrow = c(2, 2))
plot(mod1)

#There are no outliers, the qqplot appears to look normal, and variance looks fairly equal
#But I could try to transform epilimnion temp

mod2 <- lm((Temp_Epi)^-1 ~  Mod_Temp + Mod_Strat_Dur)

avPlots(mod2)

#Still looks linear

par(mfrow = c(2, 2))
plot(mod2)

#An inverse transforation almost works for all assumptions, except the distribution does not appear normal

mod2b <- lm((Temp_Epi)^1/4 ~  Mod_Temp + Mod_Strat_Dur)

avPlots(mod2b)

#Still looks linear

par(mfrow = c(2, 2))
plot(mod2b)

#I think the untransformed data looks the best

summary(mod1)

#lm(formula = Temp_Epi ~ Mod_Temp + Mod_Strat_Dur)

#Residuals:
 # Min        1Q    Median        3Q       Max 
#-0.105493 -0.022133  0.003148  0.027720  0.169620 

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)
#(Intercept)   -0.015406   0.012746  -1.209    0.231
#Mod_Temp       0.895807   0.570837   1.569    0.121
#Mod_Strat_Dur  0.002019   0.026890   0.075    0.940

#Residual standard error: 0.04388 on 74 degrees of freedom
#Multiple R-squared:  0.03451,	Adjusted R-squared:  0.008411 
#F-statistic: 1.322 on 2 and 74 DF,  p-value: 0.2727

#It appears neither of the predictor variables are significant explanations of epilimnetic temperatures

################################################################################
################################################################################
################################################################################
################################################################################
#H1: temp(epi) = strat(time) + strat(strength)

pairs(~ Mod_Strat_Dur + Mod_Schmidt)


panel.cor <- function(x, y, digits=2, cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = "spearman"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x, y)
  p_value_text <- paste0("p=", format(test$p.value, digits=digits))
  text(0.5, 0.5, paste("r=", txt, p_value_text))
}

pairs(~ Temp_Epi + Mod_Strat_Dur + Mod_Schmidt,
      lower.panel = panel.smooth, upper.panel = panel.cor) 

# It seems as though these are correlated, because I already tested Duration I am just going to do a regression on stability 

mod3 <- lm(Temp_Epi ~ Mod_Schmidt)
par(mfrow = c(2,2))
plot(mod3)

#This appears to satisfy normality and equal variance

summary(mod3)

#Call:
 # lm(formula = Temp_Epi ~ Mod_Schmidt)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.108227 -0.023968  0.005316  0.024534  0.168333 

#Coefficients:
  #Estimate Std. Error t value Pr(>|t|)
#(Intercept) -7.340e-07  5.430e-03   0.000    1.000
#Mod_Schmidt  5.407e-05  6.269e-05   0.863    0.391

#Residual standard error: 0.04414 on 75 degrees of freedom
#Multiple R-squared:  0.009822,	Adjusted R-squared:  -0.003381 
#F-statistic: 0.7439 on 1 and 75 DF,  p-value: 0.3912

#Schmidt stability is not a significant predictor or epilimnion temperature


################################################################################
################################################################################
################################################################################
################################################################################
#Landuse Temp(epi) = ag_plus_dev + Cult_Crop + Total_Dev

pairs(~ Ag_Plus_Dev + Cult_Crop + Total_Dev)


panel.cor <- function(x, y, digits=2, cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = "spearman"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x, y)
  p_value_text <- paste0("p=", format(test$p.value, digits=digits))
  text(0.5, 0.5, paste("r=", txt, p_value_text))
}

pairs(~ Temp_Epi + Ag_Plus_Dev + Cult_Crop + Total_Dev,
      lower.panel = panel.smooth, upper.panel = panel.cor) 

#Removing Ag_Plus Dev

pairs(~ Temp_Epi + Cult_Crop + Total_Dev,
      lower.panel = panel.smooth, upper.panel = panel.cor)

vif(lm(Temp_Epi ~ Cult_Crop*Total_Dev))

#VIF calues see good
#Cult_Crop           Total_Dev Cult_Crop:Total_Dev 
#3.143824            1.566161            2.325356 

mod4 <- lm(Temp_Epi ~ Cult_Crop*Total_Dev)
avPlots(mod4)

#These seem convincingly linear

par(mfrow = c(2, 2))
plot(mod4)

#Also seems to be good for equal variance, normality, and no outliers


summary(mod4)
#
#Call:
#  lm(formula = Temp_Epi ~ Cult_Crop * Total_Dev)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.119904 -0.021221  0.005059  0.020561  0.172913 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)          6.499e-03  1.165e-02   0.558   0.5786  
#Cult_Crop           -2.187e-03  8.283e-04  -2.640   0.0101 *
#  Total_Dev            7.344e-05  1.887e-04   0.389   0.6982  
#Cult_Crop:Total_Dev  5.700e-05  3.633e-05   1.569   0.1210  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.04128 on 73 degrees of freedom
#Multiple R-squared:  0.1573,	Adjusted R-squared:  0.1227 
#F-statistic: 4.542 on 3 and 73 DF,  p-value: 0.005646

#It appears Agriculture is a significant predictor of epilimnetic temp trends


################################################################################
################################################################################
################################################################################
################################################################################
#Max Depth

scatter.smooth(Temp_Epi, Max_Depth, span = 2/3) #Not a strong correlation

mod5 <- lm(Temp_Epi ~ Max_Depth)
par(mfrow = c(2,2))
plot(mod5)

#Looks great for all assumptions

summary(mod5)

#Call:
#  lm(formula = Temp_Epi ~ Max_Depth)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.100521 -0.022677  0.004899  0.023227  0.157963 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept) -0.0262034  0.0126690  -2.068   0.0421 *
#  Max_Depth    0.0018373  0.0007683   2.391   0.0193 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.04276 on 75 degrees of freedom
#Multiple R-squared:  0.07084,	Adjusted R-squared:  0.05846 
#F-statistic: 5.718 on 1 and 75 DF,  p-value: 0.0193

#Max depth does appear to be a significant predictor.

