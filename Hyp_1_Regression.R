#Multiple Linear Regression to test drivers of increasing epilimnetic temperatures

library(sensemakr)
library(car)
library(carData)

#Dataframe dependent for the MLR generated from the Master_Slopes_Script
#Master_Slopes 1174 obs of 33 variables

#The first hypothesis to test is how the trends for summer mean temp and stratification duration
#impact the response variable of the trend in epilimnetic temperature. 

#My first step is to assess multicolinearity between the three variables using
#a scatterplot matrix

#Scatterplot Matrix:

#Renaming variables for easier calling

Mod_Temp <- Master_Slopes$slope[Master_Slopes$Analysis == "Mod_Temp"]
Mod_Strat_Dur <- Master_Slopes$slope[Master_Slopes$Analysis == "Mod_Strat_Dur"]
Temp_Epi <- Master_Slopes$slope[Master_Slopes$Analysis == "Temp_Epi"]

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

#
#lm(formula = Temp_Epi ~ Mod_Temp + Mod_Strat_Dur)
#
#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.107573 -0.024918  0.009349  0.026914  0.159241 

#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
#(Intercept)   -0.007463   0.012804  -0.583    0.562
#Mod_Temp       0.597741   0.573434   1.042    0.301
#Mod_Strat_Dur  0.005633   0.027012   0.209    0.835

#Residual standard error: 0.04408 on 74 degrees of freedom
#Multiple R-squared:  0.01707,	Adjusted R-squared:  -0.009492 
#F-statistic: 0.6427 on 2 and 74 DF,  p-value: 0.5288

#It appears neither of the predictor variables are significant explanations of epilimnetic temperatures



