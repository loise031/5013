## Starting with the Comb_Lakes_2 script produced by Comb_Lakes_Script and start 
## of Individual_Lake_Analysis script.
## Going to average epi temp and hypo temp by year, then plot epi temp vs hypo 
## temp with a point for each year, then do the same for DO con and DO sat.
## Hypotheses:
## 1) Annual average epi and hypo temps are significantly inversely related
## 2) Annual average epi and hypo DO cons are significantly inversely related
## 3) Annual average epi and hypo DO sats are significantly inversely related

## TEMPERATURE ##
## Comb_Lakes_Epi made from Comb_Lakes_2
epitemp_annual_avg <- tapply(Comb_Lakes_Epi$Temperature, Comb_Lakes_Epi$Year, mean)
epitemp_annual_avg_df <- data.frame(epitemp_annual_avg)
epitemp_annual_avg_df$Year <- rownames(epitemp_annual_avg_df)

## Comb_Lakes_Hypo made from Comb_Lakes_2
hypotemp_annual_avg <- tapply(Comb_Lakes_Hypo$Temperature, Comb_Lakes_Hypo$Year, mean)
hypotemp_annual_avg_df <- data.frame(hypotemp_annual_avg)
hypotemp_annual_avg_df$Year <- rownames(hypotemp_annual_avg_df)

## Make df with year, epi temp, and hypo temp
temp_annual_avg <- data.frame(epitemp_annual_avg_df$Year, epitemp_annual_avg_df$epitemp_annual_avg, hypotemp_annual_avg_df$hypotemp_annual_avg)
colnames(temp_annual_avg) [1] <- "Year"
colnames(temp_annual_avg) [2] <- "epitemp"
colnames(temp_annual_avg) [3] <- "hypotemp"

## LM of epitemp vs hypotemp
temp_lm <- lm(temp_annual_avg$hypotemp ~ temp_annual_avg$epitemp)
print(temp_lm)
summary(temp_lm)

## Plot
#library(ggplot2)
#temp_annual_avg_plot <- ggplot(data = temp_annual_avg, mapping = aes(x = epitemp, y = hypotemp)) +
 # geom_point()
plot(temp_annual_avg$epitemp, temp_annual_avg$hypotemp, xlab = "Annual avg epilimnion temp (C)", ylab = "Annual avg hypolimnion temp (C)")+
  abline(temp_lm)+
  title('Annual Avg Epi vs Hypo Temp
slope = 0.15, p = 0.50, R2 = 0.01')

## DO CON ##
## Comb_Lakes_Epi made from Comb_Lakes_2
epidocon_annual_avg <- tapply(Comb_Lakes_Epi$DO_Con, Comb_Lakes_Epi$Year, mean)
epidocon_annual_avg_df <- data.frame(epidocon_annual_avg)
epidocon_annual_avg_df$Year <- rownames(epidocon_annual_avg_df)

## Comb_Lakes_Hypo made from Comb_Lakes_2
hypodocon_annual_avg <- tapply(Comb_Lakes_Hypo$DO_Con, Comb_Lakes_Hypo$Year, mean)
hypodocon_annual_avg_df <- data.frame(hypodocon_annual_avg)
hypodocon_annual_avg_df$Year <- rownames(hypodocon_annual_avg_df)

## Make df with year, epi docon, and hypo docon
docon_annual_avg <- data.frame(epidocon_annual_avg_df$Year, epidocon_annual_avg_df$epidocon_annual_avg, hypodocon_annual_avg_df$hypodocon_annual_avg)
colnames(docon_annual_avg) [1] <- "Year"
colnames(docon_annual_avg) [2] <- "epidocon"
colnames(docon_annual_avg) [3] <- "hypodocon"

## LM of epidocon vs hypodocon
docon_lm <- lm(docon_annual_avg$hypodocon ~ docon_annual_avg$epidocon)
print(docon_lm)
summary(docon_lm)

## Plot
plot(docon_annual_avg$epidocon, docon_annual_avg$hypodocon, xlab = "Annual avg epilimnion DO Con (mg/L)", ylab = "Annual avg hypolimnion DO Con (mg/L)")+
  abline(docon_lm) +
  title('Annual Avg Epi vs Hypo DO Con
slope = -0.36, p = 0.29, R2 = 0.02')

## DO SAT ##
## Comb_Lakes_Epi made from Comb_Lakes_2
epidosat_annual_avg <- tapply(Comb_Lakes_Epi$DO_Sat, Comb_Lakes_Epi$Year, mean)
epidosat_annual_avg_df <- data.frame(epidosat_annual_avg)
epidosat_annual_avg_df$Year <- rownames(epidosat_annual_avg_df)

## Comb_Lakes_Hypo made from Comb_Lakes_2
hypodosat_annual_avg <- tapply(Comb_Lakes_Hypo$DO_Sat, Comb_Lakes_Hypo$Year, mean)
hypodosat_annual_avg_df <- data.frame(hypodosat_annual_avg)
hypodosat_annual_avg_df$Year <- rownames(hypodosat_annual_avg_df)

## Make df with year, epi dosat, and hypo dosat
dosat_annual_avg <- data.frame(epidosat_annual_avg_df$Year, epidosat_annual_avg_df$epidosat_annual_avg, hypodosat_annual_avg_df$hypodosat_annual_avg)
colnames(dosat_annual_avg) [1] <- "Year"
colnames(dosat_annual_avg) [2] <- "epidosat"
colnames(dosat_annual_avg) [3] <- "hypodosat"

## LM of epidosat vs hypodosat
dosat_lm <- lm(dosat_annual_avg$hypodosat ~ dosat_annual_avg$epidosat)
print(dosat_lm)
summary(dosat_lm)

## Plot
plot(dosat_annual_avg$epidosat, dosat_annual_avg$hypodosat, xlab = "Annual avg epilimnion DO % sat", ylab = "Annual avg hypolimnion DO % sat")+
  abline(dosat_lm) +
  title('Annual Avg Epi vs Hypo DO Sat
slope = -0.07, p = 0.73, R2 = 0.00')

##LM of latitude vs depth from master_slopes
lat <-as.numeric(masterslopes_epi_temp$Latitude)
zmax <- as.numeric(masterslopes_epi_temp$Max_Depth)
lm_latdepth <- lm(masterslopes_epi_temp$Max_Depth ~ masterslopes_epi_temp$Latitude)
print(lm_latdepth)
summary(lm_latdepth)
plot(lat, zmax)+
  abline(lm_latdepth)
   ##negative but not significant

##LM of latitude vs ag from master_slopes
lat <-as.numeric(masterslopes_epi_temp$Latitude)
aglat <- as.numeric(masterslopes_epi_temp$Cult_Crop_Pct)
lm_latag <- lm(masterslopes_epi_temp$Cult_Crop_Pct ~ masterslopes_epi_temp$Latitude)
print(lm_latag)
summary(lm_latag)
plot(lat, aglat)+
  abline(lm_latag)+
  abline(lm_latdev)
##negative AND significant !!!!!!

##LM of latitude vs dev from master_slopes
lat <-as.numeric(masterslopes_epi_temp$Latitude)
devlat <- as.numeric(masterslopes_epi_temp$Total_Dev_Pct)
lm_latdev <- lm(masterslopes_epi_temp$Total_Dev_Pct ~ masterslopes_epi_temp$Latitude)
print(lm_latdev)
summary(lm_latdev)
plot(lat, devlat)+
  abline(lm_latdev)
##similar relationship as aglat but not significant

##LM of zmax vs dev from master_slopes
zmax <-as.numeric(masterslopes_epi_temp$Max_Depth)
zdev <- as.numeric(masterslopes_epi_temp$Total_Dev_Pct)
lm_zdev <- lm(zdev ~ zmax)
print(lm_zdev)
summary(lm_zdev)
plot(zdev, zmax)+
  abline(lm_zdev)
##not significant

##LM of zmax vs ag from master_slopes
zag <- as.numeric(masterslopes_epi_temp$Cult_Crop_Pct)
lm_zag <- lm(zag ~ zmax)
print(lm_zag)
summary(lm_zag)
plot(zag, zmax)+
  abline(lm_zag)
##not significan
t
##checking means of epi temp slopes by state
mn <- subset(masterslopes_epi_temp, State == "MN")
mi <- subset(masterslopes_epi_temp, State == "MI")
wi <- subset(masterslopes_epi_temp, State == "WI")
mean(mn$slope)
mean(mi$slope)
mean(wi$slope)
sd(mn$slope)
sd(mi$slope)
sd(wi$slope)

library(ggplot2)
state_epitemp_means <- data.frame(
  name = c("mn", "mi", "wi"),
  value = c(-0.00044, 0.0128, 0.031),
  sd = c(0.04, 0.057, 0.0099)
)

ggplot(state_epitemp_means)+
  geom_bar(aes(x=name, y=value), stat = "identity", fill = 'skyblue', alpha = 0.7)+
  geom_errorbar(aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, color = "orange", alpha = 0.9, size = 1.3)

ggplot(masterslopes_epi_temp, mapping = aes(State, slope)+
         geom_violin())
vioplot(mn$slope, mi$slope, wi$slope)
install.packages("vioplot")
library(vioplot)

##checking means of hypo temp slopes by state
mnh <- subset(masterslopes_hypo_temp, State == "MN")
mih<- subset(masterslopes_hypo_temp, State == "MI")
wih <- subset(masterslopes_hypo_temp, State == "WI")
mean(mnh$slope)
mean(mih$slope)
mean(wih$slope)
sd(mnh$slope)
sd(mih$slope)
sd(wih$slope)

vioplot(mnh$slope, mih$slope, wih$slope)

##checking means of layer trend slopes by parameter
epitemp <- subset(masterslopes_epi_temp, State == "MN")
mih<- subset(masterslopes_hypo_temp, State == "MI")
wih <- subset(masterslopes_hypo_temp, State == "WI")
library(vioplot)

##violin plots (1 per lyr) by param
epivio <- vioplot(masterslopes_epi_temp$slope, masterslopes_epi_docon$slope, masterslopes_epi_dosat$slope,
        xlab = c("Parameter"), ylab = "Annual Trend", col = "palevioletred3", names = c("Temperature", "DO Con", "DO Sat"))

hypovio <- vioplot(masterslopes_hypo_temp$slope, masterslopes_hypo_docon$slope, masterslopes_hypo_dosat$slope,
        xlab = c("Parameter"), ylab = "Annual Trend", col = "cyan3", names = c("Temperature", "DO Con", "DO Sat"))

install.packages("gridExtra")
library(gridExtra)
grid.arrange(epivio, hypovio, ncol =2)
