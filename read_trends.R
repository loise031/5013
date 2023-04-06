#script to look for trends in REed modelled data
#H Rantala
# April 5, 2023

#load packages needed
library(tidyverse)
library(lubridate)        #Allows me to extract a month out of my POSIX value 
library(openair)
library(standarize)
library(trend)

#load files
setwd("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/data/limno5013")

jr1<-read.csv("Read.combined.csv",sep=",")
jr1$yrx<-paste("x",jr1$year,sep="")
#need to reorganize
jr2<-jr1%>%select(site_id,yrx,mean_surf_aug)%>%
  pivot_wider(names_from = yrx, values_from = mean_surf_aug)

jr3<-jr1%>%select(site_id:stratification_duration)%>%arrange(year)
jr3$date<-ymd_hms(paste(jr3$year,"-01-01 00:00:00",sep=""))

lakes<-as.data.frame(unique(jr3$site_id)) #looks like we have 5938 lakes in this dataset!
lakes$ID<-c(1:5938)

lakes2<-sample(length(lakes$ID),50)#randomly selects lakes based on ID. Here is where you set the number of subsampled lakes.
#run it, and go get a giant coffee
lakes3<-lakes%>%filter(ID%in%lakes2)
lakes3$site_id<-lakes3$'unique(jr3$site_id)'

jr4<-jr3%>%filter(site_id%in%lakes3$site_id)

TheilSen(jr4,pollutant = "peak_temp",ave.time="year")

jr3$peak_tempSC<-scale_by(peak_temp ~ site_id, jr3) 

#plot the estimated values verus year
ggplot(jr3, aes(year, peak_temp))+
  geom_point()+
  geom_smooth()

#plot the scaled data by year
ggplot(jr3, aes(year, peak_tempSC))+
  geom_point(color="steelblue")+
  geom_smooth()+
  theme_classic()+
  labs(y="Scaled peak temp")

#calculate individual lake trend lines
#create empty dataframe for output form Theil Sen
SensMod<- data.frame(matrix(0, (5950), 7))###create empty dataframe for g, quartiles
names(SensMod) <- c("site_id","parameter","z_score","p_value", "L95","U95","Sensslope")

h=1 #this is my counter

for(j in 1:nrow(lakes)){
  df1<-filter(lakes, ID==h)
df2<-jr1%>%filter(site_id==df1$`unique(jr3$site_id)`)%>%
  arrange(year)
m1<-sens.slope(df2$mean_surf_aug) #select variable in this step


SensMod[h,1]<-unique(df2$site_id)
SensMod[h,2]<-m1$data.name
SensMod[h,3]<-m1$statistic
SensMod[h,4]<-m1$p.value
SensMod[h,5:6]<-m1$conf.int
SensMod[h,7]<-m1$estimates

h=h+1
}

SensMod<-SensMod%>%filter(site_id!=0)

#write.csv(SensMod, "peak_temp_allLakes.csv")

ggplot(SensMod, aes(Sensslope))+
  geom_histogram(fill="lightblue2", color="black")+
  theme_classic()+
  geom_vline(xintercept = 0, color="turquoise3", linetype="dashed")
