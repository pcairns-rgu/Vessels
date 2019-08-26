#Author: Pauline Cairns  Date: 1 August 2019
#Workings to understand if a vessel is stopped or not
#set up of working directory 
setwd("H:/Vessels")

#to allow queries to be run on the database
source('filter.R')

#all sourced from the packages install facility within RStudio
#in turn these are sourced from the cran repository 
#https://cran.r-project.org/ 
library(zoo)
library(xts)

#initial analysis using only vessels in montrose_extended area
#reduce data frame to only columns potentially required
full_AIS_alt<- montrose_extended[, c(1,2,4,5,6,9,3,7,8,10,11,12)]

#select vessel and date period - currently showing EDT Hercules
full_AIS_alt<- filter(full_AIS_alt, full_AIS_alt$MMSI==210776000, full_AIS_alt$RecvTime>=as.Date("2017-08-01"))

#set up new column and set all values to FALSE
full_AIS_alt$stopped<-FALSE

#simple moving average calc using rollmean function, k is rows in mean calc,
full_AIS_alt$mean_latitude<-rollmean(full_AIS_alt$Latitude, k=5, fill=NA, align="right")
full_AIS_alt$mean_longitude<-rollmean(full_AIS_alt$Longitude, k=5, fill=NA, align="right")

#reorder columns in data frame to put relevant columns near each other
full_AIS_alt<- full_AIS_alt[, c(1:6, 13:15, 7:12)]

#set distance in degrees
distance<- 0.01

#compare meanlong, meanlat, long, lat diff with distance
for(j in 1:length(full_AIS_alt$Latitude)){
  mean_total<-full_AIS_alt$mean_longitude[j] +full_AIS_alt$mean_latitude[j]
  total<-full_AIS_alt$Longitude[j]+full_AIS_alt$Latitude[j]
  diff<-sum(mean_total,-total, na.rm=TRUE)
  if(diff< distance){
    full_AIS_alt$stopped[j]<- TRUE
  }

}

#summary table of nav stat and stopped status
diff_nav<- data.frame(full_AIS_alt$NavigationalStatus, full_AIS_alt$stopped)
diff_nav_summary<- diff_nav %>% group_by(full_AIS_alt.NavigationalStatus, full_AIS_alt$stopped)%>%summarise(total =n()) 

#summary table of nav stat, speed and stopped status
diff_nav1<- data.frame(full_AIS_alt$NavigationalStatus, full_AIS_alt$stopped, full_AIS_alt$SOG, full_AIS_alt$stopped)
diff_nav_summary1<- diff_nav1 %>% group_by(full_AIS_alt.NavigationalStatus, full_AIS_alt.SOG, full_AIS_alt$stopped)%>% 
 summarise(total =n()) 



#some of vessels used in results comparision
#Interceptor
#full_AIS_alt<- filter(full_AIS_alt, full_AIS_alt$MMSI==232009412, (full_AIS_alt$RecvTime>=as.Date("2017-07-14") & full_AIS_alt$RecvTime<=as.Date("2017-07-18")) )
#Arundo
#full_AIS_alt<- filter(full_AIS_alt, full_AIS_alt$MMSI==375607000, (full_AIS_alt$RecvTime>=as.Date("2017-09-16") & full_AIS_alt$RecvTime<=as.Date("2017-09-19")))
#Argos
#full_AIS_alt<- filter(full_AIS_alt, full_AIS_alt$MMSI==305786000, (full_AIS_alt$RecvTime>=as.Date("2017-08-01") & full_AIS_alt$RecvTime<=as.Date("2017-08-04")))


