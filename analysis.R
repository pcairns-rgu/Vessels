#Author: Pauline Cairns  Date: 1 August 2019
#File set up to for data analysis
#This file can be ignored as does not feed into project

#set up of working directory 
setwd("H:/Vessels")

#all sourced from the packages install facility within RStudio
#in turn these are sourced from the cran repository 
#https://cran.r-project.org/ 
library(corrplot)
library(ggplot2)
library(lattice)
library(lubridate)

source('filter.R')

#check the class of recvtime
#class(montrose$RecvTime)
# POSIXct, POSTIxt
#class(montrose$length)
#class(montrose$width)

#fivenum summary
#summary(montrose)

#time interval 
time_interval_montrose <- montrose$RecvTime[1] %--% montrose$RecvTime[751]
time_interval_montrose
str(time_interval_montrose)
#53215 seconds
time_duration_montrose<- as.duration(time_interval_montrose)
time_duration_montrose
#53215s (~14.78 hours)
time_period_montrose<- as.period(time_interval_montrose) 
time_period_montrose
#14H 46M 55S
str(time_period_montrose)

lm_montrose <- lm(formula=montrose$SOG~montrose$COG, data=montrose)
summary(lm_montrose)

lm_montrose_2 <- lm(formula=montrose$COG~montrose$SOG, data=montrose)
summary(lm_montrose_2)

plot(x=montrose$SOG, y=montrose$COG, type="p", xlab="SOG", ylab="COG")
abline(lm_montrose_2)

plot(x=montrose$COG, y=montrose$SOG, type="p", xlab="COG", ylab="SOG")
abline(lm_montrose)

#count on y axis is the number of AIS feeds 
# recvtime_wday_montrose<- montrose%>% 
#   mutate(wday_name = wday(RecvTime, label = TRUE)) %>% 
#   ggplot(aes(x = wday_name)) +
#   geom_bar() +
#   labs(title="Port of Montrose", x="Day of week", y= "No of AIS feeds")
# recvtime_wday_montrose
#Thursday shows the highest volume

# recvtime_month_montrose<- montrose %>% 
#   mutate(month = month(RecvTime, label = TRUE)) %>% 
#   ggplot(aes(x = month)) +
#   geom_bar()
# recvtime_month_montrose

recvtime_year_montrose<- montrose %>% 
 # mutate(rec_year = year(RecvTime)) %>%
  ggplot(aes(x = year(RecvTime))) +
  geom_bar()
recvtime_year_montrose
#need to work out why not only 2016 and 2017 showing

#for no of vessels to be changed
#count on y axis is the number of AIS feeds 
# recvtime_wday_montrose<- montrose%>% 
#   ggplot(aes(x = wday(RecvTime, label = TRUE))) +
#   geom_bar() +
#   labs(title="Port of Montrose", x="Day of week", y= "No of AIS feeds")
# recvtime_wday_montrose
#Thursday shows the highest volume

#for splitting into years ie Nov 2016, Nov 2017
# recvtime_month_montrose<- montrose %>% 
#   ggplot(aes(x = month(RecvTime, label=TRUE))) +
#   geom_bar() +
#   labs(title="Port of Montrose", x="Month", y= "No of AIS feeds")
# recvtime_month_montrose

#should not need to remove all these rows - should work with
#something like montrose[, c(1,4,6) ] - now solved but code not updated 
montrose_alt<- montrose
montrose_alt$Name<- NULL
montrose_alt$location<- NULL
montrose_alt$description <- NULL
montrose_alt$RecvTime<- NULL
montrose_alt$OffPosition<-NULL
montrose_alt$Second<-NULL
montrose_alt$MessageID<- NULL
montrose_alt$MMSI <-NULL
montrose_alt$RAIM<-NULL
montrose_alt$Altitude<-NULL
montrose_alt$ShipType<- NULL
#remove nav status as essentially the number is replacing a phrase hence not
# a figure which has a value
montrose_alt$NavigationalStatus <- NULL


all_vessels_cor<- all_vessels[, -c(1,2,9:12)]
cor_all_ves<- cor(all_vessels_cor)
pairs(cor_all_ves)

ab_vessels_cor<- aberdeen[, -c(1,2,9:12)]
cor_ab_ves<- cor(ab_vessels_cor)
cor_ab_ves
pairs(cor_ab_ves)

cor_montrose <- cor(montrose_alt)
cor_montrose
#need to check - below which value is it considered that 
#one figures is impacting on the other
corrplot(cor_montrose)
pairs(cor_montrose)
#splom(cor_montrose)

#linear regression
lm_montrose_3 <- lm(formula=montrose_alt$COG~., data=montrose_alt)
summary(lm_montrose_3)






