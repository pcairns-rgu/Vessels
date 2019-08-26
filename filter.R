#Author: Pauline Cairns  Date: 1 August 2019
#Create dataframes from database connection and 
#then filters out records from dataframes

#set up of working directory 
setwd("H:/Vessels")

#to allow queries to be run on the database
source('connect.R')

#import packages
#all sourced from the packages install facility within RStudio
#in turn these are sourced from the cran repository 
#https://cran.r-project.org/ 
library(dplyr)
library(tidyr)
library(stringr)
library(leaflet)
library(lubridate)


#Once this script has been run once, the next 5 connect commands 
#below can be hashed out to speed up app load

#Obtain all vessel details from database
#vessels_download<- connect_vessels$find('{}')

#Obtain vessels in extended area from database
#Guidance provided by https://jeroen.github.io/mongolite/query-data.html
#montrose_extended_vessels<- connect_positions$find('{"Latitude": {"$gt":56.651 , "$lt":56.758}, "Longitude": {"$gt": -2.476 , "$lt":-2.367}} ')
#peterhead_extended_vessels<- connect_positions$find('{"Latitude": {"$gt":57.442 , "$lt":57.542}, "Longitude": {"$gt": -1.795 , "$lt":-1.695}} ')
#aberdeen_extended_vessels<- connect_positions$find('{"Latitude": {"$gt":57.089 , "$lt":57.189}, "Longitude": {"$gt": -2.096 , "$lt":-1.947}} ')
#all_vessels_collected<- connect_positions$find('{"Latitude": {"$gt":56.45 , "$lt":58}, "Longitude": {"$gt": -2.9 , "$lt":-1.4}, "RecvTime": {"$gte":{"$date":"2017-06-01T00:00:20.000Z"}, "$lte":{"$date":"2017-06-14T00:00:20.000Z"}}}')

#inspect summary contents
#summary(vessels_download)

#remove columns not required
#CallSign, Dimension_A, Dimension_B,Dimension_C, Dimension_D,
#pslow, count, IMO, PositionFixingDevice, MaxDraught, DTE, 
#AtoNType, VirtualAtoN, AssignedMode, AISVersion
vessels<- vessels_download[, c(3:4, 16, 21)]
vessels<- vessels%>% replace_na(list(Name="Unknown"))
vessels<- vessels%>% replace_na(list(description="Unknown"))

#Create harbour area from extended area 
montrose_vessels<- montrose_extended_vessels %>% filter(between(Latitude, 56.701, 56.708), between(Longitude, -2.476, -2.413))
peterhead_vessels<- peterhead_extended_vessels %>% filter(between(Latitude, 57.472, 57.512), between(Longitude, -1.795, -1.735))
aberdeen_vessels<- aberdeen_extended_vessels %>% filter(between(Latitude,57.101,57.176), between(Longitude, -2.096, -1.978))

#join vessels dataframe with positions dataframe to show names
montrose<- montrose_vessels %>% left_join(vessels, by=c('MMSI'= 'MMSI'))
aberdeen<- aberdeen_vessels %>% left_join(vessels, by=c('MMSI'= 'MMSI'))
peterhead<- peterhead_vessels %>% left_join(vessels, by=c('MMSI'= 'MMSI'))
montrose_extended<- montrose_extended_vessels %>% left_join(vessels, by=c('MMSI'= 'MMSI'))
aberdeen_extended<- aberdeen_extended_vessels %>% left_join(vessels, by=c('MMSI'= 'MMSI'))
peterhead_extended<- peterhead_extended_vessels %>% left_join(vessels, by=c('MMSI'= 'MMSI'))
all_vessels<- all_vessels_collected %>% left_join(vessels, by=c('MMSI'= 'MMSI'))



#ShipType pulling through as a number - change to character
montrose$ShipType<- as.character(montrose$ShipType)
aberdeen$ShipType<- as.character(aberdeen$ShipType)
peterhead$ShipType<- as.character(peterhead$ShipType)
montrose_extended$ShipType<- as.character(montrose_extended$ShipType)
aberdeen_extended$ShipType<- as.character(aberdeen_extended$ShipType)
peterhead_extended$ShipType<- as.character(peterhead_extended$ShipType)
all_vessels$ShipType<- as.character(all_vessels$ShipType)

#check contents
#look at first 6 rows of dataset
#head(all_vessels)
#look at fivenum summary
#summary(all_vessels)
#look at type of each feature
#str(all_vessels)

#Remove columns not used in MessageID, PositionAccuracy, Second, RAIM, OffPostion, Altitude
montrose<-montrose[, -c(2,6,11:14, 16)]
peterhead<- peterhead[, -c(2,6,11:14,16)]
aberdeen<- aberdeen[, -c(2,6,11:14,16) ]
montrose_extended<-montrose_extended[, -c(2,6,11:14, 16)]
peterhead_extended<- peterhead_extended[, -c(2,6,11:14,16)]
aberdeen_extended<- aberdeen_extended[, -c(2,6,11:14,16) ]
all_vessels<-all_vessels[, -c(2,6,11:14,16)]

