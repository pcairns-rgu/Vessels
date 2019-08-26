#Author: Pauline Cairns  Date: 1 August 2019
#File set up to connect to the AIS Mongodb database

#all sourced from the packages install facility within RStudio
#in turn these are sourced from the cran repository 
#https://cran.r-project.org/ 
library(devtools)
library(mongolite)

#mongolite used to connect to mongodb database
#Guidance provided by:
#https://jeroen.github.io/mongolite/connecting-to-mongodb.html#mongo-uri-format
#https://jeroen.github.io/mongolite/query-data.html
#There are four collections in the database - currently
#only connecting to two for use in this project

#Connect to vessel positions collection
connect_positions<- mongo("positions", url = "mongodb://USERNAME:PASSWORD@SERVER/ais")

#Connect to vessel names collection
connect_vessels<- mongo("vessels", url = "mongodb://USERNAME:PASSWORD@SERVER/ais")

#Check connection works by counting records and comparing 
#to that in DB. Guidance provided by:
#https://jeroen.github.io/mongolite/query-data.html

#connect_positions$count('{}')
#gives 42732347 entries

#connect_vessels$count('{}')
#gives 15756

