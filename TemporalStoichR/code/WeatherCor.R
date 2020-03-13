# Author: Isabella Richmond 
# Last edited: March 13, 2020

# Code for testing the correlation in weather data collected from Environment & Climate Change 
# Canada's weather stations in Lethbridge, Clarenville, and Charleston (NL, Canada)
# Weather data is being used to calculate growing degree days for my temporal stoich models 
# Some gaps in data at the Lethbridge weather station 
# Testing to see if there is a highly correlated relationship (r > 0.90) in the weather data 
# between Lethbridge and Clarenville/Charleston 
# If the relationship is highly correlated, missing Lethbridge weather data will be supplemented 
# with Clarenville and Charleston data (whichever exists and if both were recorded on a day,
# the town with the highest correlation will be used)

# load packages 
library(easypackages)
easypackages::libraries("readr", "dplyr", "plyr", "tibble", "ggplot2", "tidyr")

# load datasets 
clarenville2016 <- read_csv("input/WeatherCor/Clarenville_2016_R.csv")
head(clarenville2016)
charleston2016 <- read_csv("input/WeatherCor/Charleston_2016_R.csv")
head(charleston2016)
lethbridge2016 <- read_csv("input/WeatherCor/Lethbridge_2016_R.csv")
head(lethbridge2016)
# sort datasets by date and time 
clarenville2016 <- arrange(clarenville2016, .by_group = clarenville2016$`Date_Time`)
charleston2016 <- arrange(charleston2016, .by_group = charleston2016$Date_Time)
lethbridge2016 <- arrange(lethbridge2016, .by_group = lethbridge2016$Date_Time)
# add column that identifies the town
clarenville2016 <- add_column(clarenville2016, "Station" = "Clarenville")
charleston2016 <- add_column(charleston2016, "Station" = "Charleston")
lethbridge2016 <- add_column(lethbridge2016, "Station" = "Lethbridge")

# correlations between Clarenville and Lethbridge
# combine town dataframes
clarenleth <- rbind(clarenville2016, lethbridge2016)
# mean temperature
# evlauate correlation
clmean <- cor.test(clarenville2016$MeanTemp, lethbridge2016$MeanTemp)
# plot relationship
ggplot(clarenleth, aes(x=Date_Time, y=MeanTemp, color=Station)) + geom_point()+
  labs(title = "Clarenville v Lethbridge Mean Temp", x = "Date", y = "Mean Temp degC")+
  annotate("text", x=as.Date("2016-09-30"), y=-13, label = "r = ")+
  annotate("text", x =as.Date("2016-12-31"), y = -13,label = clmean$estimate)
ggsave("graphics/WeatherCor/clarenlethmean.png")

# minimum temperature 
clmin <- cor.test(clarenville2016$MinTemp, lethbridge2016$MinTemp)
ggplot(clarenleth, aes(x=Date_Time, y=MinTemp, color=Station)) + geom_point()+
  labs(title = "Clarenville v Lethbridge Minimum Temp", x = "Date", y = "Min Temp degC")+
  annotate("text", x=as.Date("2016-09-30"), y=-21, label = "r = ")+
  annotate("text", x =as.Date("2016-12-31"), y = -21,label = clmin$estimate)
ggsave("graphics/WeatherCor/clarenlethmin.png")

# maximum temperature
clmax <- cor.test(clarenville2016$MaxTemp, lethbridge2016$MaxTemp)
ggplot(clarenleth, aes(x=Date_Time, y=MinTemp, color=Station)) + geom_point()+
  labs(title = "Clarenville v Lethbridge Maximum Temp", x = "Date", y = "Max Temp degC")+
  annotate("text", x=as.Date("2016-09-30"), y=-21, label = "r = ")+
  annotate("text", x =as.Date("2016-12-31"), y = -21,label = clmax$estimate)
ggsave("graphics/WeatherCor/clarenlethmax.png")

# correlations between Charleston and Lethbridge
# combine town dataframes
charlesleth <- rbind(charleston2016, lethbridge2016)
# mean temperature
# evlauate correlation
chlmean <- cor.test(charleston2016$MeanTemp, lethbridge2016$MeanTemp)
# plot relationship
ggplot(charlesleth, aes(x=Date_Time, y=MeanTemp, color=Station)) + geom_point()+
  labs(title = "Charleston v Lethbridge Mean Temp", x = "Date", y = "Mean Temp degC")+
  annotate("text", x=as.Date("2016-09-30"), y=-14, label = "r = ")+
  annotate("text", x =as.Date("2016-12-31"), y = -14,label = chlmean$estimate)
ggsave("graphics/WeatherCor/charleslethmean.png")

# minimum temperature 
chlmin <- cor.test(charleston2016$MinTemp, lethbridge2016$MinTemp)
ggplot(charlesleth, aes(x=Date_Time, y=MinTemp, color=Station)) + geom_point()+
  labs(title = "Charleston v Lethbridge Minimum Temp", x = "Date", y = "Min Temp degC")+
  annotate("text", x=as.Date("2016-09-30"), y=-21, label = "r = ")+
  annotate("text", x =as.Date("2016-12-31"), y = -21,label = chlmin$estimate)
ggsave("graphics/WeatherCor/charleslethmin.png")

# maximum temperature
chlmax <- cor.test(charleston2016$MaxTemp, lethbridge2016$MaxTemp)
ggplot(charlesleth, aes(x=Date_Time, y=MinTemp, color=Station)) + geom_point()+
  labs(title = "Charleston v Lethbridge Maximum Temp", x = "Date", y = "Max Temp degC")+
  annotate("text", x=as.Date("2016-09-30"), y=-21, label = "r = ")+
  annotate("text", x =as.Date("2016-12-31"), y = -21,label = chlmax$estimate)
ggsave("graphics/WeatherCor/charleslethmax.png")

# Clarenville consistently had a higher correlation with Lethbridge weather than Charleston 
# replace Lethbridge NAs with data from the Clarenville weather station
# NOTE: replacement was done in Excel - will add in code later