# Author: Isabella Richmond 
# Last edited: October 14, 2020

# Code for combining weather data from 2016 and 2017 at both weather stations - Terra Nova National Park and 
# Lethbridge. 2016 Lethbridge data was supplemented with Clarenville data (see 1 - WeatherCor.R)
# Weather data was obtained from Environment & Climate Change Canada's historical datasets. 
# Growing Degree Days were calculated as per McMaster & Wilhelm (method 1) using 5 deg. Celsius as a base temperature 
# as per Brown et al. 2019.

# load packages 
library(easypackages)
easypackages::libraries("tidyverse", "lubridate")

#### Data Preparation ####
# load datasets 
l2016 <- read_csv("input/GDD/Lethbridge_2016_full_R.csv")
l2017 <- read_csv("input/GDD/Lethbridge_2017_R.csv")
tn2016 <- read_csv("input/GDD/TNNP_2016_R.csv")
tn2017 <- read_csv("input/GDD/TNNP_2017_R.csv")

# drop NAs in TN 2017 temperature data (all in January - OK that they are missing)
l2017 <- drop_na(l2017, MeanTemp)
tn2017 <- drop_na(tn2017, MeanTemp)

#### Growing Degree Days ####
# write a growing degree day function - modified from function made by https://github.com/karawoo/cbccy
gdd <- function(dates, maxtemp, mintemp, meantemp, temp.base=5){
  # dates where mean temp < temp.base = 0
  # can't be lower than 0 or higher than 30
  gdd = if_else(meantemp < 5, 0,pmax(pmin((maxtemp+mintemp)/2-temp.base,30),0))
  df = data.frame(dates,maxtemp, mintemp, meantemp, gdd)
  dfcum = dplyr::mutate(df, gdd.cum = cumsum(gdd))$gdd.cum
  
  
}

l2016gdd <- gdd(l2016$Date_Time, l2016$MaxTemp, l2016$MinTemp, l2016$MeanTemp)
l2016 <- add_column(l2016, GDD = l2016gdd)

l2017gdd <- gdd(l2017$Date_Time, l2017$MaxTemp, l2017$MinTemp, l2017$MeanTemp)
l2017 <- add_column(l2017, GDD = l2017gdd)

tn2016gdd <- gdd(tn2016$Date_Time, tn2016$MaxTemp, tn2016$MinTemp, tn2016$MeanTemp)
tn2016 <- add_column(tn2016, GDD = tn2016gdd)

tn2017gdd <- gdd(tn2017$Date_Time, tn2017$MaxTemp, tn2017$MinTemp, tn2017$MeanTemp)
tn2017 <- add_column(tn2017, GDD = tn2017gdd)

# remove extra column in l2016 
l2016 <- subset(l2016, select = -c(X1))
# add identifying columns to each 
l2017 <- add_column(l2017, Station = "Lethbridge")
tn2016 <- add_column(tn2016, Station = "TNNP")
tn2017 <- add_column(tn2017, Station = "TNNP")
# convert dates to actual dates 
l2016$Date_Time <- as_date(l2016$Date_Time)
l2017$Date_Time <- as_date(l2017$Date_Time)
tn2016$Date_Time <- as_date(tn2016$Date_Time)
tn2017$Date_Time <- as_date(tn2017$Date_Time)
# merge datasets 
all <- rbind(l2016, l2017, tn2016, tn2017)
# save csv 
write.csv(all, "input/Weather_2016_2017_R.csv")


#### Sampling Days ####
# now we are going to extract the GDD value for each day of sampling for every measurement of elemental 
# composition we have across the four sites and study species
# load in sampling dates for all sites in 2016 and 2017 
s2016 <- read_csv("input/GDD/Stoich_Height_2016.csv")
s2017 <- read_csv("input/GDD/Stoich_Height_2017.csv")
# we only care about the four study species, subset to remove others 
s2016 <- filter(s2016, Species=="ABBA" | Species == "ACRU" | Species == "BEPA" | Species == "VAAN")
s2017 <- filter(s2017, Species=="ABBA" | Species == "ACRU" | Species == "BEPA" | Species == "VAAN")


# we want to extract the GDD on the day of sampling for each sample. Lethbridge weather station is used 
# for Bloomfield study site and TNNP weather station is used for Unicorn, TNNP, and Dunphys Pond study sites 
# simplify dataframes for easier extraction 
s2016 <- subset(s2016, select = c(TimeStamp, SiteName, TrapLocation, PlotName, Species))
s2017 <- subset(s2017, select = c(TimeStamp, SiteName, TrapLocation, PlotName, Species))

# convert timestamps to actual dates and times in R 
s2016$TimeStamp <- as_datetime(s2016$TimeStamp, format="%F %H:%M")
s2017$TimeStamp <- as_datetime(s2017$TimeStamp, format="%F %H:%M")

# repeated observations in the dataset - remove 
s2016 <- s2016 %>% 
  group_by(SiteName, TrapLocation, Species) %>% 
  filter(row_number()==1)

s2017 <- s2017 %>% 
  group_by(SiteName, TrapLocation, Species) %>% 
  filter(row_number()==1)

# extract the growing degree days for each of these observations 
# subset Bloomfield because it has a different weather station than the rest 
bl2016 <- filter(s2016, SiteName=="Bloomfield")
bl2016 <- add_column(bl2016, Date_Time = date(bl2016$TimeStamp))
bl2016 <- inner_join(bl2016, l2016, by="Date_Time")

bl2017 <- filter(s2017, SiteName=="Bloomfield")
bl2017 <- add_column(bl2017, Date_Time = date(bl2017$TimeStamp))
bl2017 <- inner_join(bl2017, l2017, by="Date_Time")

tnundp2016 <- filter(s2016, SiteName=="Dunphys Pond" | SiteName=="TNNP North" | SiteName=="Unicorn")
tnundp2016 <- add_column(tnundp2016, Date_Time = date(tnundp2016$TimeStamp))
tnundp2016 <- inner_join(tnundp2016, tn2016, by="Date_Time")

tnundp2017 <- filter(s2017, SiteName=="Dunphys Pond" | SiteName=="TNNP North" | SiteName=="Unicorn")
tnundp2017 <- add_column(tnundp2017, Date_Time = date(tnundp2017$TimeStamp))
tnundp2017 <- inner_join(tnundp2017, tn2017, by="Date_Time")


# merge all four datasets together 
extracted <- rbind(bl2016, bl2017, tnundp2016, tnundp2017)
# keep important columns
extracted <- subset(extracted, select = c(PlotName, Species, Year, Date_Time, GDD))
# save csv
write.csv(extracted, "input/GDD_2016_2017_R.csv")