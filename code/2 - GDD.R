# Author: Isabella Richmond 
# Last edited: October 14, 2020

# Code for combining weather data from 2016 and 2017 at both weather stations - Terra Nova National Park and 
# Lethbridge. 2016 Lethbridge data was supplemented with Clarenville data (see 1 - WeatherCor.R)
# Weather data was obtained from Environment & Climate Change Canada's historical datasets. 
# Growing Degree Days were calculated as per Sirois 2000 using 5 deg. Celsius as a base temperature 
# as per Brown et al. 2019.

# load packages 
library(easypackages)
easypackages::libraries("tidyverse", "pollen")

#### Data Preparation ####
l2016 <- read_csv("input/GDD/Lethbridge_2016_full_R.csv")
l2017 <- read_csv("input/GDD/Lethbridge_2017_R.csv")
tn2016 <- read_csv("input/GDD/TNNP_2016_R.csv")
tn2017 <- read_csv("input/GDD/TNNP_2017_R.csv")


#### Growing Degree Days ####
# use gdd() to calculate the number of growing degree days for each site during each year 
l2016gdd <- mutate_
