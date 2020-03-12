# Author: Isabella Richmond 
# Last edited: March 12, 2020

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
easypackages::libraries("readr", "dplyr", "tibble")

# load datasets 
clarenville2016 <- read_csv("input/Clarenville_2016_R.csv")
head(clarenville2016)
charleston2016 <- read_csv("input/Charleston_2016_R.csv")
head(charleston2016)
lethbridge2016 <- read_csv("input/Lethbridge_2016_R.csv")
head(lethbridge2016)

# correlations between Clarenville and Lethbridge
# sort datasets by date and time 
clarenville2016 <- arrange(clarenville2016, .by_group = clarenville2016$`Date_Time`)
charleston2016 <- arrange(charleston2016, .by_group = charleston2016$Date_Time)
lethbridge2016 <- arrange(lethbridge2016, .by_group = lethbridge2016$Date_Time)
# add column that identifies the town
clarenville2016 <- add_column(clarenville2016, "Station" = "Clarenville")
charleston2016 <- add_column(charleston2016, "Station" = "Charleston")
lethbridge2016 <- add_column(lethbridge2016, "Station" = "Lethbridge")
# combine town dataframes
clarenleth <- rbind(clarenville2016, lethbridge2016)

clarenvillemeanT <- data.frame(Dates = c(clarenville2016$`Date/Time`), CMeanT = c(clarenville2016$MeanTemp), LMeanT = c(lethbridge2016$MeanTemp))
# evlauate correlation
correlation <- cor.test(climate$CMeanT, climate$LMeanT)
correlation
plot(climate$Dates, climate$CMeanT, col = "red", xlab = "Day in Year", ylab = "Mean Temp (deg C)")
points(climate$Dates, climate$LMeanT, col = "blue")
text(16,18, label = "cor = 0.93")
text(37, 21, label = "p-value < 2.2e-16")
legend(230,-1, legend = c("Clarenville", "Lethbridge"), col = c("red", "blue"), pch = c(1), text.font = 1)

climate2 <- data.frame(Dates = c(Clarenville_2016_R$?..Date.Time), CMinT = c(Clarenville_2016_R$MinTemp), LMinT = c(Lethbridge_2016_R$MinTemp))
correlation2 <- cor.test(climate2$CMinT, climate2$LMinT)
correlation2
plot(climate2$Dates, climate2$CMinT, col = "red", xlab = "Day in Year", ylab = "Minimum Temp (deg C)")
points(climate2$Dates, climate2$LMinT, col = "blue")
text(36, 17, label = "p-value < 2.2e-16")
text(15,14, label = "cor = 0.89")
legend(240,-8, legend = c("Clarenville", "Lethbridge"), col = c("red", "blue"), pch = c(1), text.font = 1)

climate3 <- data.frame(Dates = c(Clarenville_2016_R$?..Date.Time), CMaxT = c(Clarenville_2016_R$MaxTemp), LMaxT = c(Lethbridge_2016_R$MaxTemp))
correlation3 <- cor.test(climate3$CMaxT, climate3$LMaxT)
correlation3
plot(climate3$Dates, climate3$CMaxT, col = "red", xlab = "Day in Year", ylab = "Maximum Temp (deg C)")
points(climate3$Dates, climate3$LMaxT, col = "blue")
text(36, 28, label = "p-value < 2.2e-16")
text(15,25, label = "cor = 0.92")
legend(200,5, legend = c("Clarenville", "Lethbridge"), col = c("red", "blue"), pch = c(1), text.font = 1)

#Correlations between Charleston and Lethbridge
climate4 <- data.frame(Dates = c(Charleston_2016_R$?..Date_Time), ChMeanT = c(Charleston_2016_R$MeanTemp), LMeanT = c(Lethbridge_2016_R$MeanTemp))
View(climate4)
correlation4 <- cor.test(climate4$ChMeanT, climate4$LMeanT)
correlation4
plot(climate4$Dates, climate4$ChMeanT, col = "red", xlab = "Day in Year", ylab = "Mean Temp (deg C)")
points(climate4$Dates, climate4$LMeanT, col = "blue")
text(16,23, label = "cor = 0.90")
text(37, 26, label = "p-value < 2.2e-16")
legend(227,-1, legend = c("Charleston", "Lethbridge"), col = c("red", "blue"), pch = c(1), text.font = 1)

climate5 <- data.frame(Dates = c(Charleston_2016_R$?..Date_Time), ChMinT = c(Charleston_2016_R$MinTemp), LMinT = c(Lethbridge_2016_R$MinTemp))
correlation5 <- cor.test(climate5$ChMinT, climate5$LMinT)
correlation5
plot(climate5$Dates, climate5$ChMinT, col = "red", xlab = "Day in Year", ylab = "Minimum Temp (deg C)")
points(climate5$Dates, climate5$LMinT, col = "blue")
text(36, 22, label = "p-value < 2.2e-16")
text(15,18, label = "cor = 0.82")
legend(220,-8, legend = c("Charleston", "Lethbridge"), col = c("red", "blue"), pch = c(1), text.font = 1)

climate6 <- data.frame(Dates = c(Charleston_2016_R$?..Date_Time), ChMaxT = c(Charleston_2016_R$MaxTemp), LMaxT = c(Lethbridge_2016_R$MaxTemp))
correlation6 <- cor.test(climate6$ChMaxT, climate6$LMaxT)
correlation6
plot(climate6$Dates, climate6$ChMaxT, col = "red", xlab = "Day in Year", ylab = "Maximum Temp (deg C)")
points(climate6$Dates, climate6$LMaxT, col = "blue")
text(36, 30, label = "p-value < 2.2e-16")
text(15,26, label = "cor = 0.88")
legend(200,5, legend = c("Charleston", "Lethbridge"), col = c("red", "blue"), pch = c(1), text.font = 1)
