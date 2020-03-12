#Correlations between Clarenville and Lethbridge
climate <- data.frame(Dates = c(Clarenville_2016_R$ï..Date.Time), CMeanT = c(Clarenville_2016_R$MeanTemp), LMeanT = c(Lethbridge_2016_R$MeanTemp))
View(climate)
correlation <- cor.test(climate$CMeanT, climate$LMeanT)
correlation
plot(climate$Dates, climate$CMeanT, col = "red", xlab = "Day in Year", ylab = "Mean Temp (deg C)")
points(climate$Dates, climate$LMeanT, col = "blue")
text(16,18, label = "cor = 0.93")
text(37, 21, label = "p-value < 2.2e-16")
legend(230,-1, legend = c("Clarenville", "Lethbridge"), col = c("red", "blue"), pch = c(1), text.font = 1)

climate2 <- data.frame(Dates = c(Clarenville_2016_R$ï..Date.Time), CMinT = c(Clarenville_2016_R$MinTemp), LMinT = c(Lethbridge_2016_R$MinTemp))
correlation2 <- cor.test(climate2$CMinT, climate2$LMinT)
correlation2
plot(climate2$Dates, climate2$CMinT, col = "red", xlab = "Day in Year", ylab = "Minimum Temp (deg C)")
points(climate2$Dates, climate2$LMinT, col = "blue")
text(36, 17, label = "p-value < 2.2e-16")
text(15,14, label = "cor = 0.89")
legend(240,-8, legend = c("Clarenville", "Lethbridge"), col = c("red", "blue"), pch = c(1), text.font = 1)

climate3 <- data.frame(Dates = c(Clarenville_2016_R$ï..Date.Time), CMaxT = c(Clarenville_2016_R$MaxTemp), LMaxT = c(Lethbridge_2016_R$MaxTemp))
correlation3 <- cor.test(climate3$CMaxT, climate3$LMaxT)
correlation3
plot(climate3$Dates, climate3$CMaxT, col = "red", xlab = "Day in Year", ylab = "Maximum Temp (deg C)")
points(climate3$Dates, climate3$LMaxT, col = "blue")
text(36, 28, label = "p-value < 2.2e-16")
text(15,25, label = "cor = 0.92")
legend(200,5, legend = c("Clarenville", "Lethbridge"), col = c("red", "blue"), pch = c(1), text.font = 1)

#Correlations between Charleston and Lethbridge
climate4 <- data.frame(Dates = c(Charleston_2016_R$ï..Date_Time), ChMeanT = c(Charleston_2016_R$MeanTemp), LMeanT = c(Lethbridge_2016_R$MeanTemp))
View(climate4)
correlation4 <- cor.test(climate4$ChMeanT, climate4$LMeanT)
correlation4
plot(climate4$Dates, climate4$ChMeanT, col = "red", xlab = "Day in Year", ylab = "Mean Temp (deg C)")
points(climate4$Dates, climate4$LMeanT, col = "blue")
text(16,23, label = "cor = 0.90")
text(37, 26, label = "p-value < 2.2e-16")
legend(227,-1, legend = c("Charleston", "Lethbridge"), col = c("red", "blue"), pch = c(1), text.font = 1)

climate5 <- data.frame(Dates = c(Charleston_2016_R$ï..Date_Time), ChMinT = c(Charleston_2016_R$MinTemp), LMinT = c(Lethbridge_2016_R$MinTemp))
correlation5 <- cor.test(climate5$ChMinT, climate5$LMinT)
correlation5
plot(climate5$Dates, climate5$ChMinT, col = "red", xlab = "Day in Year", ylab = "Minimum Temp (deg C)")
points(climate5$Dates, climate5$LMinT, col = "blue")
text(36, 22, label = "p-value < 2.2e-16")
text(15,18, label = "cor = 0.82")
legend(220,-8, legend = c("Charleston", "Lethbridge"), col = c("red", "blue"), pch = c(1), text.font = 1)

climate6 <- data.frame(Dates = c(Charleston_2016_R$ï..Date_Time), ChMaxT = c(Charleston_2016_R$MaxTemp), LMaxT = c(Lethbridge_2016_R$MaxTemp))
correlation6 <- cor.test(climate6$ChMaxT, climate6$LMaxT)
correlation6
plot(climate6$Dates, climate6$ChMaxT, col = "red", xlab = "Day in Year", ylab = "Maximum Temp (deg C)")
points(climate6$Dates, climate6$LMaxT, col = "blue")
text(36, 30, label = "p-value < 2.2e-16")
text(15,26, label = "cor = 0.88")
legend(200,5, legend = c("Charleston", "Lethbridge"), col = c("red", "blue"), pch = c(1), text.font = 1)
