# Author: Isabella Richmond
# Last edited: March 14, 2020

# This code is for the creation and evaluation of my temporal stoichiometry models. A lot of 
# Code was provided by Travis Heckford (twitter.com/travheckford)
# Models evaluate the response of stoichiometry in four boreal plant species with site, year
# weather, moisture, and productivity as explanatory variables
# Stoichiometry measured using C:N, C:P, and N:P 
# Elemental composition is also used as a response variable - % C, % N, % P and quantities

#### Data Preparation ####
# load packages
install.packages("easypackages")
library(easypackages)
install_packages("ggcorrplot")
libraries("ggcorrplot", "ggplot2","dplyr", "tibble", "readr", "plyr", "ggpol", "ggpubr", "MuMIn", "AICcmodavg", "texreg", "kimisc", "psych", "DescTools")

# import datasets
stoich <- read_csv("input/Stoich_2016_2017.csv")
gdd <- read_csv("input/GDD_2016_2017.csv")
evi <- read_csv("input/EVI_2016_2017.csv")
ndmi <- read_csv("input/NDMI_2016_2017.csv")
# subset by year so that joining is possible 
stoich2016 <- subset(stoich, Year==2016)
stoich2017 <- subset(stoich, Year==2017)
evi2016 <- subset(evi, Year==2016)
evi2017 <- subset(evi, Year==2017)
ndmi2016 <- subset(ndmi, Year==2016)
ndmi2017 <- subset(ndmi, Year==2017)
# ndmi and evi datasets have a different number of rows because there are multiple observations
# at each sample point - due to there being multiple species at each sample point
# want to add evi and ndmi columns to the stoich dataset to combine data
# use inner_join by dpylr for EVI and NDMI because it keeps all matching and does not keep 
# anything that doesn't match
# output dataframe should have same number of rows as input stoich dataframe 
stoich2016 <- stoich2016 %>%
  inner_join(evi2016,by="PlotName") %>%
  inner_join(ndmi2016, by="PlotName")
stoich2017 <- stoich2017 %>%
  inner_join(evi2017, by="PlotName")%>%
  inner_join(ndmi2017,by="PlotName")
# bind the 2016 and 2017 dataset back together 
stoich <- rbind(stoich2016,stoich2017)
# add GDD columns to stoich dataframe
stoich <- add_column(stoich, GDD=gdd$GDD, GDDAverage=gdd$AverageGDD)

# convert Year variable to factor (listed as integer)
stoich$Year <- as.factor(stoich$Year)
str(stoich)

# subset the data by species
# ABBA = Abies balsamea, balsam fir 
# ACRU = Acer rubrum, red maple 
# BEPA = Betula papyrifera, white birch 
# VAAN = Vaccinium angustifolium, lowland blueberry
ABBA <- subset(stoich, Species == "ABBA")
ACRU <- subset(stoich, Species == "ACRU")
BEPA <- subset(stoich, Species == "BEPA")
VAAN <- subset(stoich, Species == "VAAN")
str(ABBA)
str(ACRU)
str(BEPA)
str(VAAN)


#### Data Exploration ####
# make preliminary boxplots to compare data between years
# compare N, C, and P across years for each speciess
# code from Juliana Balluffi-Fry (https://gitlab.com/jballuffi)
# manually setting up colours 
cols<-c("2016"= rgb(52,71,61, maxColorValue = 255), "2017"= rgb(84,158,57, maxColorValue = 255))

#Produce boxplot for %C, %N, %P, C, N, P, C:N, C:P, N:P for each species
#ABBA 
#%C
ggplot(data=ABBA, aes(Year, C, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+ #this is where the manual colors come in
  ggtitle("Balsam Fir")+
  labs(y="% Carbon", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ABBAPerC.jpg")

#%P 
ggplot(data=ABBA, aes(Year, P, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Balsam Fir")+
  labs(y="% Phosphorus", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ABBAPerP.jpg")

#%N
ggplot(data=ABBA, aes(Year, N, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Balsam Fir")+
  labs(y="% Nitrogen", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ABBAPerN.jpg")

#Qty C
ggplot(data=ABBA, aes(Year, Qty_C, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Balsam Fir")+
  labs(y="Carbon (g)", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ABBAQuanC.jpg")

#Qty P
ggplot(data=ABBA, aes(Year, Qty_P, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Balsam Fir")+
  labs(y="Phosphorus (g)", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ABBAQuanP.jpg")

#Qty N
ggplot(data=ABBA, aes(Year, Qty_N, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Balsam Fir")+
  labs(y="Nitrogen (g)", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ABBAQuanN.jpg")

#C:N
ggplot(data=ABBA, aes(Year, CNRatio, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Balsam Fir")+
  labs(y="Carbon:Nitrogen", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ABBACN.jpg")

#C:P
ggplot(data=ABBA, aes(Year, CPRatio, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Balsam Fir")+
  labs(y="Carbon:Phosphorus", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ABBACP.jpg")

#N:P
ggplot(data=ABBA, aes(Year, NPRatio, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Balsam Fir")+
  labs(y="Nitrogen:Phosphorus", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ABBANP.jpg")

#ACRU
#%C
ggplot(data=ACRU, aes(Year, C, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+ #this is where the manual colors come in
  ggtitle("Red Maple")+
  labs(y="% Carbon", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ACRUPerC.jpg")

#%P 
ggplot(data=ACRU, aes(Year, P, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Red Maple")+
  labs(y="% Phosphorus", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ACRUPerP.jpg")

#%N
ggplot(data=ACRU, aes(Year, N, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Red Maple")+
  labs(y="% Nitrogen", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ACRUPerN.jpg")

#Qty C
ggplot(data=ACRU, aes(Year, Qty_C, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Red Maple")+
  labs(y="Carbon (g)", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ACRUQuanC.jpg")

#Qty P
ggplot(data=ACRU, aes(Year, Qty_P, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Red Maple")+
  labs(y="Phosphorus (g)", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ACRUQuanP.jpg")

#Qty N
ggplot(data=ACRU, aes(Year, Qty_N, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Red Maple")+
  labs(y="Nitrogen (g)", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ACRUQuanN.jpg")

#C:N
ggplot(data=ACRU, aes(Year, CNRatio, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Red Maple")+
  labs(y="Carbon:Nitrogen", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ACRUCN.jpg")

#C:P
ggplot(data=ACRU, aes(Year, CPRatio, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Red Maple")+
  labs(y="Carbon:Phosphorus", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ACRUCP.jpg")

#N:P
ggplot(data=ACRU, aes(Year, NPRatio, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Red Maple")+
  labs(y="Nitrogen:Phosphorus", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/ACRUNP.jpg")

#BEPA
#%C
ggplot(data=BEPA, aes(Year, C, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+ #this is where the manual colors come in
  ggtitle("White Birch")+
  labs(y="% Carbon", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/BEPAPerC.jpg")

#%P 
ggplot(data=BEPA, aes(Year, P, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("White Birch")+
  labs(y="% Phosphorus", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/BEPAPerP.jpg")

#%N
ggplot(data=BEPA, aes(Year, N, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("White Birch")+
  labs(y="% Nitrogen", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/BEPAPerN.jpg")

#Qty C
ggplot(data=BEPA, aes(Year, Qty_C, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("White Birch")+
  labs(y="Carbon (g)", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/BEPAQuanC.jpg")

#Qty P
ggplot(data=BEPA, aes(Year, Qty_P, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("White Birch")+
  labs(y="Phosphorus (g)", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/BEPAQuanP.jpg")

#Qty N
ggplot(data=BEPA, aes(Year, Qty_N, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("White Birch")+
  labs(y="Nitrogen (g)", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/BEPAQuanN.jpg")

#C:N
ggplot(data=BEPA, aes(Year, CNRatio, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("White Birch")+
  labs(y="Carbon:Nitrogen", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/BEPACN.jpg")

#C:P
ggplot(data=BEPA, aes(Year, CPRatio, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("White Birch")+
  labs(y="Carbon:Phosphorus", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/BEPACP.jpg")

#N:P
ggplot(data=BEPA, aes(Year, NPRatio, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("White Birch")+
  labs(y="Nitrogen:Phosphorus", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/BEPANP.jpg")

#VAAN
#%C
ggplot(data=VAAN, aes(Year, C, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+ #this is where the manual colors come in
  ggtitle("Lowland Blueberry")+
  labs(y="% Carbon", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/VAANPerC.jpg")

#%P 
ggplot(data=VAAN, aes(Year, P, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Lowland Blueberry")+
  labs(y="% Phosphorus", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/VAANPerP.jpg")

#%N
ggplot(data=VAAN, aes(Year, N, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Lowland Blueberry")+
  labs(y="% Nitrogen", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/VAANPerN.jpg")

#Qty C
ggplot(data=VAAN, aes(Year, Qty_C, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Lowland Blueberry")+
  labs(y="Carbon (g)", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/VAANQuanC.jpg")

#Qty P
ggplot(data=VAAN, aes(Year, Qty_P, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Lowland Blueberry")+
  labs(y="Phosphorus (g)", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/VAANQuanP.jpg")

#Qty N
ggplot(data=VAAN, aes(Year, Qty_N, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Lowland Blueberry")+
  labs(y="Nitrogen (g)", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/VAANQuanN.jpg")

#C:N
ggplot(data=VAAN, aes(Year, CNRatio, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Lowland Blueberry")+
  labs(y="Carbon:Nitrogen", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/VAANCN.jpg")

#C:P
ggplot(data=VAAN, aes(Year, CPRatio, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Lowland Blueberry")+
  labs(y="Carbon:Phosphorus", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/VAANCP.jpg")

#N:P
ggplot(data=VAAN, aes(Year, NPRatio, fill=Year))+
  geom_boxplot(position="dodge", notch=TRUE)+ 
  scale_fill_manual(values=cols, guide=FALSE)+
  ggtitle("Lowland Blueberry")+
  labs(y="Nitrogen:Phosphorus", x="Year")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size = 14),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave("graphics/StoichModels/Boxplots/VAANNP.jpg")


#### Temporal Stoich Models ####
# test for correlation between covariates
# if r > 0.70, variables are highly correlated and should not be in the same models 
# compute a correlation matrix 
# isolate the explanatory variables 
abbacorrdata <- tibble(ABBA$GDD, ABBA$NDMI,ABBA$EVI)
abbacorr <- (cor(abbacorrdata))
ggcorrplot(abbacorr, hc.order = TRUE, lab = TRUE)
ggsave("graphics/StoichModels/Correlations/ABBAcorr.jpg")

acrucorrdata <- tibble(ACRU$GDD, ACRU$NDMI,ACRU$EVI)
acrucorr <- (cor(acrucorrdata))
ggcorrplot(acrucorr, hc.order = TRUE, lab = TRUE)
ggsave("graphics/StoichModels/Correlations/ACRUcorr.jpg")

bepacorrdata <- tibble(BEPA$GDD, BEPA$NDMI,BEPA$EVI)
bepacorr <- (cor(bepacorrdata))
ggcorrplot(bepacorr, hc.order = TRUE, lab = TRUE)
ggsave("graphics/StoichModels/Correlations/BEPAcorr.jpg")

vaancorrdata <- tibble(VAAN$GDD, VAAN$NDMI,VAAN$EVI)
vaancorr <- (cor(vaancorrdata))
ggcorrplot(vaancorr, hc.order = TRUE, lab = TRUE)
ggsave("graphics/StoichModels/Correlations/VAANcorr.jpg")

# using AICc to evaluate all variables and combinations of models 
# run models separately for each species
# fit models of c, N, P, Qty C, Qty N, Qty P, C:N, or C:P, and N:P as a function of covariates for each species
# global model: Species_Element ~ Year * GDD * EVI * NDMI * (Site) 

# ABBA
# %C
ABBA.C1 <- glm(C ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.C2 <- glm(C ~ Year * EVI * GDD, data = ABBA)
ABBA.C3 <- glm(C~ Year * EVI * Site, data = ABBA)
ABBA.C4 <- glm(C ~ Year * Site * GDD, data = ABBA)
ABBA.C5 <- glm(C ~ EVI * GDD * Site, data = ABBA)
ABBA.C6 <- glm(C ~ Year * GDD, data = ABBA)
ABBA.C7 <- glm(C ~ Year * EVI, data = ABBA)
ABBA.C8 <- glm(C ~ Year * Site, data = ABBA)
ABBA.C9 <- glm(C ~ GDD * EVI, data = ABBA)
ABBA.C10 <- glm(C ~ GDD * Site, data = ABBA)
ABBA.C11 <- glm(C ~ EVI * Site, data = ABBA)
ABBA.C12 <- glm(C ~ Site, data = ABBA)
ABBA.C13 <- glm(C ~ GDD, data = ABBA)
ABBA.C14 <- glm(C ~ EVI, data = ABBA)  
ABBA.C15 <- glm(C ~ Year, data = ABBA)  
ABBA.C16 <- glm(C ~ 1, data =  ABBA)

#Create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.C <- list("ABBA.C1" = ABBA.C1, "ABBA.C2" = ABBA.C2, "ABBA.C3" = ABBA.C3, "ABBA.C4" = ABBA.C4, "ABBA.C5" = ABBA.C5, "ABBA.C6" = ABBA.C6, "ABBA.C7" = ABBA.C7, "ABBA.C8" =  ABBA.C8, "ABBA.C9" = ABBA.C9, "ABBA.C10" = ABBA.C10, "ABBA.C11" = ABBA.C11, "ABBA.C12" = ABBA.C12, "ABBA.C13" = ABBA.C13, "ABBA.C14" = ABBA.C14, "ABBA.C15" = ABBA.C15, "ABBA.C16" = ABBA.C16)
ABBA.C <- aictab(cand.set = Models.ABBA.C)
print(ABBA.C)
write.csv(ABBA.C, "AIC/Tables/ABBA_C.csv")
#Summary of the most parsimonious model
summary(ABBA.C4)
#Calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.C4, which = "Nagelkerke")

#%P
ABBA.P1 <- glm(P ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.P2 <- glm(P ~ Year * EVI * GDD, data = ABBA)
ABBA.P3 <- glm(P~ Year * EVI * Site, data = ABBA)
ABBA.P4 <- glm(P ~ Year * Site * GDD, data = ABBA)
ABBA.P5 <- glm(P ~ EVI * GDD * Site, data = ABBA)
ABBA.P6 <- glm(P ~ Year * GDD, data = ABBA)
ABBA.P7 <- glm(P ~ Year * EVI, data = ABBA)
ABBA.P8 <- glm(P ~ Year * Site, data = ABBA)
ABBA.P9 <- glm(P ~ GDD * EVI, data = ABBA)
ABBA.P10 <- glm(P ~ GDD * Site, data = ABBA)
ABBA.P11 <- glm(P ~ EVI * Site, data = ABBA)
ABBA.P12 <- glm(P ~ Site, data = ABBA)
ABBA.P13 <- glm(P ~ GDD, data = ABBA)
ABBA.P14 <- glm(P ~ EVI, data = ABBA)  
ABBA.P15 <- glm(P ~ Year, data = ABBA)  
ABBA.P16 <- glm(P ~ 1, data =  ABBA)
Models.ABBA.P <- list("ABBA.P1" = ABBA.P1, "ABBA.P2" = ABBA.P2, "ABBA.P3" = ABBA.P3, "ABBA.P4" = ABBA.P4, "ABBA.P5" = ABBA.P5, "ABBA.P6" = ABBA.P6, "ABBA.P7" = ABBA.P7, "ABBA.P8" =  ABBA.P8, "ABBA.P9" = ABBA.P9, "ABBA.P10" = ABBA.P10, "ABBA.P11" = ABBA.P11, "ABBA.P12" = ABBA.P12, "ABBA.P13" = ABBA.P13, "ABBA.P14" = ABBA.P14, "ABBA.P15" = ABBA.P15, "ABBA.P16" = ABBA.P16)
ABBA.P <- aictab(cand.set = Models.ABBA.P)
print(ABBA.P)
write.csv(ABBA.P, "AIC/Tables/ABBA_P.csv")
summary(ABBA.P8)
PseudoR2(ABBA.P8, which = "Nagelkerke")
#%N
ABBA.N1 <- glm(N ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.N2 <- glm(N ~ Year * EVI * GDD, data = ABBA)
ABBA.N3 <- glm(N~ Year * EVI * Site, data = ABBA)
ABBA.N4 <- glm(N ~ Year * Site * GDD, data = ABBA)
ABBA.N5 <- glm(N ~ EVI * GDD * Site, data = ABBA)
ABBA.N6 <- glm(N ~ Year * GDD, data = ABBA)
ABBA.N7 <- glm(N ~ Year * EVI, data = ABBA)
ABBA.N8 <- glm(N ~ Year * Site, data = ABBA)
ABBA.N9 <- glm(N ~ AverageGDD * EVI, data = ABBA)
ABBA.N10 <- glm(N ~ AverageGDD * Site, data = ABBA)
ABBA.N11 <- glm(N ~ EVI * Site, data = ABBA)
ABBA.N12 <- glm(N ~ Site, data = ABBA)
ABBA.N13 <- glm(N ~ GDD, data = ABBA)
ABBA.N14 <- glm(N ~ EVI, data = ABBA)  
ABBA.N15 <- glm(N ~ Year, data = ABBA)  
ABBA.N16 <- glm(N ~ 1, data =  ABBA)
Models.ABBA.N <- list("ABBA.N1" = ABBA.N1, "ABBA.N2" = ABBA.N2, "ABBA.N3" = ABBA.N3, "ABBA.N4" = ABBA.N4, "ABBA.N5" = ABBA.N5, "ABBA.N6" = ABBA.N6, "ABBA.N7" = ABBA.N7, "ABBA.N8" =  ABBA.N8, "ABBA.N9" = ABBA.N9, "ABBA.N10" = ABBA.N10, "ABBA.N11" = ABBA.N11, "ABBA.N12" = ABBA.N12, "ABBA.N13" = ABBA.N13, "ABBA.N14" = ABBA.N14, "ABBA.N15" = ABBA.N15, "ABBA.N16" = ABBA.N16)
ABBA.N <- aictab(cand.set = Models.ABBA.N)
print(ABBA.N)
write.csv(ABBA.N, "AIC/Tables/ABBA_N.csv")
summary(ABBA.N5)
PseudoR2(ABBA.N5, which = "Nagelkerke")
#C:N
ABBA.CN1 <- glm(CNRatio ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.CN2 <- glm(CNRatio ~ Year * EVI * GDD, data = ABBA)
ABBA.CN3 <- glm(CNRatio ~ Year * EVI * Site, data = ABBA)
ABBA.CN4 <- glm(CNRatio ~ Year * Site * GDD, data = ABBA)
ABBA.CN5 <- glm(CNRatio ~ EVI * GDD * Site, data = ABBA)
ABBA.CN6 <- glm(CNRatio ~ Year * GDD, data = ABBA)
ABBA.CN7 <- glm(CNRatio ~ Year * EVI, data = ABBA)
ABBA.CN8 <- glm(CNRatio ~ Year * Site, data = ABBA)
ABBA.CN9 <- glm(CNRatio ~ GDD * EVI, data = ABBA)
ABBA.CN10 <- glm(CNRatio ~ GDD * Site, data = ABBA)
ABBA.CN11 <- glm(CNRatio ~ EVI * Site, data = ABBA)
ABBA.CN12 <- glm(CNRatio ~ Site, data = ABBA)
ABBA.CN13 <- glm(CNRatio ~ GDD, data = ABBA)
ABBA.CN14 <- glm(CNRatio ~ EVI, data = ABBA)  
ABBA.CN15 <- glm(CNRatio ~ Year, data = ABBA)  
ABBA.CN16 <- glm(CNRatio ~ 1, data =  ABBA)
Models.ABBA.CN <- list("ABBA.CN1" = ABBA.CN1, "ABBA.CN2" = ABBA.CN2, "ABBA.CN3" = ABBA.CN3, "ABBA.CN4" = ABBA.CN4, "ABBA.CN5" = ABBA.CN5, "ABBA.CN6" = ABBA.CN6, "ABBA.CN7" = ABBA.CN7, "ABBA.CN8" =  ABBA.CN8, "ABBA.CN9" = ABBA.CN9, "ABBA.CN10" = ABBA.CN10, "ABBA.CN11" = ABBA.CN11, "ABBA.CN12" = ABBA.CN12, "ABBA.CN13" = ABBA.CN13, "ABBA.CN14" = ABBA.CN14, "ABBA.CN15" = ABBA.CN15, "ABBA.CN16" = ABBA.CN16)
ABBA.CN <- aictab(cand.set = Models.ABBA.CN)
print(ABBA.CN)
write.csv(ABBA.CN, "AIC/Tables/ABBA_CN.csv")
summary(ABBA.CN5)
PseudoR2(ABBA.CN5, which = "Nagelkerke")
#C:P
ABBA.CP1 <- glm(CPRatio ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.CP2 <- glm(CPRatio ~ Year * EVI * GDD, data = ABBA)
ABBA.CP3 <- glm(CPRatio ~ Year * EVI * Site, data = ABBA)
ABBA.CP4 <- glm(CPRatio ~ Year * Site * GDD, data = ABBA)
ABBA.CP5 <- glm(CPRatio ~ EVI * GDD * Site, data = ABBA)
ABBA.CP6 <- glm(CPRatio ~ Year * GDD, data = ABBA)
ABBA.CP7 <- glm(CPRatio ~ Year * EVI, data = ABBA)
ABBA.CP8 <- glm(CPRatio ~ Year * Site, data = ABBA)
ABBA.CP9 <- glm(CPRatio ~ GDD * EVI, data = ABBA)
ABBA.CP10 <- glm(CPRatio ~ GDD * Site, data = ABBA)
ABBA.CP11 <- glm(CPRatio ~ EVI * Site, data = ABBA)
ABBA.CP12 <- glm(CPRatio ~ Site, data = ABBA)
ABBA.CP13 <- glm(CPRatio ~ GDD, data = ABBA)
ABBA.CP14 <- glm(CPRatio ~ EVI, data = ABBA)  
ABBA.CP15 <- glm(CPRatio ~ Year, data = ABBA)  
ABBA.CP16 <- glm(CPRatio ~ 1, data =  ABBA)
Models.ABBA.CP <- list("ABBA.CP1" = ABBA.CP1, "ABBA.CP2" = ABBA.CP2, "ABBA.CP3" = ABBA.CP3, "ABBA.CP4" = ABBA.CP4, "ABBA.CP5" = ABBA.CP5, "ABBA.CP6" = ABBA.CP6, "ABBA.CP7" = ABBA.CP7, "ABBA.CP8" =  ABBA.CP8, "ABBA.CP9" = ABBA.CP9, "ABBA.CP10" = ABBA.CP10, "ABBA.CP11" = ABBA.CP11, "ABBA.CP12" = ABBA.CP12, "ABBA.CP13" = ABBA.CP13, "ABBA.CP14" = ABBA.CP14, "ABBA.CP15" = ABBA.CP15, "ABBA.CP16" = ABBA.CP16)
ABBA.CP <- aictab(cand.set = Models.ABBA.CP)
print(ABBA.CP)
write.csv(ABBA.CP, "AIC/Tables/ABBA_CP.csv")
summary(ABBA.CP4)
PseudoR2(ABBA.CP4, which = "Nagelkerke")
#C:P
ABBA.NP1 <- glm(NPRatio ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.NP2 <- glm(NPRatio ~ Year * EVI * GDD, data = ABBA)
ABBA.NP3 <- glm(NPRatio ~ Year * EVI * Site, data = ABBA)
ABBA.NP4 <- glm(NPRatio ~ Year * Site * GDD, data = ABBA)
ABBA.NP5 <- glm(NPRatio ~ EVI * GDD * Site, data = ABBA)
ABBA.NP6 <- glm(NPRatio ~ Year * GDD, data = ABBA)
ABBA.NP7 <- glm(NPRatio ~ Year * EVI, data = ABBA)
ABBA.NP8 <- glm(NPRatio ~ Year * Site, data = ABBA)
ABBA.NP9 <- glm(NPRatio ~ GDD * EVI, data = ABBA)
ABBA.NP10 <- glm(NPRatio ~ GDD * Site, data = ABBA)
ABBA.NP11 <- glm(NPRatio ~ EVI * Site, data = ABBA)
ABBA.NP12 <- glm(NPRatio ~ Site, data = ABBA)
ABBA.NP13 <- glm(NPRatio ~ GDD, data = ABBA)
ABBA.NP14 <- glm(NPRatio ~ EVI, data = ABBA)  
ABBA.NP15 <- glm(NPRatio ~ Year, data = ABBA)  
ABBA.NP16 <- glm(NPRatio ~ 1, data =  ABBA)
Models.ABBA.NP <- list("ABBA.NP1" = ABBA.NP1, "ABBA.NP2" = ABBA.NP2, "ABBA.NP3" = ABBA.NP3, "ABBA.NP4" = ABBA.NP4, "ABBA.NP5" = ABBA.NP5, "ABBA.NP6" = ABBA.NP6, "ABBA.NP7" = ABBA.NP7, "ABBA.NP8" =  ABBA.NP8, "ABBA.NP9" = ABBA.NP9, "ABBA.NP10" = ABBA.NP10, "ABBA.NP11" = ABBA.NP11, "ABBA.NP12" = ABBA.NP12, "ABBA.NP13" = ABBA.NP13, "ABBA.NP14" = ABBA.NP14, "ABBA.NP15" = ABBA.NP15, "ABBA.NP16" = ABBA.NP16)
ABBA.NP <- aictab(cand.set = Models.ABBA.NP)
print(ABBA.NP)
write.csv(ABBA.NP, "AIC/Tables/ABBA_NP.csv")
summary(ABBA.NP11)
PseudoR2(ABBA.NP11, which = "Nagelkerke")
#Qty_C
ABBA.Qty_C1 <- glm(Qty_C ~ Year * EVI * GDD * Site, data = ABBA)

ABBA.Qty_C2 <- glm(Qty_C ~ Year * EVI * GDD, data = ABBA)
ABBA.Qty_C3 <- glm(Qty_C ~ Year * EVI * Site, data = ABBA)
ABBA.Qty_C4 <- glm(Qty_C ~ Year * Site * GDD, data = ABBA)
ABBA.Qty_C5 <- glm(Qty_C ~ EVI * GDD * Site, data = ABBA)
ABBA.Qty_C6 <- glm(Qty_C ~ Year * GDD, data = ABBA)
ABBA.Qty_C7 <- glm(Qty_C ~ Year * EVI, data = ABBA)
ABBA.Qty_C8 <- glm(Qty_C ~ Year * Site, data = ABBA)
ABBA.Qty_C9 <- glm(Qty_C ~ GDD * EVI, data = ABBA)
ABBA.Qty_C10 <- glm(Qty_C ~ GDD * Site, data = ABBA)
ABBA.Qty_C11 <- glm(Qty_C ~ EVI * Site, data = ABBA)
ABBA.Qty_C12 <- glm(Qty_C ~ Site, data = ABBA)
ABBA.Qty_C13 <- glm(Qty_C ~ GDD, data = ABBA)
ABBA.Qty_C14 <- glm(Qty_C ~ EVI, data = ABBA)  
ABBA.Qty_C15 <- glm(Qty_C ~ Year, data = ABBA)  
ABBA.Qty_C16 <- glm(Qty_C ~ 1, data =  ABBA)
Models.ABBA.Qty_C <- list("ABBA.Qty_C1" = ABBA.Qty_C1, "ABBA.Qty_C2" = ABBA.Qty_C2, "ABBA.Qty_C3" = ABBA.Qty_C3, "ABBA.Qty_C4" = ABBA.Qty_C4, "ABBA.Qty_C5" = ABBA.Qty_C5, "ABBA.Qty_C6" = ABBA.Qty_C6, "ABBA.Qty_C7" = ABBA.Qty_C7, "ABBA.Qty_C8" =  ABBA.Qty_C8, "ABBA.Qty_C9" = ABBA.Qty_C9, "ABBA.Qty_C10" = ABBA.Qty_C10, "ABBA.Qty_C11" = ABBA.Qty_C11, "ABBA.Qty_C12" = ABBA.Qty_C12, "ABBA.Qty_C13" = ABBA.Qty_C13, "ABBA.Qty_C14" = ABBA.Qty_C14, "ABBA.Qty_C15" = ABBA.Qty_C15, "ABBA.Qty_C16" = ABBA.Qty_C16)
ABBA.Qty_C <- aictab(cand.set = Models.ABBA.Qty_C)
print(ABBA.Qty_C)
write.csv(ABBA.Qty_C, "AIC/Tables/ABBA_Qty_C.csv")
summary(ABBA.Qty_C2)
PseudoR2(ABBA.Qty_C2, which = "Nagelkerke")
#Qty_P
ABBA.Qty_P1 <- glm(Qty_P ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.Qty_P2 <- glm(Qty_P ~ Year * EVI * GDD, data = ABBA)
ABBA.Qty_P3 <- glm(Qty_P ~ Year * EVI * Site, data = ABBA)
ABBA.Qty_P4 <- glm(Qty_P ~ Year * Site * GDD, data = ABBA)
ABBA.Qty_P5 <- glm(Qty_P ~ EVI * GDD * Site, data = ABBA)
ABBA.Qty_P6 <- glm(Qty_P ~ Year * GDD, data = ABBA)
ABBA.Qty_P7 <- glm(Qty_P ~ Year * EVI, data = ABBA)
ABBA.Qty_P8 <- glm(Qty_P ~ Year * Site, data = ABBA)
ABBA.Qty_P9 <- glm(Qty_P ~ GDD * EVI, data = ABBA)
ABBA.Qty_P10 <- glm(Qty_P ~ GDD * Site, data = ABBA)
ABBA.Qty_P11 <- glm(Qty_P ~ EVI * Site, data = ABBA)
ABBA.Qty_P12 <- glm(Qty_P ~ Site, data = ABBA)
ABBA.Qty_P13 <- glm(Qty_P ~ GDD, data = ABBA)
ABBA.Qty_P14 <- glm(Qty_P ~ EVI, data = ABBA)  
ABBA.Qty_P15 <- glm(Qty_P ~ Year, data = ABBA)  
ABBA.Qty_P16 <- glm(Qty_P ~ 1, data =  ABBA)
Models.ABBA.Qty_P <- list("ABBA.Qty_P1" = ABBA.Qty_P1, "ABBA.Qty_P2" = ABBA.Qty_P2, "ABBA.Qty_P3" = ABBA.Qty_P3, "ABBA.Qty_P4" = ABBA.Qty_P4, "ABBA.Qty_P5" = ABBA.Qty_P5, "ABBA.Qty_P6" = ABBA.Qty_P6, "ABBA.Qty_P7" = ABBA.Qty_P7, "ABBA.Qty_P8" =  ABBA.Qty_P8, "ABBA.Qty_P9" = ABBA.Qty_P9, "ABBA.Qty_P10" = ABBA.Qty_P10, "ABBA.Qty_P11" = ABBA.Qty_P11, "ABBA.Qty_P12" = ABBA.Qty_P12, "ABBA.Qty_P13" = ABBA.Qty_P13, "ABBA.Qty_P14" = ABBA.Qty_P14, "ABBA.Qty_P15" = ABBA.Qty_P15, "ABBA.Qty_P16" = ABBA.Qty_P16)
ABBA.Qty_P <- aictab(cand.set = Models.ABBA.Qty_P)
print(ABBA.Qty_P)
write.csv(ABBA.Qty_P, "AIC/Tables/ABBA_Qty_P.csv")
summary(ABBA.Qty_P2)
PseudoR2(ABBA.Qty_P2, which = "Nagelkerke")
#Qty_N
ABBA.Qty_N1 <- glm(Qty_N ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.Qty_N2 <- glm(Qty_N ~ Year * EVI * GDD, data = ABBA)
ABBA.Qty_N3 <- glm(Qty_N ~ Year * EVI * Site, data = ABBA)
ABBA.Qty_N4 <- glm(Qty_N ~ Year * Site * GDD, data = ABBA)
ABBA.Qty_N5 <- glm(Qty_N ~ EVI * GDD * Site, data = ABBA)
ABBA.Qty_N6 <- glm(Qty_N ~ Year * GDD, data = ABBA)
ABBA.Qty_N7 <- glm(Qty_N ~ Year * EVI, data = ABBA)
ABBA.Qty_N8 <- glm(Qty_N ~ Year * Site, data = ABBA)
ABBA.Qty_N9 <- glm(Qty_N ~ GDD * EVI, data = ABBA)
ABBA.Qty_N10 <- glm(Qty_N ~ GDD * Site, data = ABBA)
ABBA.Qty_N11 <- glm(Qty_N ~ EVI * Site, data = ABBA)
ABBA.Qty_N12 <- glm(Qty_N ~ Site, data = ABBA)
ABBA.Qty_N13 <- glm(Qty_N ~ GDD, data = ABBA)
ABBA.Qty_N14 <- glm(Qty_N ~ EVI, data = ABBA)  
ABBA.Qty_N15 <- glm(Qty_N ~ Year, data = ABBA)  
ABBA.Qty_N16 <- glm(Qty_N ~ 1, data =  ABBA)
Models.ABBA.Qty_N <- list("ABBA.Qty_N1" = ABBA.Qty_N1, "ABBA.Qty_N2" = ABBA.Qty_N2, "ABBA.Qty_N3" = ABBA.Qty_N3, "ABBA.Qty_N4" = ABBA.Qty_N4, "ABBA.Qty_N5" = ABBA.Qty_N5, "ABBA.Qty_N6" = ABBA.Qty_N6, "ABBA.Qty_N7" = ABBA.Qty_N7, "ABBA.Qty_N8" =  ABBA.Qty_N8, "ABBA.Qty_N9" = ABBA.Qty_N9, "ABBA.Qty_N10" = ABBA.Qty_N10, "ABBA.Qty_N11" = ABBA.Qty_N11, "ABBA.Qty_N12" = ABBA.Qty_N12, "ABBA.Qty_N13" = ABBA.Qty_N13, "ABBA.Qty_N14" = ABBA.Qty_N14, "ABBA.Qty_N15" = ABBA.Qty_N15, "ABBA.Qty_N16" = ABBA.Qty_N16)
ABBA.Qty_N <- aictab(cand.set = Models.ABBA.Qty_N)
print(ABBA.Qty_N)
write.csv(ABBA.Qty_N, "AIC/Tables/ABBA_Qty_N.csv")
summary(ABBA.Qty_N2)
PseudoR2(ABBA.Qty_N2, which = "Nagelkerke")

#ACRU
#%C
ACRU.C1 <- glm(C ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.C2 <- glm(C ~ Year * EVI * GDD, data = ACRU)
ACRU.C3 <- glm(C~ Year * EVI * Site, data = ACRU)
ACRU.C4 <- glm(C ~ Year * Site * GDD, data = ACRU)
ACRU.C5 <- glm(C ~ EVI * GDD * Site, data = ACRU)
ACRU.C6 <- glm(C ~ Year * GDD, data = ACRU)
ACRU.C7 <- glm(C ~ Year * EVI, data = ACRU)
ACRU.C8 <- glm(C ~ Year * Site, data = ACRU)
ACRU.C9 <- glm(C ~ GDD * EVI, data = ACRU)
ACRU.C10 <- glm(C ~ GDD * Site, data = ACRU)
ACRU.C11 <- glm(C ~ EVI * Site, data = ACRU)
ACRU.C12 <- glm(C ~ Site, data = ACRU)
ACRU.C13 <- glm(C ~ GDD, data = ACRU)
ACRU.C14 <- glm(C ~ EVI, data = ACRU)  
ACRU.C15 <- glm(C ~ Year, data = ACRU)  
ACRU.C16 <- glm(C ~ 1, data =  ACRU)
Models.ACRU.C <- list("ACRU.C1" = ACRU.C1, "ACRU.C2" = ACRU.C2, "ACRU.C3" = ACRU.C3, "ACRU.C4" = ACRU.C4, "ACRU.C5" = ACRU.C5, "ACRU.C6" = ACRU.C6, "ACRU.C7" = ACRU.C7, "ACRU.C8" =  ACRU.C8, "ACRU.C9" = ACRU.C9, "ACRU.C10" = ACRU.C10, "ACRU.C11" = ACRU.C11, "ACRU.C12" = ACRU.C12, "ACRU.C13" = ACRU.C13, "ACRU.C14" = ACRU.C14, "ACRU.C15" = ACRU.C15, "ACRU.C16" = ACRU.C16)
ACRU.C <- aictab(cand.set = Models.ACRU.C)
print(ACRU.C)
write.csv(ACRU.C, "AIC/Tables/ACRU_C.csv")
summary(ACRU.C1)
PseudoR2(ACRU.C1, which = "Nagelkerke")

#%P
ACRU.P1 <- glm(P ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.P2 <- glm(P ~ Year * EVI * GDD, data = ACRU)
ACRU.P3 <- glm(P~ Year * EVI * Site, data = ACRU)
ACRU.P4 <- glm(P ~ Year * Site * GDD, data = ACRU)
ACRU.P5 <- glm(P ~ EVI * GDD * Site, data = ACRU)
ACRU.P6 <- glm(P ~ Year * GDD, data = ACRU)
ACRU.P7 <- glm(P ~ Year * EVI, data = ACRU)
ACRU.P8 <- glm(P ~ Year * Site, data = ACRU)
ACRU.P9 <- glm(P ~ GDD * EVI, data = ACRU)
ACRU.P10 <- glm(P ~ GDD * Site, data = ACRU)
ACRU.P11 <- glm(P ~ EVI * Site, data = ACRU)
ACRU.P12 <- glm(P ~ Site, data = ACRU)
ACRU.P13 <- glm(P ~ GDD, data = ACRU)
ACRU.P14 <- glm(P ~ EVI, data = ACRU)  
ACRU.P15 <- glm(P ~ Year, data = ACRU)  
ACRU.P16 <- glm(P ~ 1, data =  ACRU)
Models.ACRU.P <- list("ACRU.P1" = ACRU.P1, "ACRU.P2" = ACRU.P2, "ACRU.P3" = ACRU.P3, "ACRU.P4" = ACRU.P4, "ACRU.P5" = ACRU.P5, "ACRU.P6" = ACRU.P6, "ACRU.P7" = ACRU.P7, "ACRU.P8" =  ACRU.P8, "ACRU.P9" = ACRU.P9, "ACRU.P10" = ACRU.P10, "ACRU.P11" = ACRU.P11, "ACRU.P12" = ACRU.P12, "ACRU.P13" = ACRU.P13, "ACRU.P14" = ACRU.P14, "ACRU.P15" = ACRU.P15, "ACRU.P16" = ACRU.P16)
ACRU.P <- aictab(cand.set = Models.ACRU.P)
print(ACRU.P)
write.csv(ACRU.P, "AIC/Tables/ACRU_P.csv")
summary(ACRU.P16)
PseudoR2(ACRU.P16, which = "Nagelkerke")
#%N
ACRU.N1 <- glm(N ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.N2 <- glm(N ~ Year * EVI * GDD, data = ACRU)
ACRU.N3 <- glm(N~ Year * EVI * Site, data = ACRU)
ACRU.N4 <- glm(N ~ Year * Site * GDD, data = ACRU)
ACRU.N5 <- glm(N ~ EVI * GDD * Site, data = ACRU)
ACRU.N6 <- glm(N ~ Year * GDD, data = ACRU)
ACRU.N7 <- glm(N ~ Year * EVI, data = ACRU)
ACRU.N8 <- glm(N ~ Year * Site, data = ACRU)
ACRU.N9 <- glm(N ~ AverageGDD * EVI, data = ACRU)
ACRU.N10 <- glm(N ~ AverageGDD * Site, data = ACRU)
ACRU.N11 <- glm(N ~ EVI * Site, data = ACRU)
ACRU.N12 <- glm(N ~ Site, data = ACRU)
ACRU.N13 <- glm(N ~ GDD, data = ACRU)
ACRU.N14 <- glm(N ~ EVI, data = ACRU)  
ACRU.N15 <- glm(N ~ Year, data = ACRU)  
ACRU.N16 <- glm(N ~ 1, data =  ACRU)
Models.ACRU.N <- list("ACRU.N1" = ACRU.N1, "ACRU.N2" = ACRU.N2, "ACRU.N3" = ACRU.N3, "ACRU.N4" = ACRU.N4, "ACRU.N5" = ACRU.N5, "ACRU.N6" = ACRU.N6, "ACRU.N7" = ACRU.N7, "ACRU.N8" =  ACRU.N8, "ACRU.N9" = ACRU.N9, "ACRU.N10" = ACRU.N10, "ACRU.N11" = ACRU.N11, "ACRU.N12" = ACRU.N12, "ACRU.N13" = ACRU.N13, "ACRU.N14" = ACRU.N14, "ACRU.N15" = ACRU.N15, "ACRU.N16" = ACRU.N16)
ACRU.N <- aictab(cand.set = Models.ACRU.N)
print(ACRU.N)
write.csv(ACRU.N, "AIC/Tables/ACRU_N.csv")
summary(ACRU.N2)
PseudoR2(ACRU.N2, which = "Nagelkerke")
#C:N
ACRU.CN1 <- glm(CNRatio ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.CN2 <- glm(CNRatio ~ Year * EVI * GDD, data = ACRU)
ACRU.CN3 <- glm(CNRatio ~ Year * EVI * Site, data = ACRU)
ACRU.CN4 <- glm(CNRatio ~ Year * Site * GDD, data = ACRU)
ACRU.CN5 <- glm(CNRatio ~ EVI * GDD * Site, data = ACRU)
ACRU.CN6 <- glm(CNRatio ~ Year * GDD, data = ACRU)
ACRU.CN7 <- glm(CNRatio ~ Year * EVI, data = ACRU)
ACRU.CN8 <- glm(CNRatio ~ Year * Site, data = ACRU)
ACRU.CN9 <- glm(CNRatio ~ GDD * EVI, data = ACRU)
ACRU.CN10 <- glm(CNRatio ~ GDD * Site, data = ACRU)
ACRU.CN11 <- glm(CNRatio ~ EVI * Site, data = ACRU)
ACRU.CN12 <- glm(CNRatio ~ Site, data = ACRU)
ACRU.CN13 <- glm(CNRatio ~ GDD, data = ACRU)
ACRU.CN14 <- glm(CNRatio ~ EVI, data = ACRU)  
ACRU.CN15 <- glm(CNRatio ~ Year, data = ACRU)  
ACRU.CN16 <- glm(CNRatio ~ 1, data =  ACRU)
Models.ACRU.CN <- list("ACRU.CN1" = ACRU.CN1, "ACRU.CN2" = ACRU.CN2, "ACRU.CN3" = ACRU.CN3, "ACRU.CN4" = ACRU.CN4, "ACRU.CN5" = ACRU.CN5, "ACRU.CN6" = ACRU.CN6, "ACRU.CN7" = ACRU.CN7, "ACRU.CN8" =  ACRU.CN8, "ACRU.CN9" = ACRU.CN9, "ACRU.CN10" = ACRU.CN10, "ACRU.CN11" = ACRU.CN11, "ACRU.CN12" = ACRU.CN12, "ACRU.CN13" = ACRU.CN13, "ACRU.CN14" = ACRU.CN14, "ACRU.CN15" = ACRU.CN15, "ACRU.CN16" = ACRU.CN16)
ACRU.CN <- aictab(cand.set = Models.ACRU.CN)
print(ACRU.CN)
write.csv(ACRU.CN, "AIC/Tables/ACRU_CN.csv")
summary(ACRU.CN9)
PseudoR2(ACRU.CN9, which = "Nagelkerke")
#C:P
ACRU.CP1 <- glm(CPRatio ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.CP2 <- glm(CPRatio ~ Year * EVI * GDD, data = ACRU)
ACRU.CP3 <- glm(CPRatio ~ Year * EVI * Site, data = ACRU)
ACRU.CP4 <- glm(CPRatio ~ Year * Site * GDD, data = ACRU)
ACRU.CP5 <- glm(CPRatio ~ EVI * GDD * Site, data = ACRU)
ACRU.CP6 <- glm(CPRatio ~ Year * GDD, data = ACRU)
ACRU.CP7 <- glm(CPRatio ~ Year * EVI, data = ACRU)
ACRU.CP8 <- glm(CPRatio ~ Year * Site, data = ACRU)
ACRU.CP9 <- glm(CPRatio ~ GDD * EVI, data = ACRU)
ACRU.CP10 <- glm(CPRatio ~ GDD * Site, data = ACRU)
ACRU.CP11 <- glm(CPRatio ~ EVI * Site, data = ACRU)
ACRU.CP12 <- glm(CPRatio ~ Site, data = ACRU)
ACRU.CP13 <- glm(CPRatio ~ GDD, data = ACRU)
ACRU.CP14 <- glm(CPRatio ~ EVI, data = ACRU)  
ACRU.CP15 <- glm(CPRatio ~ Year, data = ACRU)  
ACRU.CP16 <- glm(CPRatio ~ 1, data =  ACRU)
Models.ACRU.CP <- list("ACRU.CP1" = ACRU.CP1, "ACRU.CP2" = ACRU.CP2, "ACRU.CP3" = ACRU.CP3, "ACRU.CP4" = ACRU.CP4, "ACRU.CP5" = ACRU.CP5, "ACRU.CP6" = ACRU.CP6, "ACRU.CP7" = ACRU.CP7, "ACRU.CP8" =  ACRU.CP8, "ACRU.CP9" = ACRU.CP9, "ACRU.CP10" = ACRU.CP10, "ACRU.CP11" = ACRU.CP11, "ACRU.CP12" = ACRU.CP12, "ACRU.CP13" = ACRU.CP13, "ACRU.CP14" = ACRU.CP14, "ACRU.CP15" = ACRU.CP15, "ACRU.CP16" = ACRU.CP16)
ACRU.CP <- aictab(cand.set = Models.ACRU.CP)
print(ACRU.CP)
write.csv(ACRU.CP, "AIC/Tables/ACRU_CP.csv")
summary(ACRU.CP4)
PseudoR2(ACRU.CP4, which = "Nagelkerke")
#N:P
ACRU.NP1 <- glm(NPRatio ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.NP2 <- glm(NPRatio ~ Year * EVI * GDD, data = ACRU)
ACRU.NP3 <- glm(NPRatio ~ Year * EVI * Site, data = ACRU)
ACRU.NP4 <- glm(NPRatio ~ Year * Site * GDD, data = ACRU)
ACRU.NP5 <- glm(NPRatio ~ EVI * GDD * Site, data = ACRU)
ACRU.NP6 <- glm(NPRatio ~ Year * GDD, data = ACRU)
ACRU.NP7 <- glm(NPRatio ~ Year * EVI, data = ACRU)
ACRU.NP8 <- glm(NPRatio ~ Year * Site, data = ACRU)
ACRU.NP9 <- glm(NPRatio ~ GDD * EVI, data = ACRU)
ACRU.NP10 <- glm(NPRatio ~ GDD * Site, data = ACRU)
ACRU.NP11 <- glm(NPRatio ~ EVI * Site, data = ACRU)
ACRU.NP12 <- glm(NPRatio ~ Site, data = ACRU)
ACRU.NP13 <- glm(NPRatio ~ GDD, data = ACRU)
ACRU.NP14 <- glm(NPRatio ~ EVI, data = ACRU)  
ACRU.NP15 <- glm(NPRatio ~ Year, data = ACRU)  
ACRU.NP16 <- glm(NPRatio ~ 1, data =  ACRU)
Models.ACRU.NP <- list("ACRU.NP1" = ACRU.NP1, "ACRU.NP2" = ACRU.NP2, "ACRU.NP3" = ACRU.NP3, "ACRU.NP4" = ACRU.NP4, "ACRU.NP5" = ACRU.NP5, "ACRU.NP6" = ACRU.NP6, "ACRU.NP7" = ACRU.NP7, "ACRU.NP8" =  ACRU.NP8, "ACRU.NP9" = ACRU.NP9, "ACRU.NP10" = ACRU.NP10, "ACRU.NP11" = ACRU.NP11, "ACRU.NP12" = ACRU.NP12, "ACRU.NP13" = ACRU.NP13, "ACRU.NP14" = ACRU.NP14, "ACRU.NP15" = ACRU.NP15, "ACRU.NP16" = ACRU.NP16)
ACRU.NP <- aictab(cand.set = Models.ACRU.NP)
print(ACRU.NP)
write.csv(ACRU.NP, "AIC/Tables/ACRU_NP.csv")
summary(ACRU.NP11)
PseudoR2(ACRU.NP11, which = "Nagelkerke")
#Qty_C
ACRU.Qty_C1 <- glm(Qty_C ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.Qty_C2 <- glm(Qty_C ~ Year * EVI * GDD, data = ACRU)
ACRU.Qty_C3 <- glm(Qty_C ~ Year * EVI * Site, data = ACRU)
ACRU.Qty_C4 <- glm(Qty_C ~ Year * Site * GDD, data = ACRU)
ACRU.Qty_C5 <- glm(Qty_C ~ EVI * GDD * Site, data = ACRU)
ACRU.Qty_C6 <- glm(Qty_C ~ Year * GDD, data = ACRU)
ACRU.Qty_C7 <- glm(Qty_C ~ Year * EVI, data = ACRU)
ACRU.Qty_C8 <- glm(Qty_C ~ Year * Site, data = ACRU)
ACRU.Qty_C9 <- glm(Qty_C ~ GDD * EVI, data = ACRU)
ACRU.Qty_C10 <- glm(Qty_C ~ GDD * Site, data = ACRU)
ACRU.Qty_C11 <- glm(Qty_C ~ EVI * Site, data = ACRU)
ACRU.Qty_C12 <- glm(Qty_C ~ Site, data = ACRU)
ACRU.Qty_C13 <- glm(Qty_C ~ GDD, data = ACRU)
ACRU.Qty_C14 <- glm(Qty_C ~ EVI, data = ACRU)  
ACRU.Qty_C15 <- glm(Qty_C ~ Year, data = ACRU)  
ACRU.Qty_C16 <- glm(Qty_C ~ 1, data =  ACRU)
Models.ACRU.Qty_C <- list("ACRU.Qty_C1" = ACRU.Qty_C1, "ACRU.Qty_C2" = ACRU.Qty_C2, "ACRU.Qty_C3" = ACRU.Qty_C3, "ACRU.Qty_C4" = ACRU.Qty_C4, "ACRU.Qty_C5" = ACRU.Qty_C5, "ACRU.Qty_C6" = ACRU.Qty_C6, "ACRU.Qty_C7" = ACRU.Qty_C7, "ACRU.Qty_C8" =  ACRU.Qty_C8, "ACRU.Qty_C9" = ACRU.Qty_C9, "ACRU.Qty_C10" = ACRU.Qty_C10, "ACRU.Qty_C11" = ACRU.Qty_C11, "ACRU.Qty_C12" = ACRU.Qty_C12, "ACRU.Qty_C13" = ACRU.Qty_C13, "ACRU.Qty_C14" = ACRU.Qty_C14, "ACRU.Qty_C15" = ACRU.Qty_C15, "ACRU.Qty_C16" = ACRU.Qty_C16)
ACRU.Qty_C <- aictab(cand.set = Models.ACRU.Qty_C)
print(ACRU.Qty_C)
write.csv(ACRU.Qty_C, "AIC/Tables/ACRU_Qty_C.csv")
summary(ACRU.Qty_C2)
PseudoR2(ACRU.Qty_C2, which = "Nagelkerke")
#Qty_P
ACRU.Qty_P1 <- glm(Qty_P ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.Qty_P2 <- glm(Qty_P ~ Year * EVI * GDD, data = ACRU)
ACRU.Qty_P3 <- glm(Qty_P ~ Year * EVI * Site, data = ACRU)
ACRU.Qty_P4 <- glm(Qty_P ~ Year * Site * GDD, data = ACRU)
ACRU.Qty_P5 <- glm(Qty_P ~ EVI * GDD * Site, data = ACRU)
ACRU.Qty_P6 <- glm(Qty_P ~ Year * GDD, data = ACRU)
ACRU.Qty_P7 <- glm(Qty_P ~ Year * EVI, data = ACRU)
ACRU.Qty_P8 <- glm(Qty_P ~ Year * Site, data = ACRU)
ACRU.Qty_P9 <- glm(Qty_P ~ GDD * EVI, data = ACRU)
ACRU.Qty_P10 <- glm(Qty_P ~ GDD * Site, data = ACRU)
ACRU.Qty_P11 <- glm(Qty_P ~ EVI * Site, data = ACRU)
ACRU.Qty_P12 <- glm(Qty_P ~ Site, data = ACRU)
ACRU.Qty_P13 <- glm(Qty_P ~ GDD, data = ACRU)
ACRU.Qty_P14 <- glm(Qty_P ~ EVI, data = ACRU)  
ACRU.Qty_P15 <- glm(Qty_P ~ Year, data = ACRU)  
ACRU.Qty_P16 <- glm(Qty_P ~ 1, data =  ACRU)
Models.ACRU.Qty_P <- list("ACRU.Qty_P1" = ACRU.Qty_P1, "ACRU.Qty_P2" = ACRU.Qty_P2, "ACRU.Qty_P3" = ACRU.Qty_P3, "ACRU.Qty_P4" = ACRU.Qty_P4, "ACRU.Qty_P5" = ACRU.Qty_P5, "ACRU.Qty_P6" = ACRU.Qty_P6, "ACRU.Qty_P7" = ACRU.Qty_P7, "ACRU.Qty_P8" =  ACRU.Qty_P8, "ACRU.Qty_P9" = ACRU.Qty_P9, "ACRU.Qty_P10" = ACRU.Qty_P10, "ACRU.Qty_P11" = ACRU.Qty_P11, "ACRU.Qty_P12" = ACRU.Qty_P12, "ACRU.Qty_P13" = ACRU.Qty_P13, "ACRU.Qty_P14" = ACRU.Qty_P14, "ACRU.Qty_P15" = ACRU.Qty_P15, "ACRU.Qty_P16" = ACRU.Qty_P16)
ACRU.Qty_P <- aictab(cand.set = Models.ACRU.Qty_P)
print(ACRU.Qty_P)
write.csv(ACRU.Qty_P, "AIC/Tables/ACRU_Qty_P.csv")
summary(ACRU.Qty_P2)
PseudoR2(ACRU.Qty_P2, which = "Nagelkerke")
#Qty_N
ACRU.Qty_N1 <- glm(Qty_N ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.Qty_N2 <- glm(Qty_N ~ Year * EVI * GDD, data = ACRU)
ACRU.Qty_N3 <- glm(Qty_N ~ Year * EVI * Site, data = ACRU)
ACRU.Qty_N4 <- glm(Qty_N ~ Year * Site * GDD, data = ACRU)
ACRU.Qty_N5 <- glm(Qty_N ~ EVI * GDD * Site, data = ACRU)
ACRU.Qty_N6 <- glm(Qty_N ~ Year * GDD, data = ACRU)
ACRU.Qty_N7 <- glm(Qty_N ~ Year * EVI, data = ACRU)
ACRU.Qty_N8 <- glm(Qty_N ~ Year * Site, data = ACRU)
ACRU.Qty_N9 <- glm(Qty_N ~ GDD * EVI, data = ACRU)
ACRU.Qty_N10 <- glm(Qty_N ~ GDD * Site, data = ACRU)
ACRU.Qty_N11 <- glm(Qty_N ~ EVI * Site, data = ACRU)
ACRU.Qty_N12 <- glm(Qty_N ~ Site, data = ACRU)
ACRU.Qty_N13 <- glm(Qty_N ~ GDD, data = ACRU)
ACRU.Qty_N14 <- glm(Qty_N ~ EVI, data = ACRU)  
ACRU.Qty_N15 <- glm(Qty_N ~ Year, data = ACRU)  
ACRU.Qty_N16 <- glm(Qty_N ~ 1, data =  ACRU)
Models.ACRU.Qty_N <- list("ACRU.Qty_N1" = ACRU.Qty_N1, "ACRU.Qty_N2" = ACRU.Qty_N2, "ACRU.Qty_N3" = ACRU.Qty_N3, "ACRU.Qty_N4" = ACRU.Qty_N4, "ACRU.Qty_N5" = ACRU.Qty_N5, "ACRU.Qty_N6" = ACRU.Qty_N6, "ACRU.Qty_N7" = ACRU.Qty_N7, "ACRU.Qty_N8" =  ACRU.Qty_N8, "ACRU.Qty_N9" = ACRU.Qty_N9, "ACRU.Qty_N10" = ACRU.Qty_N10, "ACRU.Qty_N11" = ACRU.Qty_N11, "ACRU.Qty_N12" = ACRU.Qty_N12, "ACRU.Qty_N13" = ACRU.Qty_N13, "ACRU.Qty_N14" = ACRU.Qty_N14, "ACRU.Qty_N15" = ACRU.Qty_N15, "ACRU.Qty_N16" = ACRU.Qty_N16)
ACRU.Qty_N <- aictab(cand.set = Models.ACRU.Qty_N)
print(ACRU.Qty_N)
write.csv(ACRU.Qty_N, "AIC/Tables/ACRU_Qty_N.csv")
summary(ACRU.Qty_N2)
PseudoR2(ACRU.Qty_N2, which = "Nagelkerke")

#BEPA
#%C
BEPA.C1 <- glm(C ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.C2 <- glm(C ~ Year * EVI * GDD, data = BEPA)
BEPA.C3 <- glm(C~ Year * EVI * Site, data = BEPA)
BEPA.C4 <- glm(C ~ Year * Site * GDD, data = BEPA)
BEPA.C5 <- glm(C ~ EVI * GDD * Site, data = BEPA)
BEPA.C6 <- glm(C ~ Year * GDD, data = BEPA)
BEPA.C7 <- glm(C ~ Year * EVI, data = BEPA)
BEPA.C8 <- glm(C ~ Year * Site, data = BEPA)
BEPA.C9 <- glm(C ~ GDD * EVI, data = BEPA)
BEPA.C10 <- glm(C ~ GDD * Site, data = BEPA)
BEPA.C11 <- glm(C ~ EVI * Site, data = BEPA)
BEPA.C12 <- glm(C ~ Site, data = BEPA)
BEPA.C13 <- glm(C ~ GDD, data = BEPA)
BEPA.C14 <- glm(C ~ EVI, data = BEPA)  
BEPA.C15 <- glm(C ~ Year, data = BEPA)  
BEPA.C16 <- glm(C ~ 1, data =  BEPA)
Models.BEPA.C <- list("BEPA.C1" = BEPA.C1, "BEPA.C2" = BEPA.C2, "BEPA.C3" = BEPA.C3, "BEPA.C4" = BEPA.C4, "BEPA.C5" = BEPA.C5, "BEPA.C6" = BEPA.C6, "BEPA.C7" = BEPA.C7, "BEPA.C8" =  BEPA.C8, "BEPA.C9" = BEPA.C9, "BEPA.C10" = BEPA.C10, "BEPA.C11" = BEPA.C11, "BEPA.C12" = BEPA.C12, "BEPA.C13" = BEPA.C13, "BEPA.C14" = BEPA.C14, "BEPA.C15" = BEPA.C15, "BEPA.C16" = BEPA.C16)
BEPA.C <- aictab(cand.set = Models.BEPA.C)
print(BEPA.C)
write.csv(BEPA.C, "AIC/Tables/BEPA_C.csv")
summary(BEPA.C4)
PseudoR2(BEPA.C4, which = "Nagelkerke")

#%P
BEPA.P1 <- glm(P ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.P2 <- glm(P ~ Year * EVI * GDD, data = BEPA)
BEPA.P3 <- glm(P~ Year * EVI * Site, data = BEPA)
BEPA.P4 <- glm(P ~ Year * Site * GDD, data = BEPA)
BEPA.P5 <- glm(P ~ EVI * GDD * Site, data = BEPA)
BEPA.P6 <- glm(P ~ Year * GDD, data = BEPA)
BEPA.P7 <- glm(P ~ Year * EVI, data = BEPA)
BEPA.P8 <- glm(P ~ Year * Site, data = BEPA)
BEPA.P9 <- glm(P ~ GDD * EVI, data = BEPA)
BEPA.P10 <- glm(P ~ GDD * Site, data = BEPA)
BEPA.P11 <- glm(P ~ EVI * Site, data = BEPA)
BEPA.P12 <- glm(P ~ Site, data = BEPA)
BEPA.P13 <- glm(P ~ GDD, data = BEPA)
BEPA.P14 <- glm(P ~ EVI, data = BEPA)  
BEPA.P15 <- glm(P ~ Year, data = BEPA)  
BEPA.P16 <- glm(P ~ 1, data =  BEPA)
Models.BEPA.P <- list("BEPA.P1" = BEPA.P1, "BEPA.P2" = BEPA.P2, "BEPA.P3" = BEPA.P3, "BEPA.P4" = BEPA.P4, "BEPA.P5" = BEPA.P5, "BEPA.P6" = BEPA.P6, "BEPA.P7" = BEPA.P7, "BEPA.P8" =  BEPA.P8, "BEPA.P9" = BEPA.P9, "BEPA.P10" = BEPA.P10, "BEPA.P11" = BEPA.P11, "BEPA.P12" = BEPA.P12, "BEPA.P13" = BEPA.P13, "BEPA.P14" = BEPA.P14, "BEPA.P15" = BEPA.P15, "BEPA.P16" = BEPA.P16)
BEPA.P <- aictab(cand.set = Models.BEPA.P)
print(BEPA.P)
write.csv(BEPA.P, "AIC/Tables/BEPA_P.csv")
summary(BEPA.P16)
PseudoR2(BEPA.P16, which = "Nagelkerke")
#%N
BEPA.N1 <- glm(N ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.N2 <- glm(N ~ Year * EVI * GDD, data = BEPA)
BEPA.N3 <- glm(N~ Year * EVI * Site, data = BEPA)
BEPA.N4 <- glm(N ~ Year * Site * GDD, data = BEPA)
BEPA.N5 <- glm(N ~ EVI * GDD * Site, data = BEPA)
BEPA.N6 <- glm(N ~ Year * GDD, data = BEPA)
BEPA.N7 <- glm(N ~ Year * EVI, data = BEPA)
BEPA.N8 <- glm(N ~ Year * Site, data = BEPA)
BEPA.N9 <- glm(N ~ AverageGDD * EVI, data = BEPA)
BEPA.N10 <- glm(N ~ AverageGDD * Site, data = BEPA)
BEPA.N11 <- glm(N ~ EVI * Site, data = BEPA)
BEPA.N12 <- glm(N ~ Site, data = BEPA)
BEPA.N13 <- glm(N ~ GDD, data = BEPA)
BEPA.N14 <- glm(N ~ EVI, data = BEPA)  
BEPA.N15 <- glm(N ~ Year, data = BEPA)  
BEPA.N16 <- glm(N ~ 1, data =  BEPA)
Models.BEPA.N <- list("BEPA.N1" = BEPA.N1, "BEPA.N2" = BEPA.N2, "BEPA.N3" = BEPA.N3, "BEPA.N4" = BEPA.N4, "BEPA.N5" = BEPA.N5, "BEPA.N6" = BEPA.N6, "BEPA.N7" = BEPA.N7, "BEPA.N8" =  BEPA.N8, "BEPA.N9" = BEPA.N9, "BEPA.N10" = BEPA.N10, "BEPA.N11" = BEPA.N11, "BEPA.N12" = BEPA.N12, "BEPA.N13" = BEPA.N13, "BEPA.N14" = BEPA.N14, "BEPA.N15" = BEPA.N15, "BEPA.N16" = BEPA.N16)
BEPA.N <- aictab(cand.set = Models.BEPA.N)
print(BEPA.N)
write.csv(BEPA.N, "AIC/Tables/BEPA_N.csv")
summary(BEPA.N2)
PseudoR2(BEPA.N2, which = "Nagelkerke")
#C:N
BEPA.CN1 <- glm(CNRatio ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.CN2 <- glm(CNRatio ~ Year * EVI * GDD, data = BEPA)
BEPA.CN3 <- glm(CNRatio ~ Year * EVI * Site, data = BEPA)
BEPA.CN4 <- glm(CNRatio ~ Year * Site * GDD, data = BEPA)
BEPA.CN5 <- glm(CNRatio ~ EVI * GDD * Site, data = BEPA)
BEPA.CN6 <- glm(CNRatio ~ Year * GDD, data = BEPA)
BEPA.CN7 <- glm(CNRatio ~ Year * EVI, data = BEPA)
BEPA.CN8 <- glm(CNRatio ~ Year * Site, data = BEPA)
BEPA.CN9 <- glm(CNRatio ~ GDD * EVI, data = BEPA)
BEPA.CN10 <- glm(CNRatio ~ GDD * Site, data = BEPA)
BEPA.CN11 <- glm(CNRatio ~ EVI * Site, data = BEPA)
BEPA.CN12 <- glm(CNRatio ~ Site, data = BEPA)
BEPA.CN13 <- glm(CNRatio ~ GDD, data = BEPA)
BEPA.CN14 <- glm(CNRatio ~ EVI, data = BEPA)  
BEPA.CN15 <- glm(CNRatio ~ Year, data = BEPA)  
BEPA.CN16 <- glm(CNRatio ~ 1, data =  BEPA)
Models.BEPA.CN <- list("BEPA.CN1" = BEPA.CN1, "BEPA.CN2" = BEPA.CN2, "BEPA.CN3" = BEPA.CN3, "BEPA.CN4" = BEPA.CN4, "BEPA.CN5" = BEPA.CN5, "BEPA.CN6" = BEPA.CN6, "BEPA.CN7" = BEPA.CN7, "BEPA.CN8" =  BEPA.CN8, "BEPA.CN9" = BEPA.CN9, "BEPA.CN10" = BEPA.CN10, "BEPA.CN11" = BEPA.CN11, "BEPA.CN12" = BEPA.CN12, "BEPA.CN13" = BEPA.CN13, "BEPA.CN14" = BEPA.CN14, "BEPA.CN15" = BEPA.CN15, "BEPA.CN16" = BEPA.CN16)
BEPA.CN <- aictab(cand.set = Models.BEPA.CN)
print(BEPA.CN)
write.csv(BEPA.CN, "AIC/Tables/BEPA_CN.csv")
summary(BEPA.CN9)
PseudoR2(BEPA.CN9, which = "Nagelkerke")
#C:P
BEPA.CP1 <- glm(CPRatio ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.CP2 <- glm(CPRatio ~ Year * EVI * GDD, data = BEPA)
BEPA.CP3 <- glm(CPRatio ~ Year * EVI * Site, data = BEPA)
BEPA.CP4 <- glm(CPRatio ~ Year * Site * GDD, data = BEPA)
BEPA.CP5 <- glm(CPRatio ~ EVI * GDD * Site, data = BEPA)
BEPA.CP6 <- glm(CPRatio ~ Year * GDD, data = BEPA)
BEPA.CP7 <- glm(CPRatio ~ Year * EVI, data = BEPA)
BEPA.CP8 <- glm(CPRatio ~ Year * Site, data = BEPA)
BEPA.CP9 <- glm(CPRatio ~ GDD * EVI, data = BEPA)
BEPA.CP10 <- glm(CPRatio ~ GDD * Site, data = BEPA)
BEPA.CP11 <- glm(CPRatio ~ EVI * Site, data = BEPA)
BEPA.CP12 <- glm(CPRatio ~ Site, data = BEPA)
BEPA.CP13 <- glm(CPRatio ~ GDD, data = BEPA)
BEPA.CP14 <- glm(CPRatio ~ EVI, data = BEPA)  
BEPA.CP15 <- glm(CPRatio ~ Year, data = BEPA)  
BEPA.CP16 <- glm(CPRatio ~ 1, data =  BEPA)
Models.BEPA.CP <- list("BEPA.CP1" = BEPA.CP1, "BEPA.CP2" = BEPA.CP2, "BEPA.CP3" = BEPA.CP3, "BEPA.CP4" = BEPA.CP4, "BEPA.CP5" = BEPA.CP5, "BEPA.CP6" = BEPA.CP6, "BEPA.CP7" = BEPA.CP7, "BEPA.CP8" =  BEPA.CP8, "BEPA.CP9" = BEPA.CP9, "BEPA.CP10" = BEPA.CP10, "BEPA.CP11" = BEPA.CP11, "BEPA.CP12" = BEPA.CP12, "BEPA.CP13" = BEPA.CP13, "BEPA.CP14" = BEPA.CP14, "BEPA.CP15" = BEPA.CP15, "BEPA.CP16" = BEPA.CP16)
BEPA.CP <- aictab(cand.set = Models.BEPA.CP)
print(BEPA.CP)
write.csv(BEPA.CP, "AIC/Tables/BEPA_CP.csv")
summary(BEPA.CP4)
PseudoR2(BEPA.CP4, which = "Nagelkerke")
#N:P
BEPA.NP1 <- glm(NPRatio ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.NP2 <- glm(NPRatio ~ Year * EVI * GDD, data = BEPA)
BEPA.NP3 <- glm(NPRatio ~ Year * EVI * Site, data = BEPA)
BEPA.NP4 <- glm(NPRatio ~ Year * Site * GDD, data = BEPA)
BEPA.NP5 <- glm(NPRatio ~ EVI * GDD * Site, data = BEPA)
BEPA.NP6 <- glm(NPRatio ~ Year * GDD, data = BEPA)
BEPA.NP7 <- glm(NPRatio ~ Year * EVI, data = BEPA)
BEPA.NP8 <- glm(NPRatio ~ Year * Site, data = BEPA)
BEPA.NP9 <- glm(NPRatio ~ GDD * EVI, data = BEPA)
BEPA.NP10 <- glm(NPRatio ~ GDD * Site, data = BEPA)
BEPA.NP11 <- glm(NPRatio ~ EVI * Site, data = BEPA)
BEPA.NP12 <- glm(NPRatio ~ Site, data = BEPA)
BEPA.NP13 <- glm(NPRatio ~ GDD, data = BEPA)
BEPA.NP14 <- glm(NPRatio ~ EVI, data = BEPA)  
BEPA.NP15 <- glm(NPRatio ~ Year, data = BEPA)  
BEPA.NP16 <- glm(NPRatio ~ 1, data =  BEPA)
Models.BEPA.NP <- list("BEPA.NP1" = BEPA.NP1, "BEPA.NP2" = BEPA.NP2, "BEPA.NP3" = BEPA.NP3, "BEPA.NP4" = BEPA.NP4, "BEPA.NP5" = BEPA.NP5, "BEPA.NP6" = BEPA.NP6, "BEPA.NP7" = BEPA.NP7, "BEPA.NP8" =  BEPA.NP8, "BEPA.NP9" = BEPA.NP9, "BEPA.NP10" = BEPA.NP10, "BEPA.NP11" = BEPA.NP11, "BEPA.NP12" = BEPA.NP12, "BEPA.NP13" = BEPA.NP13, "BEPA.NP14" = BEPA.NP14, "BEPA.NP15" = BEPA.NP15, "BEPA.NP16" = BEPA.NP16)
BEPA.NP <- aictab(cand.set = Models.BEPA.NP)
print(BEPA.NP)
write.csv(BEPA.NP, "AIC/Tables/BEPA_NP.csv")
summary(BEPA.NP11)
PseudoR2(BEPA.NP11, which = "Nagelkerke")
#Qty_C
BEPA.Qty_C1 <- glm(Qty_C ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.Qty_C2 <- glm(Qty_C ~ Year * EVI * GDD, data = BEPA)
BEPA.Qty_C3 <- glm(Qty_C ~ Year * EVI * Site, data = BEPA)
BEPA.Qty_C4 <- glm(Qty_C ~ Year * Site * GDD, data = BEPA)
BEPA.Qty_C5 <- glm(Qty_C ~ EVI * GDD * Site, data = BEPA)
BEPA.Qty_C6 <- glm(Qty_C ~ Year * GDD, data = BEPA)
BEPA.Qty_C7 <- glm(Qty_C ~ Year * EVI, data = BEPA)
BEPA.Qty_C8 <- glm(Qty_C ~ Year * Site, data = BEPA)
BEPA.Qty_C9 <- glm(Qty_C ~ GDD * EVI, data = BEPA)
BEPA.Qty_C10 <- glm(Qty_C ~ GDD * Site, data = BEPA)
BEPA.Qty_C11 <- glm(Qty_C ~ EVI * Site, data = BEPA)
BEPA.Qty_C12 <- glm(Qty_C ~ Site, data = BEPA)
BEPA.Qty_C13 <- glm(Qty_C ~ GDD, data = BEPA)
BEPA.Qty_C14 <- glm(Qty_C ~ EVI, data = BEPA)  
BEPA.Qty_C15 <- glm(Qty_C ~ Year, data = BEPA)  
BEPA.Qty_C16 <- glm(Qty_C ~ 1, data =  BEPA)
Models.BEPA.Qty_C <- list("BEPA.Qty_C1" = BEPA.Qty_C1, "BEPA.Qty_C2" = BEPA.Qty_C2, "BEPA.Qty_C3" = BEPA.Qty_C3, "BEPA.Qty_C4" = BEPA.Qty_C4, "BEPA.Qty_C5" = BEPA.Qty_C5, "BEPA.Qty_C6" = BEPA.Qty_C6, "BEPA.Qty_C7" = BEPA.Qty_C7, "BEPA.Qty_C8" =  BEPA.Qty_C8, "BEPA.Qty_C9" = BEPA.Qty_C9, "BEPA.Qty_C10" = BEPA.Qty_C10, "BEPA.Qty_C11" = BEPA.Qty_C11, "BEPA.Qty_C12" = BEPA.Qty_C12, "BEPA.Qty_C13" = BEPA.Qty_C13, "BEPA.Qty_C14" = BEPA.Qty_C14, "BEPA.Qty_C15" = BEPA.Qty_C15, "BEPA.Qty_C16" = BEPA.Qty_C16)
BEPA.Qty_C <- aictab(cand.set = Models.BEPA.Qty_C)
print(BEPA.Qty_C)
write.csv(BEPA.Qty_C, "AIC/Tables/BEPA_Qty_C.csv")
summary(BEPA.Qty_C2)
PseudoR2(BEPA.Qty_C2, which = "Nagelkerke")
#Qty_P
BEPA.Qty_P1 <- glm(Qty_P ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.Qty_P2 <- glm(Qty_P ~ Year * EVI * GDD, data = BEPA)
BEPA.Qty_P3 <- glm(Qty_P ~ Year * EVI * Site, data = BEPA)
BEPA.Qty_P4 <- glm(Qty_P ~ Year * Site * GDD, data = BEPA)
BEPA.Qty_P5 <- glm(Qty_P ~ EVI * GDD * Site, data = BEPA)
BEPA.Qty_P6 <- glm(Qty_P ~ Year * GDD, data = BEPA)
BEPA.Qty_P7 <- glm(Qty_P ~ Year * EVI, data = BEPA)
BEPA.Qty_P8 <- glm(Qty_P ~ Year * Site, data = BEPA)
BEPA.Qty_P9 <- glm(Qty_P ~ GDD * EVI, data = BEPA)
BEPA.Qty_P10 <- glm(Qty_P ~ GDD * Site, data = BEPA)
BEPA.Qty_P11 <- glm(Qty_P ~ EVI * Site, data = BEPA)
BEPA.Qty_P12 <- glm(Qty_P ~ Site, data = BEPA)
BEPA.Qty_P13 <- glm(Qty_P ~ GDD, data = BEPA)
BEPA.Qty_P14 <- glm(Qty_P ~ EVI, data = BEPA)  
BEPA.Qty_P15 <- glm(Qty_P ~ Year, data = BEPA)  
BEPA.Qty_P16 <- glm(Qty_P ~ 1, data =  BEPA)
Models.BEPA.Qty_P <- list("BEPA.Qty_P1" = BEPA.Qty_P1, "BEPA.Qty_P2" = BEPA.Qty_P2, "BEPA.Qty_P3" = BEPA.Qty_P3, "BEPA.Qty_P4" = BEPA.Qty_P4, "BEPA.Qty_P5" = BEPA.Qty_P5, "BEPA.Qty_P6" = BEPA.Qty_P6, "BEPA.Qty_P7" = BEPA.Qty_P7, "BEPA.Qty_P8" =  BEPA.Qty_P8, "BEPA.Qty_P9" = BEPA.Qty_P9, "BEPA.Qty_P10" = BEPA.Qty_P10, "BEPA.Qty_P11" = BEPA.Qty_P11, "BEPA.Qty_P12" = BEPA.Qty_P12, "BEPA.Qty_P13" = BEPA.Qty_P13, "BEPA.Qty_P14" = BEPA.Qty_P14, "BEPA.Qty_P15" = BEPA.Qty_P15, "BEPA.Qty_P16" = BEPA.Qty_P16)
BEPA.Qty_P <- aictab(cand.set = Models.BEPA.Qty_P)
print(BEPA.Qty_P)
write.csv(BEPA.Qty_P, "AIC/Tables/BEPA_Qty_P.csv")
summary(BEPA.Qty_P2)
PseudoR2(BEPA.Qty_P2, which = "Nagelkerke")
#Qty_N
BEPA.Qty_N1 <- glm(Qty_N ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.Qty_N2 <- glm(Qty_N ~ Year * EVI * GDD, data = BEPA)
BEPA.Qty_N3 <- glm(Qty_N ~ Year * EVI * Site, data = BEPA)
BEPA.Qty_N4 <- glm(Qty_N ~ Year * Site * GDD, data = BEPA)
BEPA.Qty_N5 <- glm(Qty_N ~ EVI * GDD * Site, data = BEPA)
BEPA.Qty_N6 <- glm(Qty_N ~ Year * GDD, data = BEPA)
BEPA.Qty_N7 <- glm(Qty_N ~ Year * EVI, data = BEPA)
BEPA.Qty_N8 <- glm(Qty_N ~ Year * Site, data = BEPA)
BEPA.Qty_N9 <- glm(Qty_N ~ GDD * EVI, data = BEPA)
BEPA.Qty_N10 <- glm(Qty_N ~ GDD * Site, data = BEPA)
BEPA.Qty_N11 <- glm(Qty_N ~ EVI * Site, data = BEPA)
BEPA.Qty_N12 <- glm(Qty_N ~ Site, data = BEPA)
BEPA.Qty_N13 <- glm(Qty_N ~ GDD, data = BEPA)
BEPA.Qty_N14 <- glm(Qty_N ~ EVI, data = BEPA)  
BEPA.Qty_N15 <- glm(Qty_N ~ Year, data = BEPA)  
BEPA.Qty_N16 <- glm(Qty_N ~ 1, data =  BEPA)
Models.BEPA.Qty_N <- list("BEPA.Qty_N1" = BEPA.Qty_N1, "BEPA.Qty_N2" = BEPA.Qty_N2, "BEPA.Qty_N3" = BEPA.Qty_N3, "BEPA.Qty_N4" = BEPA.Qty_N4, "BEPA.Qty_N5" = BEPA.Qty_N5, "BEPA.Qty_N6" = BEPA.Qty_N6, "BEPA.Qty_N7" = BEPA.Qty_N7, "BEPA.Qty_N8" =  BEPA.Qty_N8, "BEPA.Qty_N9" = BEPA.Qty_N9, "BEPA.Qty_N10" = BEPA.Qty_N10, "BEPA.Qty_N11" = BEPA.Qty_N11, "BEPA.Qty_N12" = BEPA.Qty_N12, "BEPA.Qty_N13" = BEPA.Qty_N13, "BEPA.Qty_N14" = BEPA.Qty_N14, "BEPA.Qty_N15" = BEPA.Qty_N15, "BEPA.Qty_N16" = BEPA.Qty_N16)
BEPA.Qty_N <- aictab(cand.set = Models.BEPA.Qty_N)
print(BEPA.Qty_N)
write.csv(BEPA.Qty_N, "AIC/Tables/BEPA_Qty_N.csv")
summary(BEPA.Qty_N2)
PseudoR2(BEPA.Qty_N2, which = "Nagelkerke")

#VAAN
#%C
VAAN.C1 <- glm(C ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.C2 <- glm(C ~ Year * EVI * GDD, data = VAAN)
VAAN.C3 <- glm(C~ Year * EVI * Site, data = VAAN)
VAAN.C4 <- glm(C ~ Year * Site * GDD, data = VAAN)
VAAN.C5 <- glm(C ~ EVI * GDD * Site, data = VAAN)
VAAN.C6 <- glm(C ~ Year * GDD, data = VAAN)
VAAN.C7 <- glm(C ~ Year * EVI, data = VAAN)
VAAN.C8 <- glm(C ~ Year * Site, data = VAAN)
VAAN.C9 <- glm(C ~ GDD * EVI, data = VAAN)
VAAN.C10 <- glm(C ~ GDD * Site, data = VAAN)
VAAN.C11 <- glm(C ~ EVI * Site, data = VAAN)
VAAN.C12 <- glm(C ~ Site, data = VAAN)
VAAN.C13 <- glm(C ~ GDD, data = VAAN)
VAAN.C14 <- glm(C ~ EVI, data = VAAN)  
VAAN.C15 <- glm(C ~ Year, data = VAAN)  
VAAN.C16 <- glm(C ~ 1, data =  VAAN)
Models.VAAN.C <- list("VAAN.C1" = VAAN.C1, "VAAN.C2" = VAAN.C2, "VAAN.C3" = VAAN.C3, "VAAN.C4" = VAAN.C4, "VAAN.C5" = VAAN.C5, "VAAN.C6" = VAAN.C6, "VAAN.C7" = VAAN.C7, "VAAN.C8" =  VAAN.C8, "VAAN.C9" = VAAN.C9, "VAAN.C10" = VAAN.C10, "VAAN.C11" = VAAN.C11, "VAAN.C12" = VAAN.C12, "VAAN.C13" = VAAN.C13, "VAAN.C14" = VAAN.C14, "VAAN.C15" = VAAN.C15, "VAAN.C16" = VAAN.C16)
VAAN.C <- aictab(cand.set = Models.VAAN.C)
print(VAAN.C)
write.csv(VAAN.C, "AIC/Tables/VAAN_C.csv")
summary(VAAN.C8)
PseudoR2(VAAN.C8, which = "Nagelkerke")

#%P
VAAN.P1 <- glm(P ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.P2 <- glm(P ~ Year * EVI * GDD, data = VAAN)
VAAN.P3 <- glm(P~ Year * EVI * Site, data = VAAN)
VAAN.P4 <- glm(P ~ Year * Site * GDD, data = VAAN)
VAAN.P5 <- glm(P ~ EVI * GDD * Site, data = VAAN)
VAAN.P6 <- glm(P ~ Year * GDD, data = VAAN)
VAAN.P7 <- glm(P ~ Year * EVI, data = VAAN)
VAAN.P8 <- glm(P ~ Year * Site, data = VAAN)
VAAN.P9 <- glm(P ~ GDD * EVI, data = VAAN)
VAAN.P10 <- glm(P ~ GDD * Site, data = VAAN)
VAAN.P11 <- glm(P ~ EVI * Site, data = VAAN)
VAAN.P12 <- glm(P ~ Site, data = VAAN)
VAAN.P13 <- glm(P ~ GDD, data = VAAN)
VAAN.P14 <- glm(P ~ EVI, data = VAAN)  
VAAN.P15 <- glm(P ~ Year, data = VAAN)  
VAAN.P16 <- glm(P ~ 1, data =  VAAN)
Models.VAAN.P <- list("VAAN.P1" = VAAN.P1, "VAAN.P2" = VAAN.P2, "VAAN.P3" = VAAN.P3, "VAAN.P4" = VAAN.P4, "VAAN.P5" = VAAN.P5, "VAAN.P6" = VAAN.P6, "VAAN.P7" = VAAN.P7, "VAAN.P8" =  VAAN.P8, "VAAN.P9" = VAAN.P9, "VAAN.P10" = VAAN.P10, "VAAN.P11" = VAAN.P11, "VAAN.P12" = VAAN.P12, "VAAN.P13" = VAAN.P13, "VAAN.P14" = VAAN.P14, "VAAN.P15" = VAAN.P15, "VAAN.P16" = VAAN.P16)
VAAN.P <- aictab(cand.set = Models.VAAN.P)
print(VAAN.P)
write.csv(VAAN.P, "AIC/Tables/VAAN_P.csv")
summary(VAAN.P16)
PseudoR2(VAAN.P16, which = "Nagelkerke")
#%N
VAAN.N1 <- glm(N ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.N2 <- glm(N ~ Year * EVI * GDD, data = VAAN)
VAAN.N3 <- glm(N~ Year * EVI * Site, data = VAAN)
VAAN.N4 <- glm(N ~ Year * Site * GDD, data = VAAN)
VAAN.N5 <- glm(N ~ EVI * GDD * Site, data = VAAN)
VAAN.N6 <- glm(N ~ Year * GDD, data = VAAN)
VAAN.N7 <- glm(N ~ Year * EVI, data = VAAN)
VAAN.N8 <- glm(N ~ Year * Site, data = VAAN)
VAAN.N9 <- glm(N ~ AverageGDD * EVI, data = VAAN)
VAAN.N10 <- glm(N ~ AverageGDD * Site, data = VAAN)
VAAN.N11 <- glm(N ~ EVI * Site, data = VAAN)
VAAN.N12 <- glm(N ~ Site, data = VAAN)
VAAN.N13 <- glm(N ~ GDD, data = VAAN)
VAAN.N14 <- glm(N ~ EVI, data = VAAN)  
VAAN.N15 <- glm(N ~ Year, data = VAAN)  
VAAN.N16 <- glm(N ~ 1, data =  VAAN)
Models.VAAN.N <- list("VAAN.N1" = VAAN.N1, "VAAN.N2" = VAAN.N2, "VAAN.N3" = VAAN.N3, "VAAN.N4" = VAAN.N4, "VAAN.N5" = VAAN.N5, "VAAN.N6" = VAAN.N6, "VAAN.N7" = VAAN.N7, "VAAN.N8" =  VAAN.N8, "VAAN.N9" = VAAN.N9, "VAAN.N10" = VAAN.N10, "VAAN.N11" = VAAN.N11, "VAAN.N12" = VAAN.N12, "VAAN.N13" = VAAN.N13, "VAAN.N14" = VAAN.N14, "VAAN.N15" = VAAN.N15, "VAAN.N16" = VAAN.N16)
VAAN.N <- aictab(cand.set = Models.VAAN.N)
print(VAAN.N)
write.csv(VAAN.N, "AIC/Tables/VAAN_N.csv")
summary(VAAN.N2)
PseudoR2(VAAN.N2, which = "Nagelkerke")
#C:N
VAAN.CN1 <- glm(CNRatio ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.CN2 <- glm(CNRatio ~ Year * EVI * GDD, data = VAAN)
VAAN.CN3 <- glm(CNRatio ~ Year * EVI * Site, data = VAAN)
VAAN.CN4 <- glm(CNRatio ~ Year * Site * GDD, data = VAAN)
VAAN.CN5 <- glm(CNRatio ~ EVI * GDD * Site, data = VAAN)
VAAN.CN6 <- glm(CNRatio ~ Year * GDD, data = VAAN)
VAAN.CN7 <- glm(CNRatio ~ Year * EVI, data = VAAN)
VAAN.CN8 <- glm(CNRatio ~ Year * Site, data = VAAN)
VAAN.CN9 <- glm(CNRatio ~ GDD * EVI, data = VAAN)
VAAN.CN10 <- glm(CNRatio ~ GDD * Site, data = VAAN)
VAAN.CN11 <- glm(CNRatio ~ EVI * Site, data = VAAN)
VAAN.CN12 <- glm(CNRatio ~ Site, data = VAAN)
VAAN.CN13 <- glm(CNRatio ~ GDD, data = VAAN)
VAAN.CN14 <- glm(CNRatio ~ EVI, data = VAAN)  
VAAN.CN15 <- glm(CNRatio ~ Year, data = VAAN)  
VAAN.CN16 <- glm(CNRatio ~ 1, data =  VAAN)
Models.VAAN.CN <- list("VAAN.CN1" = VAAN.CN1, "VAAN.CN2" = VAAN.CN2, "VAAN.CN3" = VAAN.CN3, "VAAN.CN4" = VAAN.CN4, "VAAN.CN5" = VAAN.CN5, "VAAN.CN6" = VAAN.CN6, "VAAN.CN7" = VAAN.CN7, "VAAN.CN8" =  VAAN.CN8, "VAAN.CN9" = VAAN.CN9, "VAAN.CN10" = VAAN.CN10, "VAAN.CN11" = VAAN.CN11, "VAAN.CN12" = VAAN.CN12, "VAAN.CN13" = VAAN.CN13, "VAAN.CN14" = VAAN.CN14, "VAAN.CN15" = VAAN.CN15, "VAAN.CN16" = VAAN.CN16)
VAAN.CN <- aictab(cand.set = Models.VAAN.CN)
print(VAAN.CN)
write.csv(VAAN.CN, "AIC/Tables/VAAN_CN.csv")
summary(VAAN.CN9)
PseudoR2(VAAN.CN9, which = "Nagelkerke")
#C:P
VAAN.CP1 <- glm(CPRatio ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.CP2 <- glm(CPRatio ~ Year * EVI * GDD, data = VAAN)
VAAN.CP3 <- glm(CPRatio ~ Year * EVI * Site, data = VAAN)
VAAN.CP4 <- glm(CPRatio ~ Year * Site * GDD, data = VAAN)
VAAN.CP5 <- glm(CPRatio ~ EVI * GDD * Site, data = VAAN)
VAAN.CP6 <- glm(CPRatio ~ Year * GDD, data = VAAN)
VAAN.CP7 <- glm(CPRatio ~ Year * EVI, data = VAAN)
VAAN.CP8 <- glm(CPRatio ~ Year * Site, data = VAAN)
VAAN.CP9 <- glm(CPRatio ~ GDD * EVI, data = VAAN)
VAAN.CP10 <- glm(CPRatio ~ GDD * Site, data = VAAN)
VAAN.CP11 <- glm(CPRatio ~ EVI * Site, data = VAAN)
VAAN.CP12 <- glm(CPRatio ~ Site, data = VAAN)
VAAN.CP13 <- glm(CPRatio ~ GDD, data = VAAN)
VAAN.CP14 <- glm(CPRatio ~ EVI, data = VAAN)  
VAAN.CP15 <- glm(CPRatio ~ Year, data = VAAN)  
VAAN.CP16 <- glm(CPRatio ~ 1, data =  VAAN)
Models.VAAN.CP <- list("VAAN.CP1" = VAAN.CP1, "VAAN.CP2" = VAAN.CP2, "VAAN.CP3" = VAAN.CP3, "VAAN.CP4" = VAAN.CP4, "VAAN.CP5" = VAAN.CP5, "VAAN.CP6" = VAAN.CP6, "VAAN.CP7" = VAAN.CP7, "VAAN.CP8" =  VAAN.CP8, "VAAN.CP9" = VAAN.CP9, "VAAN.CP10" = VAAN.CP10, "VAAN.CP11" = VAAN.CP11, "VAAN.CP12" = VAAN.CP12, "VAAN.CP13" = VAAN.CP13, "VAAN.CP14" = VAAN.CP14, "VAAN.CP15" = VAAN.CP15, "VAAN.CP16" = VAAN.CP16)
VAAN.CP <- aictab(cand.set = Models.VAAN.CP)
print(VAAN.CP)
write.csv(VAAN.CP, "AIC/Tables/VAAN_CP.csv")
summary(VAAN.CP4)
PseudoR2(VAAN.CP4, which = "Nagelkerke")
#N:P
VAAN.NP1 <- glm(NPRatio ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.NP2 <- glm(NPRatio ~ Year * EVI * GDD, data = VAAN)
VAAN.NP3 <- glm(NPRatio ~ Year * EVI * Site, data = VAAN)
VAAN.NP4 <- glm(NPRatio ~ Year * Site * GDD, data = VAAN)
VAAN.NP5 <- glm(NPRatio ~ EVI * GDD * Site, data = VAAN)
VAAN.NP6 <- glm(NPRatio ~ Year * GDD, data = VAAN)
VAAN.NP7 <- glm(NPRatio ~ Year * EVI, data = VAAN)
VAAN.NP8 <- glm(NPRatio ~ Year * Site, data = VAAN)
VAAN.NP9 <- glm(NPRatio ~ GDD * EVI, data = VAAN)
VAAN.NP10 <- glm(NPRatio ~ GDD * Site, data = VAAN)
VAAN.NP11 <- glm(NPRatio ~ EVI * Site, data = VAAN)
VAAN.NP12 <- glm(NPRatio ~ Site, data = VAAN)
VAAN.NP13 <- glm(NPRatio ~ GDD, data = VAAN)
VAAN.NP14 <- glm(NPRatio ~ EVI, data = VAAN)  
VAAN.NP15 <- glm(NPRatio ~ Year, data = VAAN)  
VAAN.NP16 <- glm(NPRatio ~ 1, data =  VAAN)
Models.VAAN.NP <- list("VAAN.NP1" = VAAN.NP1, "VAAN.NP2" = VAAN.NP2, "VAAN.NP3" = VAAN.NP3, "VAAN.NP4" = VAAN.NP4, "VAAN.NP5" = VAAN.NP5, "VAAN.NP6" = VAAN.NP6, "VAAN.NP7" = VAAN.NP7, "VAAN.NP8" =  VAAN.NP8, "VAAN.NP9" = VAAN.NP9, "VAAN.NP10" = VAAN.NP10, "VAAN.NP11" = VAAN.NP11, "VAAN.NP12" = VAAN.NP12, "VAAN.NP13" = VAAN.NP13, "VAAN.NP14" = VAAN.NP14, "VAAN.NP15" = VAAN.NP15, "VAAN.NP16" = VAAN.NP16)
VAAN.NP <- aictab(cand.set = Models.VAAN.NP)
print(VAAN.NP)
write.csv(VAAN.NP, "AIC/Tables/VAAN_NP.csv")
summary(VAAN.NP11)
PseudoR2(VAAN.NP11, which = "Nagelkerke")
#Qty_C
VAAN.Qty_C1 <- glm(Qty_C ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.Qty_C2 <- glm(Qty_C ~ Year * EVI * GDD, data = VAAN)
VAAN.Qty_C3 <- glm(Qty_C ~ Year * EVI * Site, data = VAAN)
VAAN.Qty_C4 <- glm(Qty_C ~ Year * Site * GDD, data = VAAN)
VAAN.Qty_C5 <- glm(Qty_C ~ EVI * GDD * Site, data = VAAN)
VAAN.Qty_C6 <- glm(Qty_C ~ Year * GDD, data = VAAN)
VAAN.Qty_C7 <- glm(Qty_C ~ Year * EVI, data = VAAN)
VAAN.Qty_C8 <- glm(Qty_C ~ Year * Site, data = VAAN)
VAAN.Qty_C9 <- glm(Qty_C ~ GDD * EVI, data = VAAN)
VAAN.Qty_C10 <- glm(Qty_C ~ GDD * Site, data = VAAN)
VAAN.Qty_C11 <- glm(Qty_C ~ EVI * Site, data = VAAN)
VAAN.Qty_C12 <- glm(Qty_C ~ Site, data = VAAN)
VAAN.Qty_C13 <- glm(Qty_C ~ GDD, data = VAAN)
VAAN.Qty_C14 <- glm(Qty_C ~ EVI, data = VAAN)  
VAAN.Qty_C15 <- glm(Qty_C ~ Year, data = VAAN)  
VAAN.Qty_C16 <- glm(Qty_C ~ 1, data =  VAAN)
Models.VAAN.Qty_C <- list("VAAN.Qty_C1" = VAAN.Qty_C1, "VAAN.Qty_C2" = VAAN.Qty_C2, "VAAN.Qty_C3" = VAAN.Qty_C3, "VAAN.Qty_C4" = VAAN.Qty_C4, "VAAN.Qty_C5" = VAAN.Qty_C5, "VAAN.Qty_C6" = VAAN.Qty_C6, "VAAN.Qty_C7" = VAAN.Qty_C7, "VAAN.Qty_C8" =  VAAN.Qty_C8, "VAAN.Qty_C9" = VAAN.Qty_C9, "VAAN.Qty_C10" = VAAN.Qty_C10, "VAAN.Qty_C11" = VAAN.Qty_C11, "VAAN.Qty_C12" = VAAN.Qty_C12, "VAAN.Qty_C13" = VAAN.Qty_C13, "VAAN.Qty_C14" = VAAN.Qty_C14, "VAAN.Qty_C15" = VAAN.Qty_C15, "VAAN.Qty_C16" = VAAN.Qty_C16)
VAAN.Qty_C <- aictab(cand.set = Models.VAAN.Qty_C)
print(VAAN.Qty_C)
write.csv(VAAN.Qty_C, "AIC/Tables/VAAN_Qty_C.csv")
summary(VAAN.Qty_C2)
PseudoR2(VAAN.Qty_C2, which = "Nagelkerke")
#Qty_P
VAAN.Qty_P1 <- glm(Qty_P ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.Qty_P2 <- glm(Qty_P ~ Year * EVI * GDD, data = VAAN)
VAAN.Qty_P3 <- glm(Qty_P ~ Year * EVI * Site, data = VAAN)
VAAN.Qty_P4 <- glm(Qty_P ~ Year * Site * GDD, data = VAAN)
VAAN.Qty_P5 <- glm(Qty_P ~ EVI * GDD * Site, data = VAAN)
VAAN.Qty_P6 <- glm(Qty_P ~ Year * GDD, data = VAAN)
VAAN.Qty_P7 <- glm(Qty_P ~ Year * EVI, data = VAAN)
VAAN.Qty_P8 <- glm(Qty_P ~ Year * Site, data = VAAN)
VAAN.Qty_P9 <- glm(Qty_P ~ GDD * EVI, data = VAAN)
VAAN.Qty_P10 <- glm(Qty_P ~ GDD * Site, data = VAAN)
VAAN.Qty_P11 <- glm(Qty_P ~ EVI * Site, data = VAAN)
VAAN.Qty_P12 <- glm(Qty_P ~ Site, data = VAAN)
VAAN.Qty_P13 <- glm(Qty_P ~ GDD, data = VAAN)
VAAN.Qty_P14 <- glm(Qty_P ~ EVI, data = VAAN)  
VAAN.Qty_P15 <- glm(Qty_P ~ Year, data = VAAN)  
VAAN.Qty_P16 <- glm(Qty_P ~ 1, data =  VAAN)
Models.VAAN.Qty_P <- list("VAAN.Qty_P1" = VAAN.Qty_P1, "VAAN.Qty_P2" = VAAN.Qty_P2, "VAAN.Qty_P3" = VAAN.Qty_P3, "VAAN.Qty_P4" = VAAN.Qty_P4, "VAAN.Qty_P5" = VAAN.Qty_P5, "VAAN.Qty_P6" = VAAN.Qty_P6, "VAAN.Qty_P7" = VAAN.Qty_P7, "VAAN.Qty_P8" =  VAAN.Qty_P8, "VAAN.Qty_P9" = VAAN.Qty_P9, "VAAN.Qty_P10" = VAAN.Qty_P10, "VAAN.Qty_P11" = VAAN.Qty_P11, "VAAN.Qty_P12" = VAAN.Qty_P12, "VAAN.Qty_P13" = VAAN.Qty_P13, "VAAN.Qty_P14" = VAAN.Qty_P14, "VAAN.Qty_P15" = VAAN.Qty_P15, "VAAN.Qty_P16" = VAAN.Qty_P16)
VAAN.Qty_P <- aictab(cand.set = Models.VAAN.Qty_P)
print(VAAN.Qty_P)
write.csv(VAAN.Qty_P, "AIC/Tables/VAAN_Qty_P.csv")
summary(VAAN.Qty_P1)
PseudoR2(VAAN.Qty_P1, which = "Nagelkerke")
#Qty_N
VAAN.Qty_N1 <- glm(Qty_N ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.Qty_N2 <- glm(Qty_N ~ Year * EVI * GDD, data = VAAN)
VAAN.Qty_N3 <- glm(Qty_N ~ Year * EVI * Site, data = VAAN)
VAAN.Qty_N4 <- glm(Qty_N ~ Year * Site * GDD, data = VAAN)
VAAN.Qty_N5 <- glm(Qty_N ~ EVI * GDD * Site, data = VAAN)
VAAN.Qty_N6 <- glm(Qty_N ~ Year * GDD, data = VAAN)
VAAN.Qty_N7 <- glm(Qty_N ~ Year * EVI, data = VAAN)
VAAN.Qty_N8 <- glm(Qty_N ~ Year * Site, data = VAAN)
VAAN.Qty_N9 <- glm(Qty_N ~ GDD * EVI, data = VAAN)
VAAN.Qty_N10 <- glm(Qty_N ~ GDD * Site, data = VAAN)
VAAN.Qty_N11 <- glm(Qty_N ~ EVI * Site, data = VAAN)
VAAN.Qty_N12 <- glm(Qty_N ~ Site, data = VAAN)
VAAN.Qty_N13 <- glm(Qty_N ~ GDD, data = VAAN)
VAAN.Qty_N14 <- glm(Qty_N ~ EVI, data = VAAN)  
VAAN.Qty_N15 <- glm(Qty_N ~ Year, data = VAAN)  
VAAN.Qty_N16 <- glm(Qty_N ~ 1, data =  VAAN)
Models.VAAN.Qty_N <- list("VAAN.Qty_N1" = VAAN.Qty_N1, "VAAN.Qty_N2" = VAAN.Qty_N2, "VAAN.Qty_N3" = VAAN.Qty_N3, "VAAN.Qty_N4" = VAAN.Qty_N4, "VAAN.Qty_N5" = VAAN.Qty_N5, "VAAN.Qty_N6" = VAAN.Qty_N6, "VAAN.Qty_N7" = VAAN.Qty_N7, "VAAN.Qty_N8" =  VAAN.Qty_N8, "VAAN.Qty_N9" = VAAN.Qty_N9, "VAAN.Qty_N10" = VAAN.Qty_N10, "VAAN.Qty_N11" = VAAN.Qty_N11, "VAAN.Qty_N12" = VAAN.Qty_N12, "VAAN.Qty_N13" = VAAN.Qty_N13, "VAAN.Qty_N14" = VAAN.Qty_N14, "VAAN.Qty_N15" = VAAN.Qty_N15, "VAAN.Qty_N16" = VAAN.Qty_N16)
VAAN.Qty_N <- aictab(cand.set = Models.VAAN.Qty_N)
print(VAAN.Qty_N)
write.csv(VAAN.Qty_N, "AIC/Tables/VAAN_Qty_N.csv")
summary(VAAN.Qty_N2)
PseudoR2(VAAN.Qty_N2, which = "Nagelkerke")

