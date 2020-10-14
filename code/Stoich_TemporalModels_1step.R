# Author: Isabella Richmond
# Last edited: March 23, 2020

# NOTE: This code is no longer being used - moving to a 2-step model approach, see 
#       Stoich_TemporalModels_2step.R in code folder to see current modelling approach
#       Moving to new approach to simplify models and make them more interpretable

# This code was for the creation and evaluation of my temporal stoichiometry models. A lot of 
# Code was provided by Travis Heckford (twitter.com/travheckford)
# Models evaluate the response of stoichiometry in four boreal plant species with site, year
# weather, moisture, and productivity as explanatory variables
# Stoichiometry measured using C:N, C:P, and N:P 
# Elemental composition is also used as a response variable - % C, % N, % P and quantities

#### Data Preparation ####
# load packages
install.packages("easypackages")
library(easypackages)
install_packages("MuMIn", "purrr", "ggcorrplot", "purrr", "broom", "patchwork")
libraries("purrr", "patchwork", "broom", "ggcorrplot", "ggplot2","dplyr", "tibble", "readr", "plyr", "ggpol", "ggpubr", "MuMIn", "AICcmodavg", "texreg", "kimisc", "psych", "DescTools")

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

### Create Functions ### 
# create a function to make the residual plots later on in the script 
# code originally obtained and edited from github.com/aosmith and https://rstudio-pubs-static.s3.amazonaws.com/43190_f9bd428cf5b94562a1b46e00b454e05a.html
resid_plots <- function(model, modelname){
  # augment is from broom package, it adds columns of residuals, predictions, etc.
  output <- augment(model)
  # first plot is a residual vs fitted plot
  p1 <- ggplot(model, aes(.fitted, .resid))+geom_point()
  p1 <- p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1 <- p1+xlab("Fitted values")+ylab("Residuals")
  p1 <- p1+ggtitle("Residual vs Fitted Plot")+theme_bw()+theme(plot.title = element_text(size=9),axis.title = element_text(size=8))
  # second plot is a qqplot to test normality
  p2 <- ggplot(model, aes(sample=.stdresid))+stat_qq()
  p2 <- p2+geom_qq_line(col='red')+xlab("Theoretical Quantiles")+ylab("Stdized Residuals")
  p2 <- p2+ggtitle("Normal Q-Q")+theme_bw()+theme(plot.title = element_text(size=9),axis.title = element_text(size=8))
  # third plot is a Cook's distance plot to assess outliers 
  p3 <- ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
  p3 <- p3+xlab("Obs. Number")+ylab("Cook's distance")
  p3 <- p3+ggtitle("Cook's distance")+theme_bw()+theme(plot.title = element_text(size=9),axis.title = element_text(size=8))
  # last plot is a check of influential data points, similar to Cook's distance
  p4 <- ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
  p4 <- p4+stat_smooth(method="loess", na.rm=TRUE)
  p4 <- p4+xlab("Leverage")+ylab("Stdized Residuals")
  p4 <- p4+ggtitle("Residual vs Leverage Plot")
  p4 <- p4+scale_size_continuous("Cook's Distance", range=c(1,5))
  p4 <- p4+theme_bw()+theme(plot.title = element_text(size=9),axis.title = element_text(size=8), legend.position="bottom")
  # plot with patchwork 
  (p1 | p2) /
    (p3 | p4) +
    patchwork::plot_annotation(title = paste("Diagnostic plots", modelname))
}


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
ABBA.C1 <- glm(C ~ Year * EVI * GDD * NDMI * Site, data = ABBA)
ABBA.C2 <- glm(C ~ Year * EVI * GDD * NDMI, data = ABBA)
ABBA.C3 <- glm(C ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.C4 <- glm(C ~ Year * EVI * Site * NDMI, data = ABBA)
ABBA.C5 <- glm(C ~ Year * Site * GDD * NDMI, data = ABBA) 
ABBA.C6 <- glm(C ~ Site * EVI * GDD * NDMI, data = ABBA) 
ABBA.C7 <- glm(C ~ Year * EVI * GDD, data = ABBA) 
ABBA.C8 <- glm(C ~ Year * EVI * NDMI, data = ABBA)
ABBA.C9 <- glm(C ~ Year * EVI * Site, data = ABBA)
ABBA.C10 <- glm(C ~ EVI * GDD * NDMI, data = ABBA)
ABBA.C11 <- glm(C ~ EVI * GDD * Site, data = ABBA)
ABBA.C12 <- glm(C ~ Year * GDD * NDMI, data = ABBA)
ABBA.C13 <- glm(C ~ Year * Site * GDD, data = ABBA)
ABBA.C14 <- glm(C ~ Year * NDMI * Site, data = ABBA)
ABBA.C15 <- glm(C ~ GDD * NDMI * Site, data = ABBA)
ABBA.C16 <- glm(C ~ Year * Site, data = ABBA)
ABBA.C17 <- glm(C ~ Year * EVI, data = ABBA)
ABBA.C18 <- glm(C ~ Year * NDMI, data = ABBA)
ABBA.C19 <- glm(C ~ Year * GDD, data = ABBA)
ABBA.C20 <- glm(C ~ EVI * Site, data = ABBA)
ABBA.C21 <- glm(C ~ NDMI * Site, data = ABBA)
ABBA.C22 <- glm(C ~ GDD * Site, data = ABBA)
ABBA.C23 <- glm(C ~ EVI * NDMI, data = ABBA)
ABBA.C24 <- glm(C ~ NDMI * GDD, data = ABBA)
ABBA.C25 <- glm(C ~ GDD * EVI, data = ABBA)
ABBA.C26 <- glm(C ~ Site, data = ABBA)
ABBA.C27 <- glm(C ~ NDMI, data = ABBA)
ABBA.C28 <- glm(C ~ GDD, data = ABBA)
ABBA.C29 <- glm(C ~ EVI, data = ABBA) 
ABBA.C30 <- glm(C ~ Year, data = ABBA) 
ABBA.C31 <- glm(C ~ 1, data =  ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Cmodels <- list(ABBA.C1, ABBA.C2, ABBA.C3, ABBA.C4, ABBA.C5, ABBA.C6, ABBA.C7, ABBA.C8, ABBA.C9, ABBA.C10, ABBA.C11, ABBA.C12, ABBA.C13, ABBA.C14, ABBA.C15, ABBA.C16, ABBA.C17, ABBA.C18, ABBA.C19, ABBA.C20, ABBA.C21, ABBA.C22, ABBA.C23, ABBA.C24, ABBA.C25, ABBA.C26, ABBA.C27, ABBA.C28, ABBA.C29, ABBA.C30, ABBA.C31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.C.residplots <- imap(ABBA.Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ABBA_C_glm.pdf")
ABBA.C.residplots
dev.off()
# if models pass assumptions, proceed. If not, use different error structure 

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.C <- list("ABBA.C1" = ABBA.C1, "ABBA.C2" = ABBA.C2, "ABBA.C3" = ABBA.C3, "ABBA.C4" = ABBA.C4, "ABBA.C5" = ABBA.C5, "ABBA.C6" = ABBA.C6, "ABBA.C7" = ABBA.C7, "ABBA.C8" =  ABBA.C8, "ABBA.C9" = ABBA.C9, "ABBA.C10" = ABBA.C10, "ABBA.C11" = ABBA.C11, "ABBA.C12" = ABBA.C12, "ABBA.C13" = ABBA.C13, "ABBA.C14" = ABBA.C14, "ABBA.C15" = ABBA.C15, "ABBA.C16" = ABBA.C16, "ABBA.C17" = ABBA.C17, "ABBA.C18" = ABBA.C18, "ABBA.C19" = ABBA.C19, "ABBA.C20" = ABBA.C20, "ABBA.C21" = ABBA.C21, "ABBA.C22" = ABBA.C22, "ABBA.C23" = ABBA.C23, "ABBA.C24" =  ABBA.C24, "ABBA.C25" = ABBA.C25, "ABBA.C26" = ABBA.C26, "ABBA.C27" = ABBA.C27, "ABBA.C28" = ABBA.C28, "ABBA.C29" = ABBA.C29, "ABBA.C30" = ABBA.C30, "ABBA.C31" = ABBA.C31)
ABBA.C <- aictab(cand.set = Models.ABBA.C)
print(ABBA.C)
write.csv(ABBA.C, "output/AIC/ABBA_C.csv")
# summary of the most parsimonious models - delta AICc of less than 2
summary(ABBA.C1)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.C1, which = "Nagelkerke")
#%P
ABBA.P1 <- glm(P ~ Year * EVI * GDD * NDMI * Site, data = ABBA)
ABBA.P2 <- glm(P ~ Year * EVI * GDD * NDMI, data = ABBA)
ABBA.P3 <- glm(P ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.P4 <- glm(P ~ Year * EVI * Site * NDMI, data = ABBA)
ABBA.P5 <- glm(P ~ Year * Site * GDD * NDMI, data = ABBA) 
ABBA.P6 <- glm(P ~ Site * EVI * GDD * NDMI, data = ABBA) 
ABBA.P7 <- glm(P ~ Year * EVI * GDD, data = ABBA) 
ABBA.P8 <- glm(P ~ Year * EVI * NDMI, data = ABBA)
ABBA.P9 <- glm(P ~ Year * EVI * Site, data = ABBA)
ABBA.P10 <- glm(P ~ EVI * GDD * NDMI, data = ABBA)
ABBA.P11 <- glm(P ~ EVI * GDD * Site, data = ABBA)
ABBA.P12 <- glm(P ~ Year * GDD * NDMI, data = ABBA)
ABBA.P13 <- glm(P ~ Year * Site * GDD, data = ABBA)
ABBA.P14 <- glm(P ~ Year * NDMI * Site, data = ABBA)
ABBA.P15 <- glm(P ~ GDD * NDMI * Site, data = ABBA)
ABBA.P16 <- glm(P ~ Year * Site, data = ABBA)
ABBA.P17 <- glm(P ~ Year * EVI, data = ABBA)
ABBA.P18 <- glm(P ~ Year * NDMI, data = ABBA)
ABBA.P19 <- glm(P ~ Year * GDD, data = ABBA)
ABBA.P20 <- glm(P ~ EVI * Site, data = ABBA)
ABBA.P21 <- glm(P ~ NDMI * Site, data = ABBA)
ABBA.P22 <- glm(P ~ GDD * Site, data = ABBA)
ABBA.P23 <- glm(P ~ EVI * NDMI, data = ABBA)
ABBA.P24 <- glm(P ~ NDMI * GDD, data = ABBA)
ABBA.P25 <- glm(P ~ GDD * EVI, data = ABBA)
ABBA.P26 <- glm(P ~ Site, data = ABBA)
ABBA.P27 <- glm(P ~ NDMI, data = ABBA)
ABBA.P28 <- glm(P ~ GDD, data = ABBA)
ABBA.P29 <- glm(P ~ EVI, data = ABBA) 
ABBA.P30 <- glm(P ~ Year, data = ABBA) 
ABBA.P31 <- glm(P ~ 1, data =  ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Pmodels <- list(ABBA.P1, ABBA.P2, ABBA.P3, ABBA.P4, ABBA.P5, ABBA.P6, ABBA.P7, ABBA.P8, ABBA.P9, ABBA.P10, ABBA.P11, ABBA.P12, ABBA.P13, ABBA.P14, ABBA.P15, ABBA.P16, ABBA.P17, ABBA.P18, ABBA.P19, ABBA.P20, ABBA.P21, ABBA.P22, ABBA.P23, ABBA.P24, ABBA.P25, ABBA.P26, ABBA.P27, ABBA.P28, ABBA.P29, ABBA.P30, ABBA.P31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.P.residplots <- imap(ABBA.Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ABBA_P_glm.pdf")
ABBA.P.residplots
dev.off()
# if models pass assumptions, proceed. If not, use different error structure 

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.P <- list("ABBA.P1" = ABBA.P1, "ABBA.P2" = ABBA.P2, "ABBA.P3" = ABBA.P3, "ABBA.P4" = ABBA.P4, "ABBA.P5" = ABBA.P5, "ABBA.P6" = ABBA.P6, "ABBA.P7" = ABBA.P7, "ABBA.P8" =  ABBA.P8, "ABBA.P9" = ABBA.P9, "ABBA.P10" = ABBA.P10, "ABBA.P11" = ABBA.P11, "ABBA.P12" = ABBA.P12, "ABBA.P13" = ABBA.P13, "ABBA.P14" = ABBA.P14, "ABBA.P15" = ABBA.P15, "ABBA.P16" = ABBA.P16, "ABBA.P17" = ABBA.P17, "ABBA.P18" = ABBA.P18, "ABBA.P19" = ABBA.P19, "ABBA.P20" = ABBA.P20, "ABBA.P21" = ABBA.P21, "ABBA.P22" = ABBA.P22, "ABBA.P23" = ABBA.P23, "ABBA.P24" =  ABBA.P24, "ABBA.P25" = ABBA.P25, "ABBA.P26" = ABBA.P26, "ABBA.P27" = ABBA.P27, "ABBA.P28" = ABBA.P28, "ABBA.P29" = ABBA.P29, "ABBA.P30" = ABBA.P30, "ABBA.P31" = ABBA.P31)
ABBA.P <- aictab(cand.set = Models.ABBA.P)
print(ABBA.P)
write.csv(ABBA.P, "output/AIC/ABBA_P.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ABBA.P10)
summary(ABBA.P16)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.P10, which = "Nagelkerke")
PseudoR2(ABBA.P16, which = "Nagelkerke")
#%N
ABBA.N1 <- glm(N ~ Year * EVI * GDD * NDMI * Site, data = ABBA)
ABBA.N2 <- glm(N ~ Year * EVI * GDD * NDMI, data = ABBA)
ABBA.N3 <- glm(N ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.N4 <- glm(N ~ Year * EVI * Site * NDMI, data = ABBA)
ABBA.N5 <- glm(N ~ Year * Site * GDD * NDMI, data = ABBA) 
ABBA.N6 <- glm(N ~ Site * EVI * GDD * NDMI, data = ABBA) 
ABBA.N7 <- glm(N ~ Year * EVI * GDD, data = ABBA) 
ABBA.N8 <- glm(N ~ Year * EVI * NDMI, data = ABBA)
ABBA.N9 <- glm(N ~ Year * EVI * Site, data = ABBA)
ABBA.N10 <- glm(N ~ EVI * GDD * NDMI, data = ABBA)
ABBA.N11 <- glm(N ~ EVI * GDD * Site, data = ABBA)
ABBA.N12 <- glm(N ~ Year * GDD * NDMI, data = ABBA)
ABBA.N13 <- glm(N ~ Year * Site * GDD, data = ABBA)
ABBA.N14 <- glm(N ~ Year * NDMI * Site, data = ABBA)
ABBA.N15 <- glm(N ~ GDD * NDMI * Site, data = ABBA)
ABBA.N16 <- glm(N ~ Year * Site, data = ABBA)
ABBA.N17 <- glm(N ~ Year * EVI, data = ABBA)
ABBA.N18 <- glm(N ~ Year * NDMI, data = ABBA)
ABBA.N19 <- glm(N ~ Year * GDD, data = ABBA)
ABBA.N20 <- glm(N ~ EVI * Site, data = ABBA)
ABBA.N21 <- glm(N ~ NDMI * Site, data = ABBA)
ABBA.N22 <- glm(N ~ GDD * Site, data = ABBA)
ABBA.N23 <- glm(N ~ EVI * NDMI, data = ABBA)
ABBA.N24 <- glm(N ~ NDMI * GDD, data = ABBA)
ABBA.N25 <- glm(N ~ GDD * EVI, data = ABBA)
ABBA.N26 <- glm(N ~ Site, data = ABBA)
ABBA.N27 <- glm(N ~ NDMI, data = ABBA)
ABBA.N28 <- glm(N ~ GDD, data = ABBA)
ABBA.N29 <- glm(N ~ EVI, data = ABBA) 
ABBA.N30 <- glm(N ~ Year, data = ABBA) 
ABBA.N31 <- glm(N ~ 1, data =  ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Nmodels <- list(ABBA.N1, ABBA.N2, ABBA.N3, ABBA.N4, ABBA.N5, ABBA.N6, ABBA.N7, ABBA.N8, ABBA.N9, ABBA.N10, ABBA.N11, ABBA.N12, ABBA.N13, ABBA.N14, ABBA.N15, ABBA.N16, ABBA.N17, ABBA.N18, ABBA.N19, ABBA.N20, ABBA.N21, ABBA.N22, ABBA.N23, ABBA.N24, ABBA.N25, ABBA.N26, ABBA.N27, ABBA.N28, ABBA.N29, ABBA.N30, ABBA.N31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.N.residplots <- imap(ABBA.Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ABBA_N_glm.pdf")
ABBA.N.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.N <- list("ABBA.N1" = ABBA.N1, "ABBA.N2" = ABBA.N2, "ABBA.N3" = ABBA.N3, "ABBA.N4" = ABBA.N4, "ABBA.N5" = ABBA.N5, "ABBA.N6" = ABBA.N6, "ABBA.N7" = ABBA.N7, "ABBA.N8" =  ABBA.N8, "ABBA.N9" = ABBA.N9, "ABBA.N10" = ABBA.N10, "ABBA.N11" = ABBA.N11, "ABBA.N12" = ABBA.N12, "ABBA.N13" = ABBA.N13, "ABBA.N14" = ABBA.N14, "ABBA.N15" = ABBA.N15, "ABBA.N16" = ABBA.N16, "ABBA.N17" = ABBA.N17, "ABBA.N18" = ABBA.N18, "ABBA.N19" = ABBA.N19, "ABBA.N20" = ABBA.N20, "ABBA.N21" = ABBA.N21, "ABBA.N22" = ABBA.N22, "ABBA.N23" = ABBA.N23, "ABBA.N24" =  ABBA.N24, "ABBA.N25" = ABBA.N25, "ABBA.N26" = ABBA.N26, "ABBA.N27" = ABBA.N27, "ABBA.N28" = ABBA.N28, "ABBA.N29" = ABBA.N29, "ABBA.N30" = ABBA.N30, "ABBA.N31" = ABBA.N31)
ABBA.N <- aictab(cand.set = Models.ABBA.N)
print(ABBA.N)
write.csv(ABBA.N, "output/AIC/ABBA_N.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ABBA.N22)
summary(ABBA.N11)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.N22, which = "Nagelkerke")
PseudoR2(ABBA.N11, which = "Nagelkerke")
#C:N
ABBA.CN1 <- glm(CNRatio ~ Year * EVI * GDD * NDMI * Site, data = ABBA)
ABBA.CN2 <- glm(CNRatio ~ Year * EVI * GDD * NDMI, data = ABBA)
ABBA.CN3 <- glm(CNRatio ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.CN4 <- glm(CNRatio ~ Year * EVI * Site * NDMI, data = ABBA)
ABBA.CN5 <- glm(CNRatio ~ Year * Site * GDD * NDMI, data = ABBA) 
ABBA.CN6 <- glm(CNRatio ~ Site * EVI * GDD * NDMI, data = ABBA) 
ABBA.CN7 <- glm(CNRatio ~ Year * EVI * GDD, data = ABBA) 
ABBA.CN8 <- glm(CNRatio ~ Year * EVI * NDMI, data = ABBA)
ABBA.CN9 <- glm(CNRatio ~ Year * EVI * Site, data = ABBA)
ABBA.CN10 <- glm(CNRatio ~ EVI * GDD * NDMI, data = ABBA)
ABBA.CN11 <- glm(CNRatio ~ EVI * GDD * Site, data = ABBA)
ABBA.CN12 <- glm(CNRatio ~ Year * GDD * NDMI, data = ABBA)
ABBA.CN13 <- glm(CNRatio ~ Year * Site * GDD, data = ABBA)
ABBA.CN14 <- glm(CNRatio ~ Year * NDMI * Site, data = ABBA)
ABBA.CN15 <- glm(CNRatio ~ GDD * NDMI * Site, data = ABBA)
ABBA.CN16 <- glm(CNRatio ~ Year * Site, data = ABBA)
ABBA.CN17 <- glm(CNRatio ~ Year * EVI, data = ABBA)
ABBA.CN18 <- glm(CNRatio ~ Year * NDMI, data = ABBA)
ABBA.CN19 <- glm(CNRatio ~ Year * GDD, data = ABBA)
ABBA.CN20 <- glm(CNRatio ~ EVI * Site, data = ABBA)
ABBA.CN21 <- glm(CNRatio ~ NDMI * Site, data = ABBA)
ABBA.CN22 <- glm(CNRatio ~ GDD * Site, data = ABBA)
ABBA.CN23 <- glm(CNRatio ~ EVI * NDMI, data = ABBA)
ABBA.CN24 <- glm(CNRatio ~ NDMI * GDD, data = ABBA)
ABBA.CN25 <- glm(CNRatio ~ GDD * EVI, data = ABBA)
ABBA.CN26 <- glm(CNRatio ~ Site, data = ABBA)
ABBA.CN27 <- glm(CNRatio ~ NDMI, data = ABBA)
ABBA.CN28 <- glm(CNRatio ~ GDD, data = ABBA)
ABBA.CN29 <- glm(CNRatio ~ EVI, data = ABBA) 
ABBA.CN30 <- glm(CNRatio ~ Year, data = ABBA) 
ABBA.CN31 <- glm(CNRatio ~ 1, data =  ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.CNmodels <- list(ABBA.CN1, ABBA.CN2, ABBA.CN3, ABBA.CN4, ABBA.CN5, ABBA.CN6, ABBA.CN7, ABBA.CN8, ABBA.CN9, ABBA.CN10, ABBA.CN11, ABBA.CN12, ABBA.CN13, ABBA.CN14, ABBA.CN15, ABBA.CN16, ABBA.CN17, ABBA.CN18, ABBA.CN19, ABBA.CN20, ABBA.CN21, ABBA.CN22, ABBA.CN23, ABBA.CN24, ABBA.CN25, ABBA.CN26, ABBA.CN27, ABBA.CN28, ABBA.CN29, ABBA.CN30, ABBA.CN31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.CN.residplots <- imap(ABBA.CNmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ABBA_CN_glm.pdf")
ABBA.CN.residplots
dev.off()
# if models pass assumptions, proceed. If not, use different error structure 

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.CN <- list("ABBA.CN1" = ABBA.CN1, "ABBA.CN2" = ABBA.CN2, "ABBA.CN3" = ABBA.CN3, "ABBA.CN4" = ABBA.CN4, "ABBA.CN5" = ABBA.CN5, "ABBA.CN6" = ABBA.CN6, "ABBA.CN7" = ABBA.CN7, "ABBA.CN8" =  ABBA.CN8, "ABBA.CN9" = ABBA.CN9, "ABBA.CN10" = ABBA.CN10, "ABBA.CN11" = ABBA.CN11, "ABBA.CN12" = ABBA.CN12, "ABBA.CN13" = ABBA.CN13, "ABBA.CN14" = ABBA.CN14, "ABBA.CN15" = ABBA.CN15, "ABBA.CN16" = ABBA.CN16, "ABBA.CN17" = ABBA.CN17, "ABBA.CN18" = ABBA.CN18, "ABBA.CN19" = ABBA.CN19, "ABBA.CN20" = ABBA.CN20, "ABBA.CN21" = ABBA.CN21, "ABBA.CN22" = ABBA.CN22, "ABBA.CN23" = ABBA.CN23, "ABBA.CN24" =  ABBA.CN24, "ABBA.CN25" = ABBA.CN25, "ABBA.CN26" = ABBA.CN26, "ABBA.CN27" = ABBA.CN27, "ABBA.CN28" = ABBA.CN28, "ABBA.CN29" = ABBA.CN29, "ABBA.CN30" = ABBA.CN30, "ABBA.CN31" = ABBA.CN31)
ABBA.CN <- aictab(cand.set = Models.ABBA.CN)
print(ABBA.CN)
write.csv(ABBA.CN, "output/AIC/ABBA_CN.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ABBA.CN11)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.CN11, which = "Nagelkerke")
#C:P
ABBA.CP1 <- glm(CPRatio ~ Year * EVI * GDD * NDMI * Site, data = ABBA)
ABBA.CP2 <- glm(CPRatio ~ Year * EVI * GDD * NDMI, data = ABBA)
ABBA.CP3 <- glm(CPRatio ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.CP4 <- glm(CPRatio ~ Year * EVI * Site * NDMI, data = ABBA)
ABBA.CP5 <- glm(CPRatio ~ Year * Site * GDD * NDMI, data = ABBA) 
ABBA.CP6 <- glm(CPRatio ~ Site * EVI * GDD * NDMI, data = ABBA) 
ABBA.CP7 <- glm(CPRatio ~ Year * EVI * GDD, data = ABBA) 
ABBA.CP8 <- glm(CPRatio ~ Year * EVI * NDMI, data = ABBA)
ABBA.CP9 <- glm(CPRatio ~ Year * EVI * Site, data = ABBA)
ABBA.CP10 <- glm(CPRatio ~ EVI * GDD * NDMI, data = ABBA)
ABBA.CP11 <- glm(CPRatio ~ EVI * GDD * Site, data = ABBA)
ABBA.CP12 <- glm(CPRatio ~ Year * GDD * NDMI, data = ABBA)
ABBA.CP13 <- glm(CPRatio ~ Year * Site * GDD, data = ABBA)
ABBA.CP14 <- glm(CPRatio ~ Year * NDMI * Site, data = ABBA)
ABBA.CP15 <- glm(CPRatio ~ GDD * NDMI * Site, data = ABBA)
ABBA.CP16 <- glm(CPRatio ~ Year * Site, data = ABBA)
ABBA.CP17 <- glm(CPRatio ~ Year * EVI, data = ABBA)
ABBA.CP18 <- glm(CPRatio ~ Year * NDMI, data = ABBA)
ABBA.CP19 <- glm(CPRatio ~ Year * GDD, data = ABBA)
ABBA.CP20 <- glm(CPRatio ~ EVI * Site, data = ABBA)
ABBA.CP21 <- glm(CPRatio ~ NDMI * Site, data = ABBA)
ABBA.CP22 <- glm(CPRatio ~ GDD * Site, data = ABBA)
ABBA.CP23 <- glm(CPRatio ~ EVI * NDMI, data = ABBA)
ABBA.CP24 <- glm(CPRatio ~ NDMI * GDD, data = ABBA)
ABBA.CP25 <- glm(CPRatio ~ GDD * EVI, data = ABBA)
ABBA.CP26 <- glm(CPRatio ~ Site, data = ABBA)
ABBA.CP27 <- glm(CPRatio ~ NDMI, data = ABBA)
ABBA.CP28 <- glm(CPRatio ~ GDD, data = ABBA)
ABBA.CP29 <- glm(CPRatio ~ EVI, data = ABBA) 
ABBA.CP30 <- glm(CPRatio ~ Year, data = ABBA) 
ABBA.CP31 <- glm(CPRatio ~ 1, data =  ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.CPmodels <- list(ABBA.CP1, ABBA.CP2, ABBA.CP3, ABBA.CP4, ABBA.CP5, ABBA.CP6, ABBA.CP7, ABBA.CP8, ABBA.CP9, ABBA.CP10, ABBA.CP11, ABBA.CP12, ABBA.CP13, ABBA.CP14, ABBA.CP15, ABBA.CP16, ABBA.CP17, ABBA.CP18, ABBA.CP19, ABBA.CP20, ABBA.CP21, ABBA.CP22, ABBA.CP23, ABBA.CP24, ABBA.CP25, ABBA.CP26, ABBA.CP27, ABBA.CP28, ABBA.CP29, ABBA.CP30, ABBA.CP31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.CP.residplots <- imap(ABBA.CPmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ABBA_CP_glm.pdf")
ABBA.CP.residplots
dev.off()
# if models pass assumptions, proceed. If not, use different error structure
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.CP <- list("ABBA.CP1" = ABBA.CP1, "ABBA.CP2" = ABBA.CP2, "ABBA.CP3" = ABBA.CP3, "ABBA.CP4" = ABBA.CP4, "ABBA.CP5" = ABBA.CP5, "ABBA.CP6" = ABBA.CP6, "ABBA.CP7" = ABBA.CP7, "ABBA.CP8" =  ABBA.CP8, "ABBA.CP9" = ABBA.CP9, "ABBA.CP10" = ABBA.CP10, "ABBA.CP11" = ABBA.CP11, "ABBA.CP12" = ABBA.CP12, "ABBA.CP13" = ABBA.CP13, "ABBA.CP14" = ABBA.CP14, "ABBA.CP15" = ABBA.CP15, "ABBA.CP16" = ABBA.CP16, "ABBA.CP17" = ABBA.CP17, "ABBA.CP18" = ABBA.CP18, "ABBA.CP19" = ABBA.CP19, "ABBA.CP20" = ABBA.CP20, "ABBA.CP21" = ABBA.CP21, "ABBA.CP22" = ABBA.CP22, "ABBA.CP23" = ABBA.CP23, "ABBA.CP24" =  ABBA.CP24, "ABBA.CP25" = ABBA.CP25, "ABBA.CP26" = ABBA.CP26, "ABBA.CP27" = ABBA.CP27, "ABBA.CP28" = ABBA.CP28, "ABBA.CP29" = ABBA.CP29, "ABBA.CP30" = ABBA.CP30, "ABBA.CP31" = ABBA.CP31)
ABBA.CP <- aictab(cand.set = Models.ABBA.CP)
print(ABBA.CP)
write.csv(ABBA.CP, "output/AIC/ABBA_CP.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ABBA.CP16)
summary(ABBA.CP10)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.CP16, which = "Nagelkerke")
PseudoR2(ABBA.CP10, which = "Nagelkerke")
#N:P
ABBA.NP1 <- glm(NPRatio ~ Year * EVI * GDD * NDMI * Site, data = ABBA)
ABBA.NP2 <- glm(NPRatio ~ Year * EVI * GDD * NDMI, data = ABBA)
ABBA.NP3 <- glm(NPRatio ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.NP4 <- glm(NPRatio ~ Year * EVI * Site * NDMI, data = ABBA)
ABBA.NP5 <- glm(NPRatio ~ Year * Site * GDD * NDMI, data = ABBA) 
ABBA.NP6 <- glm(NPRatio ~ Site * EVI * GDD * NDMI, data = ABBA) 
ABBA.NP7 <- glm(NPRatio ~ Year * EVI * GDD, data = ABBA) 
ABBA.NP8 <- glm(NPRatio ~ Year * EVI * NDMI, data = ABBA)
ABBA.NP9 <- glm(NPRatio ~ Year * EVI * Site, data = ABBA)
ABBA.NP10 <- glm(NPRatio ~ EVI * GDD * NDMI, data = ABBA)
ABBA.NP11 <- glm(NPRatio ~ EVI * GDD * Site, data = ABBA)
ABBA.NP12 <- glm(NPRatio ~ Year * GDD * NDMI, data = ABBA)
ABBA.NP13 <- glm(NPRatio ~ Year * Site * GDD, data = ABBA)
ABBA.NP14 <- glm(NPRatio ~ Year * NDMI * Site, data = ABBA)
ABBA.NP15 <- glm(NPRatio ~ GDD * NDMI * Site, data = ABBA)
ABBA.NP16 <- glm(NPRatio ~ Year * Site, data = ABBA)
ABBA.NP17 <- glm(NPRatio ~ Year * EVI, data = ABBA)
ABBA.NP18 <- glm(NPRatio ~ Year * NDMI, data = ABBA)
ABBA.NP19 <- glm(NPRatio ~ Year * GDD, data = ABBA)
ABBA.NP20 <- glm(NPRatio ~ EVI * Site, data = ABBA)
ABBA.NP21 <- glm(NPRatio ~ NDMI * Site, data = ABBA)
ABBA.NP22 <- glm(NPRatio ~ GDD * Site, data = ABBA)
ABBA.NP23 <- glm(NPRatio ~ EVI * NDMI, data = ABBA)
ABBA.NP24 <- glm(NPRatio ~ NDMI * GDD, data = ABBA)
ABBA.NP25 <- glm(NPRatio ~ GDD * EVI, data = ABBA)
ABBA.NP26 <- glm(NPRatio ~ Site, data = ABBA)
ABBA.NP27 <- glm(NPRatio ~ NDMI, data = ABBA)
ABBA.NP28 <- glm(NPRatio ~ GDD, data = ABBA)
ABBA.NP29 <- glm(NPRatio ~ EVI, data = ABBA) 
ABBA.NP30 <- glm(NPRatio ~ Year, data = ABBA) 
ABBA.NP31 <- glm(NPRatio ~ 1, data =  ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.NPmodels <- list(ABBA.NP1, ABBA.NP2, ABBA.NP3, ABBA.NP4, ABBA.NP5, ABBA.NP6, ABBA.NP7, ABBA.NP8, ABBA.NP9, ABBA.NP10, ABBA.NP11, ABBA.NP12, ABBA.NP13, ABBA.NP14, ABBA.NP15, ABBA.NP16, ABBA.NP17, ABBA.NP18, ABBA.NP19, ABBA.NP20, ABBA.NP21, ABBA.NP22, ABBA.NP23, ABBA.NP24, ABBA.NP25, ABBA.NP26, ABBA.NP27, ABBA.NP28, ABBA.NP29, ABBA.NP30, ABBA.NP31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.NP.residplots <- imap(ABBA.NPmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ABBA_NP_glm.pdf")
ABBA.NP.residplots
dev.off()
# if models pass assumptions, proceed. If not, use different error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.NP <- list("ABBA.NP1" = ABBA.NP1, "ABBA.NP2" = ABBA.NP2, "ABBA.NP3" = ABBA.NP3, "ABBA.NP4" = ABBA.NP4, "ABBA.NP5" = ABBA.NP5, "ABBA.NP6" = ABBA.NP6, "ABBA.NP7" = ABBA.NP7, "ABBA.NP8" =  ABBA.NP8, "ABBA.NP9" = ABBA.NP9, "ABBA.NP10" = ABBA.NP10, "ABBA.NP11" = ABBA.NP11, "ABBA.NP12" = ABBA.NP12, "ABBA.NP13" = ABBA.NP13, "ABBA.NP14" = ABBA.NP14, "ABBA.NP15" = ABBA.NP15, "ABBA.NP16" = ABBA.NP16, "ABBA.NP17" = ABBA.NP17, "ABBA.NP18" = ABBA.NP18, "ABBA.NP19" = ABBA.NP19, "ABBA.NP20" = ABBA.NP20, "ABBA.NP21" = ABBA.NP21, "ABBA.NP22" = ABBA.NP22, "ABBA.NP23" = ABBA.NP23, "ABBA.NP24" =  ABBA.NP24, "ABBA.NP25" = ABBA.NP25, "ABBA.NP26" = ABBA.NP26, "ABBA.NP27" = ABBA.NP27, "ABBA.NP28" = ABBA.NP28, "ABBA.NP29" = ABBA.NP29, "ABBA.NP30" = ABBA.NP30, "ABBA.NP31" = ABBA.NP31)
ABBA.NP <- aictab(cand.set = Models.ABBA.NP)
print(ABBA.NP)
write.csv(ABBA.NP, "output/AIC/ABBA_NP.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ABBA.NP20)
summary(ABBA.NP26)
summary(ABBA.NP10)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.NP20, which = "Nagelkerke")
PseudoR2(ABBA.NP26, which = "Nagelkerke")
PseudoR2(ABBA.NP10, which = "Nagelkerke")
#Qty_C
ABBA.Qty_C1 <- glm(Qty_C ~ Year * EVI * GDD * NDMI * Site, data = ABBA)
ABBA.Qty_C2 <- glm(Qty_C ~ Year * EVI * GDD * NDMI, data = ABBA)
ABBA.Qty_C3 <- glm(Qty_C ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.Qty_C4 <- glm(Qty_C ~ Year * EVI * Site * NDMI, data = ABBA)
ABBA.Qty_C5 <- glm(Qty_C ~ Year * Site * GDD * NDMI, data = ABBA) 
ABBA.Qty_C6 <- glm(Qty_C ~ Site * EVI * GDD * NDMI, data = ABBA) 
ABBA.Qty_C7 <- glm(Qty_C ~ Year * EVI * GDD, data = ABBA) 
ABBA.Qty_C8 <- glm(Qty_C ~ Year * EVI * NDMI, data = ABBA)
ABBA.Qty_C9 <- glm(Qty_C ~ Year * EVI * Site, data = ABBA)
ABBA.Qty_C10 <- glm(Qty_C ~ EVI * GDD * NDMI, data = ABBA)
ABBA.Qty_C11 <- glm(Qty_C ~ EVI * GDD * Site, data = ABBA)
ABBA.Qty_C12 <- glm(Qty_C ~ Year * GDD * NDMI, data = ABBA)
ABBA.Qty_C13 <- glm(Qty_C ~ Year * Site * GDD, data = ABBA)
ABBA.Qty_C14 <- glm(Qty_C ~ Year * NDMI * Site, data = ABBA)
ABBA.Qty_C15 <- glm(Qty_C ~ GDD * NDMI * Site, data = ABBA)
ABBA.Qty_C16 <- glm(Qty_C ~ Year * Site, data = ABBA)
ABBA.Qty_C17 <- glm(Qty_C ~ Year * EVI, data = ABBA)
ABBA.Qty_C18 <- glm(Qty_C ~ Year * NDMI, data = ABBA)
ABBA.Qty_C19 <- glm(Qty_C ~ Year * GDD, data = ABBA)
ABBA.Qty_C20 <- glm(Qty_C ~ EVI * Site, data = ABBA)
ABBA.Qty_C21 <- glm(Qty_C ~ NDMI * Site, data = ABBA)
ABBA.Qty_C22 <- glm(Qty_C ~ GDD * Site, data = ABBA)
ABBA.Qty_C23 <- glm(Qty_C ~ EVI * NDMI, data = ABBA)
ABBA.Qty_C24 <- glm(Qty_C ~ NDMI * GDD, data = ABBA)
ABBA.Qty_C25 <- glm(Qty_C ~ GDD * EVI, data = ABBA)
ABBA.Qty_C26 <- glm(Qty_C ~ Site, data = ABBA)
ABBA.Qty_C27 <- glm(Qty_C ~ NDMI, data = ABBA)
ABBA.Qty_C28 <- glm(Qty_C ~ GDD, data = ABBA)
ABBA.Qty_C29 <- glm(Qty_C ~ EVI, data = ABBA) 
ABBA.Qty_C30 <- glm(Qty_C ~ Year, data = ABBA) 
ABBA.Qty_C31 <- glm(Qty_C ~ 1, data =  ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Qty_Cmodels <- list(ABBA.Qty_C1, ABBA.Qty_C2, ABBA.Qty_C3, ABBA.Qty_C4, ABBA.Qty_C5, ABBA.Qty_C6, ABBA.Qty_C7, ABBA.Qty_C8, ABBA.Qty_C9, ABBA.Qty_C10, ABBA.Qty_C11, ABBA.Qty_C12, ABBA.Qty_C13, ABBA.Qty_C14, ABBA.Qty_C15, ABBA.Qty_C16, ABBA.Qty_C17, ABBA.Qty_C18, ABBA.Qty_C19, ABBA.Qty_C20, ABBA.Qty_C21, ABBA.Qty_C22, ABBA.Qty_C23, ABBA.Qty_C24, ABBA.Qty_C25, ABBA.Qty_C26, ABBA.Qty_C27, ABBA.Qty_C28, ABBA.Qty_C29, ABBA.Qty_C30, ABBA.Qty_C31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.Qty_C.residplots <- imap(ABBA.Qty_Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ABBA_QtyC_glm.pdf")
ABBA.Qty_C.residplots
dev.off()
# if models pass assumptions, proceed. If not, use different error structure 

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.Qty_C <- list("ABBA.Qty_C1" = ABBA.Qty_C1, "ABBA.Qty_C2" = ABBA.Qty_C2, "ABBA.Qty_C3" = ABBA.Qty_C3, "ABBA.Qty_C4" = ABBA.Qty_C4, "ABBA.Qty_C5" = ABBA.Qty_C5, "ABBA.Qty_C6" = ABBA.Qty_C6, "ABBA.Qty_C7" = ABBA.Qty_C7, "ABBA.Qty_C8" =  ABBA.Qty_C8, "ABBA.Qty_C9" = ABBA.Qty_C9, "ABBA.Qty_C10" = ABBA.Qty_C10, "ABBA.Qty_C11" = ABBA.Qty_C11, "ABBA.Qty_C12" = ABBA.Qty_C12, "ABBA.Qty_C13" = ABBA.Qty_C13, "ABBA.Qty_C14" = ABBA.Qty_C14, "ABBA.Qty_C15" = ABBA.Qty_C15, "ABBA.Qty_C16" = ABBA.Qty_C16, "ABBA.Qty_C17" = ABBA.Qty_C17, "ABBA.Qty_C18" = ABBA.Qty_C18, "ABBA.Qty_C19" = ABBA.Qty_C19, "ABBA.Qty_C20" = ABBA.Qty_C20, "ABBA.Qty_C21" = ABBA.Qty_C21, "ABBA.Qty_C22" = ABBA.Qty_C22, "ABBA.Qty_C23" = ABBA.Qty_C23, "ABBA.Qty_C24" =  ABBA.Qty_C24, "ABBA.Qty_C25" = ABBA.Qty_C25, "ABBA.Qty_C26" = ABBA.Qty_C26, "ABBA.Qty_C27" = ABBA.Qty_C27, "ABBA.Qty_C28" = ABBA.Qty_C28, "ABBA.Qty_C29" = ABBA.Qty_C29, "ABBA.Qty_C30" = ABBA.Qty_C30, "ABBA.Qty_C31" = ABBA.Qty_C31)
ABBA.Qty_C <- aictab(cand.set = Models.ABBA.Qty_C)
print(ABBA.Qty_C)
write.csv(ABBA.Qty_C, "output/AIC/ABBA_QtyC.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ABBA.Qty_C14)
summary(ABBA.Qty_C7)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.Qty_C14, which = "Nagelkerke")
PseudoR2(ABBA.Qty_C7, which = "Nagelkerke")
#Qty_P
ABBA.Qty_P1 <- glm(Qty_P ~ Year * EVI * GDD * NDMI * Site, data = ABBA)
ABBA.Qty_P2 <- glm(Qty_P ~ Year * EVI * GDD * NDMI, data = ABBA)
ABBA.Qty_P3 <- glm(Qty_P ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.Qty_P4 <- glm(Qty_P ~ Year * EVI * Site * NDMI, data = ABBA)
ABBA.Qty_P5 <- glm(Qty_P ~ Year * Site * GDD * NDMI, data = ABBA) 
ABBA.Qty_P6 <- glm(Qty_P ~ Site * EVI * GDD * NDMI, data = ABBA) 
ABBA.Qty_P7 <- glm(Qty_P ~ Year * EVI * GDD, data = ABBA) 
ABBA.Qty_P8 <- glm(Qty_P ~ Year * EVI * NDMI, data = ABBA)
ABBA.Qty_P9 <- glm(Qty_P ~ Year * EVI * Site, data = ABBA)
ABBA.Qty_P10 <- glm(Qty_P ~ EVI * GDD * NDMI, data = ABBA)
ABBA.Qty_P11 <- glm(Qty_P ~ EVI * GDD * Site, data = ABBA)
ABBA.Qty_P12 <- glm(Qty_P ~ Year * GDD * NDMI, data = ABBA)
ABBA.Qty_P13 <- glm(Qty_P ~ Year * Site * GDD, data = ABBA)
ABBA.Qty_P14 <- glm(Qty_P ~ Year * NDMI * Site, data = ABBA)
ABBA.Qty_P15 <- glm(Qty_P ~ GDD * NDMI * Site, data = ABBA)
ABBA.Qty_P16 <- glm(Qty_P ~ Year * Site, data = ABBA)
ABBA.Qty_P17 <- glm(Qty_P ~ Year * EVI, data = ABBA)
ABBA.Qty_P18 <- glm(Qty_P ~ Year * NDMI, data = ABBA)
ABBA.Qty_P19 <- glm(Qty_P ~ Year * GDD, data = ABBA)
ABBA.Qty_P20 <- glm(Qty_P ~ EVI * Site, data = ABBA)
ABBA.Qty_P21 <- glm(Qty_P ~ NDMI * Site, data = ABBA)
ABBA.Qty_P22 <- glm(Qty_P ~ GDD * Site, data = ABBA)
ABBA.Qty_P23 <- glm(Qty_P ~ EVI * NDMI, data = ABBA)
ABBA.Qty_P24 <- glm(Qty_P ~ NDMI * GDD, data = ABBA)
ABBA.Qty_P25 <- glm(Qty_P ~ GDD * EVI, data = ABBA)
ABBA.Qty_P26 <- glm(Qty_P ~ Site, data = ABBA)
ABBA.Qty_P27 <- glm(Qty_P ~ NDMI, data = ABBA)
ABBA.Qty_P28 <- glm(Qty_P ~ GDD, data = ABBA)
ABBA.Qty_P29 <- glm(Qty_P ~ EVI, data = ABBA) 
ABBA.Qty_P30 <- glm(Qty_P ~ Year, data = ABBA) 
ABBA.Qty_P31 <- glm(Qty_P ~ 1, data =  ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Qty_Pmodels <- list(ABBA.Qty_P1, ABBA.Qty_P2, ABBA.Qty_P3, ABBA.Qty_P4, ABBA.Qty_P5, ABBA.Qty_P6, ABBA.Qty_P7, ABBA.Qty_P8, ABBA.Qty_P9, ABBA.Qty_P10, ABBA.Qty_P11, ABBA.Qty_P12, ABBA.Qty_P13, ABBA.Qty_P14, ABBA.Qty_P15, ABBA.Qty_P16, ABBA.Qty_P17, ABBA.Qty_P18, ABBA.Qty_P19, ABBA.Qty_P20, ABBA.Qty_P21, ABBA.Qty_P22, ABBA.Qty_P23, ABBA.Qty_P24, ABBA.Qty_P25, ABBA.Qty_P26, ABBA.Qty_P27, ABBA.Qty_P28, ABBA.Qty_P29, ABBA.Qty_P30, ABBA.Qty_P31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.Qty_P.residplots <- imap(ABBA.Qty_Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ABBA_QtyP_glm.pdf")
ABBA.Qty_P.residplots
dev.off()

# if models pass assumptions, proceed. If not, use different error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.Qty_P <- list("ABBA.Qty_P1" = ABBA.Qty_P1, "ABBA.Qty_P2" = ABBA.Qty_P2, "ABBA.Qty_P3" = ABBA.Qty_P3, "ABBA.Qty_P4" = ABBA.Qty_P4, "ABBA.Qty_P5" = ABBA.Qty_P5, "ABBA.Qty_P6" = ABBA.Qty_P6, "ABBA.Qty_P7" = ABBA.Qty_P7, "ABBA.Qty_P8" =  ABBA.Qty_P8, "ABBA.Qty_P9" = ABBA.Qty_P9, "ABBA.Qty_P10" = ABBA.Qty_P10, "ABBA.Qty_P11" = ABBA.Qty_P11, "ABBA.Qty_P12" = ABBA.Qty_P12, "ABBA.Qty_P13" = ABBA.Qty_P13, "ABBA.Qty_P14" = ABBA.Qty_P14, "ABBA.Qty_P15" = ABBA.Qty_P15, "ABBA.Qty_P16" = ABBA.Qty_P16, "ABBA.Qty_P17" = ABBA.Qty_P17, "ABBA.Qty_P18" = ABBA.Qty_P18, "ABBA.Qty_P19" = ABBA.Qty_P19, "ABBA.Qty_P20" = ABBA.Qty_P20, "ABBA.Qty_P21" = ABBA.Qty_P21, "ABBA.Qty_P22" = ABBA.Qty_P22, "ABBA.Qty_P23" = ABBA.Qty_P23, "ABBA.Qty_P24" =  ABBA.Qty_P24, "ABBA.Qty_P25" = ABBA.Qty_P25, "ABBA.Qty_P26" = ABBA.Qty_P26, "ABBA.Qty_P27" = ABBA.Qty_P27, "ABBA.Qty_P28" = ABBA.Qty_P28, "ABBA.Qty_P29" = ABBA.Qty_P29, "ABBA.Qty_P30" = ABBA.Qty_P30, "ABBA.Qty_P31" = ABBA.Qty_P31)
ABBA.Qty_P <- aictab(cand.set = Models.ABBA.Qty_P)
print(ABBA.Qty_P)
write.csv(ABBA.Qty_P, "output/AIC/ABBA_QtyP.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ABBA.Qty_P14)
summary(ABBA.Qty_P2)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.Qty_P14, which = "Nagelkerke")
PseudoR2(ABBA.Qty_P2, which = "Nagelkerke")
#Qty_N
ABBA.Qty_N1 <- glm(Qty_N ~ Year * EVI * GDD * NDMI * Site, data = ABBA)
ABBA.Qty_N2 <- glm(Qty_N ~ Year * EVI * GDD * NDMI, data = ABBA)
ABBA.Qty_N3 <- glm(Qty_N ~ Year * EVI * GDD * Site, data = ABBA)
ABBA.Qty_N4 <- glm(Qty_N ~ Year * EVI * Site * NDMI, data = ABBA)
ABBA.Qty_N5 <- glm(Qty_N ~ Year * Site * GDD * NDMI, data = ABBA) 
ABBA.Qty_N6 <- glm(Qty_N ~ Site * EVI * GDD * NDMI, data = ABBA) 
ABBA.Qty_N7 <- glm(Qty_N ~ Year * EVI * GDD, data = ABBA) 
ABBA.Qty_N8 <- glm(Qty_N ~ Year * EVI * NDMI, data = ABBA)
ABBA.Qty_N9 <- glm(Qty_N ~ Year * EVI * Site, data = ABBA)
ABBA.Qty_N10 <- glm(Qty_N ~ EVI * GDD * NDMI, data = ABBA)
ABBA.Qty_N11 <- glm(Qty_N ~ EVI * GDD * Site, data = ABBA)
ABBA.Qty_N12 <- glm(Qty_N ~ Year * GDD * NDMI, data = ABBA)
ABBA.Qty_N13 <- glm(Qty_N ~ Year * Site * GDD, data = ABBA)
ABBA.Qty_N14 <- glm(Qty_N ~ Year * NDMI * Site, data = ABBA)
ABBA.Qty_N15 <- glm(Qty_N ~ GDD * NDMI * Site, data = ABBA)
ABBA.Qty_N16 <- glm(Qty_N ~ Year * Site, data = ABBA)
ABBA.Qty_N17 <- glm(Qty_N ~ Year * EVI, data = ABBA)
ABBA.Qty_N18 <- glm(Qty_N ~ Year * NDMI, data = ABBA)
ABBA.Qty_N19 <- glm(Qty_N ~ Year * GDD, data = ABBA)
ABBA.Qty_N20 <- glm(Qty_N ~ EVI * Site, data = ABBA)
ABBA.Qty_N21 <- glm(Qty_N ~ NDMI * Site, data = ABBA)
ABBA.Qty_N22 <- glm(Qty_N ~ GDD * Site, data = ABBA)
ABBA.Qty_N23 <- glm(Qty_N ~ EVI * NDMI, data = ABBA)
ABBA.Qty_N24 <- glm(Qty_N ~ NDMI * GDD, data = ABBA)
ABBA.Qty_N25 <- glm(Qty_N ~ GDD * EVI, data = ABBA)
ABBA.Qty_N26 <- glm(Qty_N ~ Site, data = ABBA)
ABBA.Qty_N27 <- glm(Qty_N ~ NDMI, data = ABBA)
ABBA.Qty_N28 <- glm(Qty_N ~ GDD, data = ABBA)
ABBA.Qty_N29 <- glm(Qty_N ~ EVI, data = ABBA) 
ABBA.Qty_N30 <- glm(Qty_N ~ Year, data = ABBA) 
ABBA.Qty_N31 <- glm(Qty_N ~ 1, data =  ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Qty_Nmodels <- list(ABBA.Qty_N1, ABBA.Qty_N2, ABBA.Qty_N3, ABBA.Qty_N4, ABBA.Qty_N5, ABBA.Qty_N6, ABBA.Qty_N7, ABBA.Qty_N8, ABBA.Qty_N9, ABBA.Qty_N10, ABBA.Qty_N11, ABBA.Qty_N12, ABBA.Qty_N13, ABBA.Qty_N14, ABBA.Qty_N15, ABBA.Qty_N16, ABBA.Qty_N17, ABBA.Qty_N18, ABBA.Qty_N19, ABBA.Qty_N20, ABBA.Qty_N21, ABBA.Qty_N22, ABBA.Qty_N23, ABBA.Qty_N24, ABBA.Qty_N25, ABBA.Qty_N26, ABBA.Qty_N27, ABBA.Qty_N28, ABBA.Qty_N29, ABBA.Qty_N30, ABBA.Qty_N31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.Qty_N.residplots <- imap(ABBA.Qty_Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ABBA_QtyN_glm.pdf")
ABBA.Qty_N.residplots
dev.off()

# if models pass assumptions, proceed. If not, use different error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.Qty_N <- list("ABBA.Qty_N1" = ABBA.Qty_N1, "ABBA.Qty_N2" = ABBA.Qty_N2, "ABBA.Qty_N3" = ABBA.Qty_N3, "ABBA.Qty_N4" = ABBA.Qty_N4, "ABBA.Qty_N5" = ABBA.Qty_N5, "ABBA.Qty_N6" = ABBA.Qty_N6, "ABBA.Qty_N7" = ABBA.Qty_N7, "ABBA.Qty_N8" =  ABBA.Qty_N8, "ABBA.Qty_N9" = ABBA.Qty_N9, "ABBA.Qty_N10" = ABBA.Qty_N10, "ABBA.Qty_N11" = ABBA.Qty_N11, "ABBA.Qty_N12" = ABBA.Qty_N12, "ABBA.Qty_N13" = ABBA.Qty_N13, "ABBA.Qty_N14" = ABBA.Qty_N14, "ABBA.Qty_N15" = ABBA.Qty_N15, "ABBA.Qty_N16" = ABBA.Qty_N16, "ABBA.Qty_N17" = ABBA.Qty_N17, "ABBA.Qty_N18" = ABBA.Qty_N18, "ABBA.Qty_N19" = ABBA.Qty_N19, "ABBA.Qty_N20" = ABBA.Qty_N20, "ABBA.Qty_N21" = ABBA.Qty_N21, "ABBA.Qty_N22" = ABBA.Qty_N22, "ABBA.Qty_N23" = ABBA.Qty_N23, "ABBA.Qty_N24" =  ABBA.Qty_N24, "ABBA.Qty_N25" = ABBA.Qty_N25, "ABBA.Qty_N26" = ABBA.Qty_N26, "ABBA.Qty_N27" = ABBA.Qty_N27, "ABBA.Qty_N28" = ABBA.Qty_N28, "ABBA.Qty_N29" = ABBA.Qty_N29, "ABBA.Qty_N30" = ABBA.Qty_N30, "ABBA.Qty_N31" = ABBA.Qty_N31)
ABBA.Qty_N <- aictab(cand.set = Models.ABBA.Qty_N)
print(ABBA.Qty_N)
write.csv(ABBA.Qty_N, "output/AIC/ABBA_QtyN.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ABBA.Qty_N14)
summary(ABBA.Qty_N2)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.Qty_N14, which = "Nagelkerke")
PseudoR2(ABBA.Qty_N2, which = "Nagelkerke")

# ACRU
# %C
ACRU.C1 <- glm(C ~ Year * EVI * GDD * NDMI * Site, data = ACRU)
ACRU.C2 <- glm(C ~ Year * EVI * GDD * NDMI, data = ACRU)
ACRU.C3 <- glm(C ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.C4 <- glm(C ~ Year * EVI * Site * NDMI, data = ACRU)
ACRU.C5 <- glm(C ~ Year * Site * GDD * NDMI, data = ACRU) 
ACRU.C6 <- glm(C ~ Site * EVI * GDD * NDMI, data = ACRU) 
ACRU.C7 <- glm(C ~ Year * EVI * GDD, data = ACRU) 
ACRU.C8 <- glm(C ~ Year * EVI * NDMI, data = ACRU)
ACRU.C9 <- glm(C ~ Year * EVI * Site, data = ACRU)
ACRU.C10 <- glm(C ~ EVI * GDD * NDMI, data = ACRU)
ACRU.C11 <- glm(C ~ EVI * GDD * Site, data = ACRU)
ACRU.C12 <- glm(C ~ Year * GDD * NDMI, data = ACRU)
ACRU.C13 <- glm(C ~ Year * Site * GDD, data = ACRU)
ACRU.C14 <- glm(C ~ Year * NDMI * Site, data = ACRU)
ACRU.C15 <- glm(C ~ GDD * NDMI * Site, data = ACRU)
ACRU.C16 <- glm(C ~ Year * Site, data = ACRU)
ACRU.C17 <- glm(C ~ Year * EVI, data = ACRU)
ACRU.C18 <- glm(C ~ Year * NDMI, data = ACRU)
ACRU.C19 <- glm(C ~ Year * GDD, data = ACRU)
ACRU.C20 <- glm(C ~ EVI * Site, data = ACRU)
ACRU.C21 <- glm(C ~ NDMI * Site, data = ACRU)
ACRU.C22 <- glm(C ~ GDD * Site, data = ACRU)
ACRU.C23 <- glm(C ~ EVI * NDMI, data = ACRU)
ACRU.C24 <- glm(C ~ NDMI * GDD, data = ACRU)
ACRU.C25 <- glm(C ~ GDD * EVI, data = ACRU)
ACRU.C26 <- glm(C ~ Site, data = ACRU)
ACRU.C27 <- glm(C ~ NDMI, data = ACRU)
ACRU.C28 <- glm(C ~ GDD, data = ACRU)
ACRU.C29 <- glm(C ~ EVI, data = ACRU) 
ACRU.C30 <- glm(C ~ Year, data = ACRU) 
ACRU.C31 <- glm(C ~ 1, data =  ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Cmodels <- list(ACRU.C1, ACRU.C2, ACRU.C3, ACRU.C4, ACRU.C5, ACRU.C6, ACRU.C7, ACRU.C8, ACRU.C9, ACRU.C10, ACRU.C11, ACRU.C12, ACRU.C13, ACRU.C14, ACRU.C15, ACRU.C16, ACRU.C17, ACRU.C18, ACRU.C19, ACRU.C20, ACRU.C21, ACRU.C22, ACRU.C23, ACRU.C24, ACRU.C25, ACRU.C26, ACRU.C27, ACRU.C28, ACRU.C29, ACRU.C30, ACRU.C31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.C.residplots <- imap(ACRU.Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ACRU_C_glm.pdf")
ACRU.C.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.C <- list("ACRU.C1" = ACRU.C1, "ACRU.C2" = ACRU.C2, "ACRU.C3" = ACRU.C3, "ACRU.C4" = ACRU.C4, "ACRU.C5" = ACRU.C5, "ACRU.C6" = ACRU.C6, "ACRU.C7" = ACRU.C7, "ACRU.C8" =  ACRU.C8, "ACRU.C9" = ACRU.C9, "ACRU.C10" = ACRU.C10, "ACRU.C11" = ACRU.C11, "ACRU.C12" = ACRU.C12, "ACRU.C13" = ACRU.C13, "ACRU.C14" = ACRU.C14, "ACRU.C15" = ACRU.C15, "ACRU.C16" = ACRU.C16, "ACRU.C17" = ACRU.C17, "ACRU.C18" = ACRU.C18, "ACRU.C19" = ACRU.C19, "ACRU.C20" = ACRU.C20, "ACRU.C21" = ACRU.C21, "ACRU.C22" = ACRU.C22, "ACRU.C23" = ACRU.C23, "ACRU.C24" =  ACRU.C24, "ACRU.C25" = ACRU.C25, "ACRU.C26" = ACRU.C26, "ACRU.C27" = ACRU.C27, "ACRU.C28" = ACRU.C28, "ACRU.C29" = ACRU.C29, "ACRU.C30" = ACRU.C30, "ACRU.C31" = ACRU.C31)
ACRU.C <- aictab(cand.set = Models.ACRU.C)
print(ACRU.C)
write.csv(ACRU.C, "output/AIC/ACRU_C.csv")
# summary of the most parsimonious models - delta AICc of less than 2
summary(ACRU.C3)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.C3, which = "Nagelkerke")
#%P
ACRU.P1 <- glm(P ~ Year * EVI * GDD * NDMI * Site, data = ACRU)
ACRU.P2 <- glm(P ~ Year * EVI * GDD * NDMI, data = ACRU)
ACRU.P3 <- glm(P ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.P4 <- glm(P ~ Year * EVI * Site * NDMI, data = ACRU)
ACRU.P5 <- glm(P ~ Year * Site * GDD * NDMI, data = ACRU) 
ACRU.P6 <- glm(P ~ Site * EVI * GDD * NDMI, data = ACRU) 
ACRU.P7 <- glm(P ~ Year * EVI * GDD, data = ACRU) 
ACRU.P8 <- glm(P ~ Year * EVI * NDMI, data = ACRU)
ACRU.P9 <- glm(P ~ Year * EVI * Site, data = ACRU)
ACRU.P10 <- glm(P ~ EVI * GDD * NDMI, data = ACRU)
ACRU.P11 <- glm(P ~ EVI * GDD * Site, data = ACRU)
ACRU.P12 <- glm(P ~ Year * GDD * NDMI, data = ACRU)
ACRU.P13 <- glm(P ~ Year * Site * GDD, data = ACRU)
ACRU.P14 <- glm(P ~ Year * NDMI * Site, data = ACRU)
ACRU.P15 <- glm(P ~ GDD * NDMI * Site, data = ACRU)
ACRU.P16 <- glm(P ~ Year * Site, data = ACRU)
ACRU.P17 <- glm(P ~ Year * EVI, data = ACRU)
ACRU.P18 <- glm(P ~ Year * NDMI, data = ACRU)
ACRU.P19 <- glm(P ~ Year * GDD, data = ACRU)
ACRU.P20 <- glm(P ~ EVI * Site, data = ACRU)
ACRU.P21 <- glm(P ~ NDMI * Site, data = ACRU)
ACRU.P22 <- glm(P ~ GDD * Site, data = ACRU)
ACRU.P23 <- glm(P ~ EVI * NDMI, data = ACRU)
ACRU.P24 <- glm(P ~ NDMI * GDD, data = ACRU)
ACRU.P25 <- glm(P ~ GDD * EVI, data = ACRU)
ACRU.P26 <- glm(P ~ Site, data = ACRU)
ACRU.P27 <- glm(P ~ NDMI, data = ACRU)
ACRU.P28 <- glm(P ~ GDD, data = ACRU)
ACRU.P29 <- glm(P ~ EVI, data = ACRU) 
ACRU.P30 <- glm(P ~ Year, data = ACRU) 
ACRU.P31 <- glm(P ~ 1, data =  ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Pmodels <- list(ACRU.P1, ACRU.P2, ACRU.P3, ACRU.P4, ACRU.P5, ACRU.P6, ACRU.P7, ACRU.P8, ACRU.P9, ACRU.P10, ACRU.P11, ACRU.P12, ACRU.P13, ACRU.P14, ACRU.P15, ACRU.P16, ACRU.P17, ACRU.P18, ACRU.P19, ACRU.P20, ACRU.P21, ACRU.P22, ACRU.P23, ACRU.P24, ACRU.P25, ACRU.P26, ACRU.P27, ACRU.P28, ACRU.P29, ACRU.P30, ACRU.P31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.P.residplots <- imap(ACRU.Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ACRU_P_glm.pdf")
ACRU.P.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.P <- list("ACRU.P1" = ACRU.P1, "ACRU.P2" = ACRU.P2, "ACRU.P3" = ACRU.P3, "ACRU.P4" = ACRU.P4, "ACRU.P5" = ACRU.P5, "ACRU.P6" = ACRU.P6, "ACRU.P7" = ACRU.P7, "ACRU.P8" =  ACRU.P8, "ACRU.P9" = ACRU.P9, "ACRU.P10" = ACRU.P10, "ACRU.P11" = ACRU.P11, "ACRU.P12" = ACRU.P12, "ACRU.P13" = ACRU.P13, "ACRU.P14" = ACRU.P14, "ACRU.P15" = ACRU.P15, "ACRU.P16" = ACRU.P16, "ACRU.P17" = ACRU.P17, "ACRU.P18" = ACRU.P18, "ACRU.P19" = ACRU.P19, "ACRU.P20" = ACRU.P20, "ACRU.P21" = ACRU.P21, "ACRU.P22" = ACRU.P22, "ACRU.P23" = ACRU.P23, "ACRU.P24" =  ACRU.P24, "ACRU.P25" = ACRU.P25, "ACRU.P26" = ACRU.P26, "ACRU.P27" = ACRU.P27, "ACRU.P28" = ACRU.P28, "ACRU.P29" = ACRU.P29, "ACRU.P30" = ACRU.P30, "ACRU.P31" = ACRU.P31)
ACRU.P <- aictab(cand.set = Models.ACRU.P)
print(ACRU.P)
write.csv(ACRU.P, "output/AIC/ACRU_P.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ACRU.P21)
summary(ACRU.P6)
summary(ACRU.P10)
summary(ACRU.P24)
summary(ACRU.P29)
summary(ACRU.P31)
summary(ACRU.P8)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.P21, which = "Nagelkerke")
PseudoR2(ACRU.P6, which = "Nagelkerke")
PseudoR2(ACRU.P10, which = "Nagelkerke")
PseudoR2(ACRU.P24, which = "Nagelkerke")
PseudoR2(ACRU.P29, which = "Nagelkerke")
PseudoR2(ACRU.P31, which = "Nagelkerke")
PseudoR2(ACRU.P8, which = "Nagelkerke")
#%N
ACRU.N1 <- glm(N ~ Year * EVI * GDD * NDMI * Site, data = ACRU)
ACRU.N2 <- glm(N ~ Year * EVI * GDD * NDMI, data = ACRU)
ACRU.N3 <- glm(N ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.N4 <- glm(N ~ Year * EVI * Site * NDMI, data = ACRU)
ACRU.N5 <- glm(N ~ Year * Site * GDD * NDMI, data = ACRU) 
ACRU.N6 <- glm(N ~ Site * EVI * GDD * NDMI, data = ACRU) 
ACRU.N7 <- glm(N ~ Year * EVI * GDD, data = ACRU) 
ACRU.N8 <- glm(N ~ Year * EVI * NDMI, data = ACRU)
ACRU.N9 <- glm(N ~ Year * EVI * Site, data = ACRU)
ACRU.N10 <- glm(N ~ EVI * GDD * NDMI, data = ACRU)
ACRU.N11 <- glm(N ~ EVI * GDD * Site, data = ACRU)
ACRU.N12 <- glm(N ~ Year * GDD * NDMI, data = ACRU)
ACRU.N13 <- glm(N ~ Year * Site * GDD, data = ACRU)
ACRU.N14 <- glm(N ~ Year * NDMI * Site, data = ACRU)
ACRU.N15 <- glm(N ~ GDD * NDMI * Site, data = ACRU)
ACRU.N16 <- glm(N ~ Year * Site, data = ACRU)
ACRU.N17 <- glm(N ~ Year * EVI, data = ACRU)
ACRU.N18 <- glm(N ~ Year * NDMI, data = ACRU)
ACRU.N19 <- glm(N ~ Year * GDD, data = ACRU)
ACRU.N20 <- glm(N ~ EVI * Site, data = ACRU)
ACRU.N21 <- glm(N ~ NDMI * Site, data = ACRU)
ACRU.N22 <- glm(N ~ GDD * Site, data = ACRU)
ACRU.N23 <- glm(N ~ EVI * NDMI, data = ACRU)
ACRU.N24 <- glm(N ~ NDMI * GDD, data = ACRU)
ACRU.N25 <- glm(N ~ GDD * EVI, data = ACRU)
ACRU.N26 <- glm(N ~ Site, data = ACRU)
ACRU.N27 <- glm(N ~ NDMI, data = ACRU)
ACRU.N28 <- glm(N ~ GDD, data = ACRU)
ACRU.N29 <- glm(N ~ EVI, data = ACRU) 
ACRU.N30 <- glm(N ~ Year, data = ACRU) 
ACRU.N31 <- glm(N ~ 1, data =  ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Nmodels <- list(ACRU.N1, ACRU.N2, ACRU.N3, ACRU.N4, ACRU.N5, ACRU.N6, ACRU.N7, ACRU.N8, ACRU.N9, ACRU.N10, ACRU.N11, ACRU.N12, ACRU.N13, ACRU.N14, ACRU.N15, ACRU.N16, ACRU.N17, ACRU.N18, ACRU.N19, ACRU.N20, ACRU.N21, ACRU.N22, ACRU.N23, ACRU.N24, ACRU.N25, ACRU.N26, ACRU.N27, ACRU.N28, ACRU.N29, ACRU.N30, ACRU.N31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.N.residplots <- imap(ACRU.Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ACRU_N_glm.pdf")
ACRU.N.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.N <- list("ACRU.N1" = ACRU.N1, "ACRU.N2" = ACRU.N2, "ACRU.N3" = ACRU.N3, "ACRU.N4" = ACRU.N4, "ACRU.N5" = ACRU.N5, "ACRU.N6" = ACRU.N6, "ACRU.N7" = ACRU.N7, "ACRU.N8" =  ACRU.N8, "ACRU.N9" = ACRU.N9, "ACRU.N10" = ACRU.N10, "ACRU.N11" = ACRU.N11, "ACRU.N12" = ACRU.N12, "ACRU.N13" = ACRU.N13, "ACRU.N14" = ACRU.N14, "ACRU.N15" = ACRU.N15, "ACRU.N16" = ACRU.N16, "ACRU.N17" = ACRU.N17, "ACRU.N18" = ACRU.N18, "ACRU.N19" = ACRU.N19, "ACRU.N20" = ACRU.N20, "ACRU.N21" = ACRU.N21, "ACRU.N22" = ACRU.N22, "ACRU.N23" = ACRU.N23, "ACRU.N24" =  ACRU.N24, "ACRU.N25" = ACRU.N25, "ACRU.N26" = ACRU.N26, "ACRU.N27" = ACRU.N27, "ACRU.N28" = ACRU.N28, "ACRU.N29" = ACRU.N29, "ACRU.N30" = ACRU.N30, "ACRU.N31" = ACRU.N31)
ACRU.N <- aictab(cand.set = Models.ACRU.N)
print(ACRU.N)
write.csv(ACRU.N, "output/AIC/ACRU_N.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ACRU.N12)
summary(ACRU.N10)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.N12, which = "Nagelkerke")
PseudoR2(ACRU.N10, which = "Nagelkerke")
#C:N
ACRU.CN1 <- glm(CNRatio ~ Year * EVI * GDD * NDMI * Site, data = ACRU)
ACRU.CN2 <- glm(CNRatio ~ Year * EVI * GDD * NDMI, data = ACRU)
ACRU.CN3 <- glm(CNRatio ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.CN4 <- glm(CNRatio ~ Year * EVI * Site * NDMI, data = ACRU)
ACRU.CN5 <- glm(CNRatio ~ Year * Site * GDD * NDMI, data = ACRU) 
ACRU.CN6 <- glm(CNRatio ~ Site * EVI * GDD * NDMI, data = ACRU) 
ACRU.CN7 <- glm(CNRatio ~ Year * EVI * GDD, data = ACRU) 
ACRU.CN8 <- glm(CNRatio ~ Year * EVI * NDMI, data = ACRU)
ACRU.CN9 <- glm(CNRatio ~ Year * EVI * Site, data = ACRU)
ACRU.CN10 <- glm(CNRatio ~ EVI * GDD * NDMI, data = ACRU)
ACRU.CN11 <- glm(CNRatio ~ EVI * GDD * Site, data = ACRU)
ACRU.CN12 <- glm(CNRatio ~ Year * GDD * NDMI, data = ACRU)
ACRU.CN13 <- glm(CNRatio ~ Year * Site * GDD, data = ACRU)
ACRU.CN14 <- glm(CNRatio ~ Year * NDMI * Site, data = ACRU)
ACRU.CN15 <- glm(CNRatio ~ GDD * NDMI * Site, data = ACRU)
ACRU.CN16 <- glm(CNRatio ~ Year * Site, data = ACRU)
ACRU.CN17 <- glm(CNRatio ~ Year * EVI, data = ACRU)
ACRU.CN18 <- glm(CNRatio ~ Year * NDMI, data = ACRU)
ACRU.CN19 <- glm(CNRatio ~ Year * GDD, data = ACRU)
ACRU.CN20 <- glm(CNRatio ~ EVI * Site, data = ACRU)
ACRU.CN21 <- glm(CNRatio ~ NDMI * Site, data = ACRU)
ACRU.CN22 <- glm(CNRatio ~ GDD * Site, data = ACRU)
ACRU.CN23 <- glm(CNRatio ~ EVI * NDMI, data = ACRU)
ACRU.CN24 <- glm(CNRatio ~ NDMI * GDD, data = ACRU)
ACRU.CN25 <- glm(CNRatio ~ GDD * EVI, data = ACRU)
ACRU.CN26 <- glm(CNRatio ~ Site, data = ACRU)
ACRU.CN27 <- glm(CNRatio ~ NDMI, data = ACRU)
ACRU.CN28 <- glm(CNRatio ~ GDD, data = ACRU)
ACRU.CN29 <- glm(CNRatio ~ EVI, data = ACRU) 
ACRU.CN30 <- glm(CNRatio ~ Year, data = ACRU) 
ACRU.CN31 <- glm(CNRatio ~ 1, data =  ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.CNmodels <- list(ACRU.CN1, ACRU.CN2, ACRU.CN3, ACRU.CN4, ACRU.CN5, ACRU.CN6, ACRU.CN7, ACRU.CN8, ACRU.CN9, ACRU.CN10, ACRU.CN11, ACRU.CN12, ACRU.CN13, ACRU.CN14, ACRU.CN15, ACRU.CN16, ACRU.CN17, ACRU.CN18, ACRU.CN19, ACRU.CN20, ACRU.CN21, ACRU.CN22, ACRU.CN23, ACRU.CN24, ACRU.CN25, ACRU.CN26, ACRU.CN27, ACRU.CN28, ACRU.CN29, ACRU.CN30, ACRU.CN31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.CN.residplots <- imap(ACRU.CNmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ACRU_CN_glm.pdf")
ACRU.CN.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.CN <- list("ACRU.CN1" = ACRU.CN1, "ACRU.CN2" = ACRU.CN2, "ACRU.CN3" = ACRU.CN3, "ACRU.CN4" = ACRU.CN4, "ACRU.CN5" = ACRU.CN5, "ACRU.CN6" = ACRU.CN6, "ACRU.CN7" = ACRU.CN7, "ACRU.CN8" =  ACRU.CN8, "ACRU.CN9" = ACRU.CN9, "ACRU.CN10" = ACRU.CN10, "ACRU.CN11" = ACRU.CN11, "ACRU.CN12" = ACRU.CN12, "ACRU.CN13" = ACRU.CN13, "ACRU.CN14" = ACRU.CN14, "ACRU.CN15" = ACRU.CN15, "ACRU.CN16" = ACRU.CN16, "ACRU.CN17" = ACRU.CN17, "ACRU.CN18" = ACRU.CN18, "ACRU.CN19" = ACRU.CN19, "ACRU.CN20" = ACRU.CN20, "ACRU.CN21" = ACRU.CN21, "ACRU.CN22" = ACRU.CN22, "ACRU.CN23" = ACRU.CN23, "ACRU.CN24" =  ACRU.CN24, "ACRU.CN25" = ACRU.CN25, "ACRU.CN26" = ACRU.CN26, "ACRU.CN27" = ACRU.CN27, "ACRU.CN28" = ACRU.CN28, "ACRU.CN29" = ACRU.CN29, "ACRU.CN30" = ACRU.CN30, "ACRU.CN31" = ACRU.CN31)
ACRU.CN <- aictab(cand.set = Models.ACRU.CN)
print(ACRU.CN)
write.csv(ACRU.CN, "output/AIC/ACRU_CN.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ACRU.CN10)
summary(ACRU.CN12)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.CN10, which = "Nagelkerke")
PseudoR2(ACRU.CN12, which = "Nagelkerke")
#C:P
ACRU.CP1 <- glm(CPRatio ~ Year * EVI * GDD * NDMI * Site, data = ACRU)
ACRU.CP2 <- glm(CPRatio ~ Year * EVI * GDD * NDMI, data = ACRU)
ACRU.CP3 <- glm(CPRatio ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.CP4 <- glm(CPRatio ~ Year * EVI * Site * NDMI, data = ACRU)
ACRU.CP5 <- glm(CPRatio ~ Year * Site * GDD * NDMI, data = ACRU) 
ACRU.CP6 <- glm(CPRatio ~ Site * EVI * GDD * NDMI, data = ACRU) 
ACRU.CP7 <- glm(CPRatio ~ Year * EVI * GDD, data = ACRU) 
ACRU.CP8 <- glm(CPRatio ~ Year * EVI * NDMI, data = ACRU)
ACRU.CP9 <- glm(CPRatio ~ Year * EVI * Site, data = ACRU)
ACRU.CP10 <- glm(CPRatio ~ EVI * GDD * NDMI, data = ACRU)
ACRU.CP11 <- glm(CPRatio ~ EVI * GDD * Site, data = ACRU)
ACRU.CP12 <- glm(CPRatio ~ Year * GDD * NDMI, data = ACRU)
ACRU.CP13 <- glm(CPRatio ~ Year * Site * GDD, data = ACRU)
ACRU.CP14 <- glm(CPRatio ~ Year * NDMI * Site, data = ACRU)
ACRU.CP15 <- glm(CPRatio ~ GDD * NDMI * Site, data = ACRU)
ACRU.CP16 <- glm(CPRatio ~ Year * Site, data = ACRU)
ACRU.CP17 <- glm(CPRatio ~ Year * EVI, data = ACRU)
ACRU.CP18 <- glm(CPRatio ~ Year * NDMI, data = ACRU)
ACRU.CP19 <- glm(CPRatio ~ Year * GDD, data = ACRU)
ACRU.CP20 <- glm(CPRatio ~ EVI * Site, data = ACRU)
ACRU.CP21 <- glm(CPRatio ~ NDMI * Site, data = ACRU)
ACRU.CP22 <- glm(CPRatio ~ GDD * Site, data = ACRU)
ACRU.CP23 <- glm(CPRatio ~ EVI * NDMI, data = ACRU)
ACRU.CP24 <- glm(CPRatio ~ NDMI * GDD, data = ACRU)
ACRU.CP25 <- glm(CPRatio ~ GDD * EVI, data = ACRU)
ACRU.CP26 <- glm(CPRatio ~ Site, data = ACRU)
ACRU.CP27 <- glm(CPRatio ~ NDMI, data = ACRU)
ACRU.CP28 <- glm(CPRatio ~ GDD, data = ACRU)
ACRU.CP29 <- glm(CPRatio ~ EVI, data = ACRU) 
ACRU.CP30 <- glm(CPRatio ~ Year, data = ACRU) 
ACRU.CP31 <- glm(CPRatio ~ 1, data =  ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.CPmodels <- list(ACRU.CP1, ACRU.CP2, ACRU.CP3, ACRU.CP4, ACRU.CP5, ACRU.CP6, ACRU.CP7, ACRU.CP8, ACRU.CP9, ACRU.CP10, ACRU.CP11, ACRU.CP12, ACRU.CP13, ACRU.CP14, ACRU.CP15, ACRU.CP16, ACRU.CP17, ACRU.CP18, ACRU.CP19, ACRU.CP20, ACRU.CP21, ACRU.CP22, ACRU.CP23, ACRU.CP24, ACRU.CP25, ACRU.CP26, ACRU.CP27, ACRU.CP28, ACRU.CP29, ACRU.CP30, ACRU.CP31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.CP.residplots <- imap(ACRU.CPmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ACRU_CP_glm.pdf")
ACRU.CP.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.CP <- list("ACRU.CP1" = ACRU.CP1, "ACRU.CP2" = ACRU.CP2, "ACRU.CP3" = ACRU.CP3, "ACRU.CP4" = ACRU.CP4, "ACRU.CP5" = ACRU.CP5, "ACRU.CP6" = ACRU.CP6, "ACRU.CP7" = ACRU.CP7, "ACRU.CP8" =  ACRU.CP8, "ACRU.CP9" = ACRU.CP9, "ACRU.CP10" = ACRU.CP10, "ACRU.CP11" = ACRU.CP11, "ACRU.CP12" = ACRU.CP12, "ACRU.CP13" = ACRU.CP13, "ACRU.CP14" = ACRU.CP14, "ACRU.CP15" = ACRU.CP15, "ACRU.CP16" = ACRU.CP16, "ACRU.CP17" = ACRU.CP17, "ACRU.CP18" = ACRU.CP18, "ACRU.CP19" = ACRU.CP19, "ACRU.CP20" = ACRU.CP20, "ACRU.CP21" = ACRU.CP21, "ACRU.CP22" = ACRU.CP22, "ACRU.CP23" = ACRU.CP23, "ACRU.CP24" =  ACRU.CP24, "ACRU.CP25" = ACRU.CP25, "ACRU.CP26" = ACRU.CP26, "ACRU.CP27" = ACRU.CP27, "ACRU.CP28" = ACRU.CP28, "ACRU.CP29" = ACRU.CP29, "ACRU.CP30" = ACRU.CP30, "ACRU.CP31" = ACRU.CP31)
ACRU.CP <- aictab(cand.set = Models.ACRU.CP)
print(ACRU.CP)
write.csv(ACRU.CP, "output/AIC/ACRU_CP.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ACRU.CP6)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.CP6, which = "Nagelkerke")
#N:P
ACRU.NP1 <- glm(NPRatio ~ Year * EVI * GDD * NDMI * Site, data = ACRU)
ACRU.NP2 <- glm(NPRatio ~ Year * EVI * GDD * NDMI, data = ACRU)
ACRU.NP3 <- glm(NPRatio ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.NP4 <- glm(NPRatio ~ Year * EVI * Site * NDMI, data = ACRU)
ACRU.NP5 <- glm(NPRatio ~ Year * Site * GDD * NDMI, data = ACRU) 
ACRU.NP6 <- glm(NPRatio ~ Site * EVI * GDD * NDMI, data = ACRU) 
ACRU.NP7 <- glm(NPRatio ~ Year * EVI * GDD, data = ACRU) 
ACRU.NP8 <- glm(NPRatio ~ Year * EVI * NDMI, data = ACRU)
ACRU.NP9 <- glm(NPRatio ~ Year * EVI * Site, data = ACRU)
ACRU.NP10 <- glm(NPRatio ~ EVI * GDD * NDMI, data = ACRU)
ACRU.NP11 <- glm(NPRatio ~ EVI * GDD * Site, data = ACRU)
ACRU.NP12 <- glm(NPRatio ~ Year * GDD * NDMI, data = ACRU)
ACRU.NP13 <- glm(NPRatio ~ Year * Site * GDD, data = ACRU)
ACRU.NP14 <- glm(NPRatio ~ Year * NDMI * Site, data = ACRU)
ACRU.NP15 <- glm(NPRatio ~ GDD * NDMI * Site, data = ACRU)
ACRU.NP16 <- glm(NPRatio ~ Year * Site, data = ACRU)
ACRU.NP17 <- glm(NPRatio ~ Year * EVI, data = ACRU)
ACRU.NP18 <- glm(NPRatio ~ Year * NDMI, data = ACRU)
ACRU.NP19 <- glm(NPRatio ~ Year * GDD, data = ACRU)
ACRU.NP20 <- glm(NPRatio ~ EVI * Site, data = ACRU)
ACRU.NP21 <- glm(NPRatio ~ NDMI * Site, data = ACRU)
ACRU.NP22 <- glm(NPRatio ~ GDD * Site, data = ACRU)
ACRU.NP23 <- glm(NPRatio ~ EVI * NDMI, data = ACRU)
ACRU.NP24 <- glm(NPRatio ~ NDMI * GDD, data = ACRU)
ACRU.NP25 <- glm(NPRatio ~ GDD * EVI, data = ACRU)
ACRU.NP26 <- glm(NPRatio ~ Site, data = ACRU)
ACRU.NP27 <- glm(NPRatio ~ NDMI, data = ACRU)
ACRU.NP28 <- glm(NPRatio ~ GDD, data = ACRU)
ACRU.NP29 <- glm(NPRatio ~ EVI, data = ACRU) 
ACRU.NP30 <- glm(NPRatio ~ Year, data = ACRU) 
ACRU.NP31 <- glm(NPRatio ~ 1, data =  ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.NPmodels <- list(ACRU.NP1, ACRU.NP2, ACRU.NP3, ACRU.NP4, ACRU.NP5, ACRU.NP6, ACRU.NP7, ACRU.NP8, ACRU.NP9, ACRU.NP10, ACRU.NP11, ACRU.NP12, ACRU.NP13, ACRU.NP14, ACRU.NP15, ACRU.NP16, ACRU.NP17, ACRU.NP18, ACRU.NP19, ACRU.NP20, ACRU.NP21, ACRU.NP22, ACRU.NP23, ACRU.NP24, ACRU.NP25, ACRU.NP26, ACRU.NP27, ACRU.NP28, ACRU.NP29, ACRU.NP30, ACRU.NP31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.NP.residplots <- imap(ACRU.NPmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ACRU_NP_glm.pdf")
ACRU.NP.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.NP <- list("ACRU.NP1" = ACRU.NP1, "ACRU.NP2" = ACRU.NP2, "ACRU.NP3" = ACRU.NP3, "ACRU.NP4" = ACRU.NP4, "ACRU.NP5" = ACRU.NP5, "ACRU.NP6" = ACRU.NP6, "ACRU.NP7" = ACRU.NP7, "ACRU.NP8" =  ACRU.NP8, "ACRU.NP9" = ACRU.NP9, "ACRU.NP10" = ACRU.NP10, "ACRU.NP11" = ACRU.NP11, "ACRU.NP12" = ACRU.NP12, "ACRU.NP13" = ACRU.NP13, "ACRU.NP14" = ACRU.NP14, "ACRU.NP15" = ACRU.NP15, "ACRU.NP16" = ACRU.NP16, "ACRU.NP17" = ACRU.NP17, "ACRU.NP18" = ACRU.NP18, "ACRU.NP19" = ACRU.NP19, "ACRU.NP20" = ACRU.NP20, "ACRU.NP21" = ACRU.NP21, "ACRU.NP22" = ACRU.NP22, "ACRU.NP23" = ACRU.NP23, "ACRU.NP24" =  ACRU.NP24, "ACRU.NP25" = ACRU.NP25, "ACRU.NP26" = ACRU.NP26, "ACRU.NP27" = ACRU.NP27, "ACRU.NP28" = ACRU.NP28, "ACRU.NP29" = ACRU.NP29, "ACRU.NP30" = ACRU.NP30, "ACRU.NP31" = ACRU.NP31)
ACRU.NP <- aictab(cand.set = Models.ACRU.NP)
print(ACRU.NP)
write.csv(ACRU.NP, "output/AIC/ACRU_NP.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ACRU.NP21)
summary(ACRU.NP23)
summary(ACRU.NP8)
summary(ACRU.NP29)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.NP21, which = "Nagelkerke")
PseudoR2(ACRU.NP23, which = "Nagelkerke")
PseudoR2(ACRU.NP8, which = "Nagelkerke")
PseudoR2(ACRU.NP29, which = "Nagelkerke")
#Qty_C
ACRU.Qty_C1 <- glm(Qty_C ~ Year * EVI * GDD * NDMI * Site, data = ACRU)
ACRU.Qty_C2 <- glm(Qty_C ~ Year * EVI * GDD * NDMI, data = ACRU)
ACRU.Qty_C3 <- glm(Qty_C ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.Qty_C4 <- glm(Qty_C ~ Year * EVI * Site * NDMI, data = ACRU)
ACRU.Qty_C5 <- glm(Qty_C ~ Year * Site * GDD * NDMI, data = ACRU) 
ACRU.Qty_C6 <- glm(Qty_C ~ Site * EVI * GDD * NDMI, data = ACRU) 
ACRU.Qty_C7 <- glm(Qty_C ~ Year * EVI * GDD, data = ACRU) 
ACRU.Qty_C8 <- glm(Qty_C ~ Year * EVI * NDMI, data = ACRU)
ACRU.Qty_C9 <- glm(Qty_C ~ Year * EVI * Site, data = ACRU)
ACRU.Qty_C10 <- glm(Qty_C ~ EVI * GDD * NDMI, data = ACRU)
ACRU.Qty_C11 <- glm(Qty_C ~ EVI * GDD * Site, data = ACRU)
ACRU.Qty_C12 <- glm(Qty_C ~ Year * GDD * NDMI, data = ACRU)
ACRU.Qty_C13 <- glm(Qty_C ~ Year * Site * GDD, data = ACRU)
ACRU.Qty_C14 <- glm(Qty_C ~ Year * NDMI * Site, data = ACRU)
ACRU.Qty_C15 <- glm(Qty_C ~ GDD * NDMI * Site, data = ACRU)
ACRU.Qty_C16 <- glm(Qty_C ~ Year * Site, data = ACRU)
ACRU.Qty_C17 <- glm(Qty_C ~ Year * EVI, data = ACRU)
ACRU.Qty_C18 <- glm(Qty_C ~ Year * NDMI, data = ACRU)
ACRU.Qty_C19 <- glm(Qty_C ~ Year * GDD, data = ACRU)
ACRU.Qty_C20 <- glm(Qty_C ~ EVI * Site, data = ACRU)
ACRU.Qty_C21 <- glm(Qty_C ~ NDMI * Site, data = ACRU)
ACRU.Qty_C22 <- glm(Qty_C ~ GDD * Site, data = ACRU)
ACRU.Qty_C23 <- glm(Qty_C ~ EVI * NDMI, data = ACRU)
ACRU.Qty_C24 <- glm(Qty_C ~ NDMI * GDD, data = ACRU)
ACRU.Qty_C25 <- glm(Qty_C ~ GDD * EVI, data = ACRU)
ACRU.Qty_C26 <- glm(Qty_C ~ Site, data = ACRU)
ACRU.Qty_C27 <- glm(Qty_C ~ NDMI, data = ACRU)
ACRU.Qty_C28 <- glm(Qty_C ~ GDD, data = ACRU)
ACRU.Qty_C29 <- glm(Qty_C ~ EVI, data = ACRU) 
ACRU.Qty_C30 <- glm(Qty_C ~ Year, data = ACRU) 
ACRU.Qty_C31 <- glm(Qty_C ~ 1, data =  ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Qty_Cmodels <- list(ACRU.Qty_C1, ACRU.Qty_C2, ACRU.Qty_C3, ACRU.Qty_C4, ACRU.Qty_C5, ACRU.Qty_C6, ACRU.Qty_C7, ACRU.Qty_C8, ACRU.Qty_C9, ACRU.Qty_C10, ACRU.Qty_C11, ACRU.Qty_C12, ACRU.Qty_C13, ACRU.Qty_C14, ACRU.Qty_C15, ACRU.Qty_C16, ACRU.Qty_C17, ACRU.Qty_C18, ACRU.Qty_C19, ACRU.Qty_C20, ACRU.Qty_C21, ACRU.Qty_C22, ACRU.Qty_C23, ACRU.Qty_C24, ACRU.Qty_C25, ACRU.Qty_C26, ACRU.Qty_C27, ACRU.Qty_C28, ACRU.Qty_C29, ACRU.Qty_C30, ACRU.Qty_C31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.Qty_C.residplots <- imap(ACRU.Qty_Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ACRU_QtyC_glm.pdf")
ACRU.Qty_C.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.Qty_C <- list("ACRU.Qty_C1" = ACRU.Qty_C1, "ACRU.Qty_C2" = ACRU.Qty_C2, "ACRU.Qty_C3" = ACRU.Qty_C3, "ACRU.Qty_C4" = ACRU.Qty_C4, "ACRU.Qty_C5" = ACRU.Qty_C5, "ACRU.Qty_C6" = ACRU.Qty_C6, "ACRU.Qty_C7" = ACRU.Qty_C7, "ACRU.Qty_C8" =  ACRU.Qty_C8, "ACRU.Qty_C9" = ACRU.Qty_C9, "ACRU.Qty_C10" = ACRU.Qty_C10, "ACRU.Qty_C11" = ACRU.Qty_C11, "ACRU.Qty_C12" = ACRU.Qty_C12, "ACRU.Qty_C13" = ACRU.Qty_C13, "ACRU.Qty_C14" = ACRU.Qty_C14, "ACRU.Qty_C15" = ACRU.Qty_C15, "ACRU.Qty_C16" = ACRU.Qty_C16, "ACRU.Qty_C17" = ACRU.Qty_C17, "ACRU.Qty_C18" = ACRU.Qty_C18, "ACRU.Qty_C19" = ACRU.Qty_C19, "ACRU.Qty_C20" = ACRU.Qty_C20, "ACRU.Qty_C21" = ACRU.Qty_C21, "ACRU.Qty_C22" = ACRU.Qty_C22, "ACRU.Qty_C23" = ACRU.Qty_C23, "ACRU.Qty_C24" =  ACRU.Qty_C24, "ACRU.Qty_C25" = ACRU.Qty_C25, "ACRU.Qty_C26" = ACRU.Qty_C26, "ACRU.Qty_C27" = ACRU.Qty_C27, "ACRU.Qty_C28" = ACRU.Qty_C28, "ACRU.Qty_C29" = ACRU.Qty_C29, "ACRU.Qty_C30" = ACRU.Qty_C30, "ACRU.Qty_C31" = ACRU.Qty_C31)
ACRU.Qty_C <- aictab(cand.set = Models.ACRU.Qty_C)
print(ACRU.Qty_C)
write.csv(ACRU.Qty_C, "output/AIC/ACRU_QtyC.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ACRU.Qty_C31)
summary(ACRU.Qty_C28)
summary(ACRU.Qty_C29)
summary(ACRU.Qty_C27)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.Qty_C31, which = "Nagelkerke")
PseudoR2(ACRU.Qty_C28, which = "Nagelkerke")
PseudoR2(ACRU.Qty_C29, which = "Nagelkerke")
PseudoR2(ACRU.Qty_C27, which = "Nagelkerke")
#Qty_P
ACRU.Qty_P1 <- glm(Qty_P ~ Year * EVI * GDD * NDMI * Site, data = ACRU)
ACRU.Qty_P2 <- glm(Qty_P ~ Year * EVI * GDD * NDMI, data = ACRU)
ACRU.Qty_P3 <- glm(Qty_P ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.Qty_P4 <- glm(Qty_P ~ Year * EVI * Site * NDMI, data = ACRU)
ACRU.Qty_P5 <- glm(Qty_P ~ Year * Site * GDD * NDMI, data = ACRU) 
ACRU.Qty_P6 <- glm(Qty_P ~ Site * EVI * GDD * NDMI, data = ACRU) 
ACRU.Qty_P7 <- glm(Qty_P ~ Year * EVI * GDD, data = ACRU) 
ACRU.Qty_P8 <- glm(Qty_P ~ Year * EVI * NDMI, data = ACRU)
ACRU.Qty_P9 <- glm(Qty_P ~ Year * EVI * Site, data = ACRU)
ACRU.Qty_P10 <- glm(Qty_P ~ EVI * GDD * NDMI, data = ACRU)
ACRU.Qty_P11 <- glm(Qty_P ~ EVI * GDD * Site, data = ACRU)
ACRU.Qty_P12 <- glm(Qty_P ~ Year * GDD * NDMI, data = ACRU)
ACRU.Qty_P13 <- glm(Qty_P ~ Year * Site * GDD, data = ACRU)
ACRU.Qty_P14 <- glm(Qty_P ~ Year * NDMI * Site, data = ACRU)
ACRU.Qty_P15 <- glm(Qty_P ~ GDD * NDMI * Site, data = ACRU)
ACRU.Qty_P16 <- glm(Qty_P ~ Year * Site, data = ACRU)
ACRU.Qty_P17 <- glm(Qty_P ~ Year * EVI, data = ACRU)
ACRU.Qty_P18 <- glm(Qty_P ~ Year * NDMI, data = ACRU)
ACRU.Qty_P19 <- glm(Qty_P ~ Year * GDD, data = ACRU)
ACRU.Qty_P20 <- glm(Qty_P ~ EVI * Site, data = ACRU)
ACRU.Qty_P21 <- glm(Qty_P ~ NDMI * Site, data = ACRU)
ACRU.Qty_P22 <- glm(Qty_P ~ GDD * Site, data = ACRU)
ACRU.Qty_P23 <- glm(Qty_P ~ EVI * NDMI, data = ACRU)
ACRU.Qty_P24 <- glm(Qty_P ~ NDMI * GDD, data = ACRU)
ACRU.Qty_P25 <- glm(Qty_P ~ GDD * EVI, data = ACRU)
ACRU.Qty_P26 <- glm(Qty_P ~ Site, data = ACRU)
ACRU.Qty_P27 <- glm(Qty_P ~ NDMI, data = ACRU)
ACRU.Qty_P28 <- glm(Qty_P ~ GDD, data = ACRU)
ACRU.Qty_P29 <- glm(Qty_P ~ EVI, data = ACRU) 
ACRU.Qty_P30 <- glm(Qty_P ~ Year, data = ACRU) 
ACRU.Qty_P31 <- glm(Qty_P ~ 1, data =  ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Qty_Pmodels <- list(ACRU.Qty_P1, ACRU.Qty_P2, ACRU.Qty_P3, ACRU.Qty_P4, ACRU.Qty_P5, ACRU.Qty_P6, ACRU.Qty_P7, ACRU.Qty_P8, ACRU.Qty_P9, ACRU.Qty_P10, ACRU.Qty_P11, ACRU.Qty_P12, ACRU.Qty_P13, ACRU.Qty_P14, ACRU.Qty_P15, ACRU.Qty_P16, ACRU.Qty_P17, ACRU.Qty_P18, ACRU.Qty_P19, ACRU.Qty_P20, ACRU.Qty_P21, ACRU.Qty_P22, ACRU.Qty_P23, ACRU.Qty_P24, ACRU.Qty_P25, ACRU.Qty_P26, ACRU.Qty_P27, ACRU.Qty_P28, ACRU.Qty_P29, ACRU.Qty_P30, ACRU.Qty_P31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.Qty_P.residplots <- imap(ACRU.Qty_Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ACRU_QtyP_glm.pdf")
ACRU.Qty_P.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.Qty_P <- list("ACRU.Qty_P1" = ACRU.Qty_P1, "ACRU.Qty_P2" = ACRU.Qty_P2, "ACRU.Qty_P3" = ACRU.Qty_P3, "ACRU.Qty_P4" = ACRU.Qty_P4, "ACRU.Qty_P5" = ACRU.Qty_P5, "ACRU.Qty_P6" = ACRU.Qty_P6, "ACRU.Qty_P7" = ACRU.Qty_P7, "ACRU.Qty_P8" =  ACRU.Qty_P8, "ACRU.Qty_P9" = ACRU.Qty_P9, "ACRU.Qty_P10" = ACRU.Qty_P10, "ACRU.Qty_P11" = ACRU.Qty_P11, "ACRU.Qty_P12" = ACRU.Qty_P12, "ACRU.Qty_P13" = ACRU.Qty_P13, "ACRU.Qty_P14" = ACRU.Qty_P14, "ACRU.Qty_P15" = ACRU.Qty_P15, "ACRU.Qty_P16" = ACRU.Qty_P16, "ACRU.Qty_P17" = ACRU.Qty_P17, "ACRU.Qty_P18" = ACRU.Qty_P18, "ACRU.Qty_P19" = ACRU.Qty_P19, "ACRU.Qty_P20" = ACRU.Qty_P20, "ACRU.Qty_P21" = ACRU.Qty_P21, "ACRU.Qty_P22" = ACRU.Qty_P22, "ACRU.Qty_P23" = ACRU.Qty_P23, "ACRU.Qty_P24" =  ACRU.Qty_P24, "ACRU.Qty_P25" = ACRU.Qty_P25, "ACRU.Qty_P26" = ACRU.Qty_P26, "ACRU.Qty_P27" = ACRU.Qty_P27, "ACRU.Qty_P28" = ACRU.Qty_P28, "ACRU.Qty_P29" = ACRU.Qty_P29, "ACRU.Qty_P30" = ACRU.Qty_P30, "ACRU.Qty_P31" = ACRU.Qty_P31)
ACRU.Qty_P <- aictab(cand.set = Models.ACRU.Qty_P)
print(ACRU.Qty_P)
write.csv(ACRU.Qty_P, "output/AIC/ACRU_QtyP.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ACRU.Qty_P31)
summary(ACRU.Qty_P28)
summary(ACRU.Qty_P29)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.Qty_P31, which = "Nagelkerke")
PseudoR2(ACRU.Qty_P28, which = "Nagelkerke")
PseudoR2(ACRU.Qty_P29, which = "Nagelkerke")
#Qty_N
ACRU.Qty_N1 <- glm(Qty_N ~ Year * EVI * GDD * NDMI * Site, data = ACRU)
ACRU.Qty_N2 <- glm(Qty_N ~ Year * EVI * GDD * NDMI, data = ACRU)
ACRU.Qty_N3 <- glm(Qty_N ~ Year * EVI * GDD * Site, data = ACRU)
ACRU.Qty_N4 <- glm(Qty_N ~ Year * EVI * Site * NDMI, data = ACRU)
ACRU.Qty_N5 <- glm(Qty_N ~ Year * Site * GDD * NDMI, data = ACRU) 
ACRU.Qty_N6 <- glm(Qty_N ~ Site * EVI * GDD * NDMI, data = ACRU) 
ACRU.Qty_N7 <- glm(Qty_N ~ Year * EVI * GDD, data = ACRU) 
ACRU.Qty_N8 <- glm(Qty_N ~ Year * EVI * NDMI, data = ACRU)
ACRU.Qty_N9 <- glm(Qty_N ~ Year * EVI * Site, data = ACRU)
ACRU.Qty_N10 <- glm(Qty_N ~ EVI * GDD * NDMI, data = ACRU)
ACRU.Qty_N11 <- glm(Qty_N ~ EVI * GDD * Site, data = ACRU)
ACRU.Qty_N12 <- glm(Qty_N ~ Year * GDD * NDMI, data = ACRU)
ACRU.Qty_N13 <- glm(Qty_N ~ Year * Site * GDD, data = ACRU)
ACRU.Qty_N14 <- glm(Qty_N ~ Year * NDMI * Site, data = ACRU)
ACRU.Qty_N15 <- glm(Qty_N ~ GDD * NDMI * Site, data = ACRU)
ACRU.Qty_N16 <- glm(Qty_N ~ Year * Site, data = ACRU)
ACRU.Qty_N17 <- glm(Qty_N ~ Year * EVI, data = ACRU)
ACRU.Qty_N18 <- glm(Qty_N ~ Year * NDMI, data = ACRU)
ACRU.Qty_N19 <- glm(Qty_N ~ Year * GDD, data = ACRU)
ACRU.Qty_N20 <- glm(Qty_N ~ EVI * Site, data = ACRU)
ACRU.Qty_N21 <- glm(Qty_N ~ NDMI * Site, data = ACRU)
ACRU.Qty_N22 <- glm(Qty_N ~ GDD * Site, data = ACRU)
ACRU.Qty_N23 <- glm(Qty_N ~ EVI * NDMI, data = ACRU)
ACRU.Qty_N24 <- glm(Qty_N ~ NDMI * GDD, data = ACRU)
ACRU.Qty_N25 <- glm(Qty_N ~ GDD * EVI, data = ACRU)
ACRU.Qty_N26 <- glm(Qty_N ~ Site, data = ACRU)
ACRU.Qty_N27 <- glm(Qty_N ~ NDMI, data = ACRU)
ACRU.Qty_N28 <- glm(Qty_N ~ GDD, data = ACRU)
ACRU.Qty_N29 <- glm(Qty_N ~ EVI, data = ACRU) 
ACRU.Qty_N30 <- glm(Qty_N ~ Year, data = ACRU) 
ACRU.Qty_N31 <- glm(Qty_N ~ 1, data =  ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Qty_Nmodels <- list(ACRU.Qty_N1, ACRU.Qty_N2, ACRU.Qty_N3, ACRU.Qty_N4, ACRU.Qty_N5, ACRU.Qty_N6, ACRU.Qty_N7, ACRU.Qty_N8, ACRU.Qty_N9, ACRU.Qty_N10, ACRU.Qty_N11, ACRU.Qty_N12, ACRU.Qty_N13, ACRU.Qty_N14, ACRU.Qty_N15, ACRU.Qty_N16, ACRU.Qty_N17, ACRU.Qty_N18, ACRU.Qty_N19, ACRU.Qty_N20, ACRU.Qty_N21, ACRU.Qty_N22, ACRU.Qty_N23, ACRU.Qty_N24, ACRU.Qty_N25, ACRU.Qty_N26, ACRU.Qty_N27, ACRU.Qty_N28, ACRU.Qty_N29, ACRU.Qty_N30, ACRU.Qty_N31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.Qty_N.residplots <- imap(ACRU.Qty_Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/ACRU_QtyN_glm.pdf")
ACRU.Qty_N.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.Qty_N <- list("ACRU.Qty_N1" = ACRU.Qty_N1, "ACRU.Qty_N2" = ACRU.Qty_N2, "ACRU.Qty_N3" = ACRU.Qty_N3, "ACRU.Qty_N4" = ACRU.Qty_N4, "ACRU.Qty_N5" = ACRU.Qty_N5, "ACRU.Qty_N6" = ACRU.Qty_N6, "ACRU.Qty_N7" = ACRU.Qty_N7, "ACRU.Qty_N8" =  ACRU.Qty_N8, "ACRU.Qty_N9" = ACRU.Qty_N9, "ACRU.Qty_N10" = ACRU.Qty_N10, "ACRU.Qty_N11" = ACRU.Qty_N11, "ACRU.Qty_N12" = ACRU.Qty_N12, "ACRU.Qty_N13" = ACRU.Qty_N13, "ACRU.Qty_N14" = ACRU.Qty_N14, "ACRU.Qty_N15" = ACRU.Qty_N15, "ACRU.Qty_N16" = ACRU.Qty_N16, "ACRU.Qty_N17" = ACRU.Qty_N17, "ACRU.Qty_N18" = ACRU.Qty_N18, "ACRU.Qty_N19" = ACRU.Qty_N19, "ACRU.Qty_N20" = ACRU.Qty_N20, "ACRU.Qty_N21" = ACRU.Qty_N21, "ACRU.Qty_N22" = ACRU.Qty_N22, "ACRU.Qty_N23" = ACRU.Qty_N23, "ACRU.Qty_N24" =  ACRU.Qty_N24, "ACRU.Qty_N25" = ACRU.Qty_N25, "ACRU.Qty_N26" = ACRU.Qty_N26, "ACRU.Qty_N27" = ACRU.Qty_N27, "ACRU.Qty_N28" = ACRU.Qty_N28, "ACRU.Qty_N29" = ACRU.Qty_N29, "ACRU.Qty_N30" = ACRU.Qty_N30, "ACRU.Qty_N31" = ACRU.Qty_N31)
ACRU.Qty_N <- aictab(cand.set = Models.ACRU.Qty_N)
print(ACRU.Qty_N)
write.csv(ACRU.Qty_N, "output/AIC/ACRU_QtyN.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(ACRU.Qty_N31)
summary(ACRU.Qty_N28)
summary(ACRU.Qty_N29)
summary(ACRU.Qty_N27)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.Qty_N31, which = "Nagelkerke")
PseudoR2(ACRU.Qty_N28, which = "Nagelkerke")
PseudoR2(ACRU.Qty_N29, which = "Nagelkerke")
PseudoR2(ACRU.Qty_N27, which = "Nagelkerke")

# BEPA
# %C
BEPA.C1 <- glm(C ~ Year * EVI * GDD * NDMI * Site, data = BEPA)
BEPA.C2 <- glm(C ~ Year * EVI * GDD * NDMI, data = BEPA)
BEPA.C3 <- glm(C ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.C4 <- glm(C ~ Year * EVI * Site * NDMI, data = BEPA)
BEPA.C5 <- glm(C ~ Year * Site * GDD * NDMI, data = BEPA) 
BEPA.C6 <- glm(C ~ Site * EVI * GDD * NDMI, data = BEPA) 
BEPA.C7 <- glm(C ~ Year * EVI * GDD, data = BEPA) 
BEPA.C8 <- glm(C ~ Year * EVI * NDMI, data = BEPA)
BEPA.C9 <- glm(C ~ Year * EVI * Site, data = BEPA)
BEPA.C10 <- glm(C ~ EVI * GDD * NDMI, data = BEPA)
BEPA.C11 <- glm(C ~ EVI * GDD * Site, data = BEPA)
BEPA.C12 <- glm(C ~ Year * GDD * NDMI, data = BEPA)
BEPA.C13 <- glm(C ~ Year * Site * GDD, data = BEPA)
BEPA.C14 <- glm(C ~ Year * NDMI * Site, data = BEPA)
BEPA.C15 <- glm(C ~ GDD * NDMI * Site, data = BEPA)
BEPA.C16 <- glm(C ~ Year * Site, data = BEPA)
BEPA.C17 <- glm(C ~ Year * EVI, data = BEPA)
BEPA.C18 <- glm(C ~ Year * NDMI, data = BEPA)
BEPA.C19 <- glm(C ~ Year * GDD, data = BEPA)
BEPA.C20 <- glm(C ~ EVI * Site, data = BEPA)
BEPA.C21 <- glm(C ~ NDMI * Site, data = BEPA)
BEPA.C22 <- glm(C ~ GDD * Site, data = BEPA)
BEPA.C23 <- glm(C ~ EVI * NDMI, data = BEPA)
BEPA.C24 <- glm(C ~ NDMI * GDD, data = BEPA)
BEPA.C25 <- glm(C ~ GDD * EVI, data = BEPA)
BEPA.C26 <- glm(C ~ Site, data = BEPA)
BEPA.C27 <- glm(C ~ NDMI, data = BEPA)
BEPA.C28 <- glm(C ~ GDD, data = BEPA)
BEPA.C29 <- glm(C ~ EVI, data = BEPA) 
BEPA.C30 <- glm(C ~ Year, data = BEPA) 
BEPA.C31 <- glm(C ~ 1, data =  BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Cmodels <- list(BEPA.C1, BEPA.C2, BEPA.C3, BEPA.C4, BEPA.C5, BEPA.C6, BEPA.C7, BEPA.C8, BEPA.C9, BEPA.C10, BEPA.C11, BEPA.C12, BEPA.C13, BEPA.C14, BEPA.C15, BEPA.C16, BEPA.C17, BEPA.C18, BEPA.C19, BEPA.C20, BEPA.C21, BEPA.C22, BEPA.C23, BEPA.C24, BEPA.C25, BEPA.C26, BEPA.C27, BEPA.C28, BEPA.C29, BEPA.C30, BEPA.C31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.C.residplots <- imap(BEPA.Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/BEPA_C_glm.pdf")
BEPA.C.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.C <- list("BEPA.C1" = BEPA.C1, "BEPA.C2" = BEPA.C2, "BEPA.C3" = BEPA.C3, "BEPA.C4" = BEPA.C4, "BEPA.C5" = BEPA.C5, "BEPA.C6" = BEPA.C6, "BEPA.C7" = BEPA.C7, "BEPA.C8" =  BEPA.C8, "BEPA.C9" = BEPA.C9, "BEPA.C10" = BEPA.C10, "BEPA.C11" = BEPA.C11, "BEPA.C12" = BEPA.C12, "BEPA.C13" = BEPA.C13, "BEPA.C14" = BEPA.C14, "BEPA.C15" = BEPA.C15, "BEPA.C16" = BEPA.C16, "BEPA.C17" = BEPA.C17, "BEPA.C18" = BEPA.C18, "BEPA.C19" = BEPA.C19, "BEPA.C20" = BEPA.C20, "BEPA.C21" = BEPA.C21, "BEPA.C22" = BEPA.C22, "BEPA.C23" = BEPA.C23, "BEPA.C24" =  BEPA.C24, "BEPA.C25" = BEPA.C25, "BEPA.C26" = BEPA.C26, "BEPA.C27" = BEPA.C27, "BEPA.C28" = BEPA.C28, "BEPA.C29" = BEPA.C29, "BEPA.C30" = BEPA.C30, "BEPA.C31" = BEPA.C31)
BEPA.C <- aictab(cand.set = Models.BEPA.C)
print(BEPA.C)
write.csv(BEPA.C, "output/AIC/BEPA_C.csv")
# summary of the most parsimonious models - delta AICc of less than 2
summary(BEPA.C12)
summary(BEPA.C13)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.C12, which = "Nagelkerke")
PseudoR2(BEPA.C13, which = "Nagelkerke")
#%P
BEPA.P1 <- glm(P ~ Year * EVI * GDD * NDMI * Site, data = BEPA)
BEPA.P2 <- glm(P ~ Year * EVI * GDD * NDMI, data = BEPA)
BEPA.P3 <- glm(P ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.P4 <- glm(P ~ Year * EVI * Site * NDMI, data = BEPA)
BEPA.P5 <- glm(P ~ Year * Site * GDD * NDMI, data = BEPA) 
BEPA.P6 <- glm(P ~ Site * EVI * GDD * NDMI, data = BEPA) 
BEPA.P7 <- glm(P ~ Year * EVI * GDD, data = BEPA) 
BEPA.P8 <- glm(P ~ Year * EVI * NDMI, data = BEPA)
BEPA.P9 <- glm(P ~ Year * EVI * Site, data = BEPA)
BEPA.P10 <- glm(P ~ EVI * GDD * NDMI, data = BEPA)
BEPA.P11 <- glm(P ~ EVI * GDD * Site, data = BEPA)
BEPA.P12 <- glm(P ~ Year * GDD * NDMI, data = BEPA)
BEPA.P13 <- glm(P ~ Year * Site * GDD, data = BEPA)
BEPA.P14 <- glm(P ~ Year * NDMI * Site, data = BEPA)
BEPA.P15 <- glm(P ~ GDD * NDMI * Site, data = BEPA)
BEPA.P16 <- glm(P ~ Year * Site, data = BEPA)
BEPA.P17 <- glm(P ~ Year * EVI, data = BEPA)
BEPA.P18 <- glm(P ~ Year * NDMI, data = BEPA)
BEPA.P19 <- glm(P ~ Year * GDD, data = BEPA)
BEPA.P20 <- glm(P ~ EVI * Site, data = BEPA)
BEPA.P21 <- glm(P ~ NDMI * Site, data = BEPA)
BEPA.P22 <- glm(P ~ GDD * Site, data = BEPA)
BEPA.P23 <- glm(P ~ EVI * NDMI, data = BEPA)
BEPA.P24 <- glm(P ~ NDMI * GDD, data = BEPA)
BEPA.P25 <- glm(P ~ GDD * EVI, data = BEPA)
BEPA.P26 <- glm(P ~ Site, data = BEPA)
BEPA.P27 <- glm(P ~ NDMI, data = BEPA)
BEPA.P28 <- glm(P ~ GDD, data = BEPA)
BEPA.P29 <- glm(P ~ EVI, data = BEPA) 
BEPA.P30 <- glm(P ~ Year, data = BEPA) 
BEPA.P31 <- glm(P ~ 1, data =  BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Pmodels <- list(BEPA.P1, BEPA.P2, BEPA.P3, BEPA.P4, BEPA.P5, BEPA.P6, BEPA.P7, BEPA.P8, BEPA.P9, BEPA.P10, BEPA.P11, BEPA.P12, BEPA.P13, BEPA.P14, BEPA.P15, BEPA.P16, BEPA.P17, BEPA.P18, BEPA.P19, BEPA.P20, BEPA.P21, BEPA.P22, BEPA.P23, BEPA.P24, BEPA.P25, BEPA.P26, BEPA.P27, BEPA.P28, BEPA.P29, BEPA.P30, BEPA.P31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.P.residplots <- imap(BEPA.Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/BEPA_P_glm.pdf")
BEPA.P.residplots
dev.off()


# create an AIPc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.P <- list("BEPA.P1" = BEPA.P1, "BEPA.P2" = BEPA.P2, "BEPA.P3" = BEPA.P3, "BEPA.P4" = BEPA.P4, "BEPA.P5" = BEPA.P5, "BEPA.P6" = BEPA.P6, "BEPA.P7" = BEPA.P7, "BEPA.P8" =  BEPA.P8, "BEPA.P9" = BEPA.P9, "BEPA.P10" = BEPA.P10, "BEPA.P11" = BEPA.P11, "BEPA.P12" = BEPA.P12, "BEPA.P13" = BEPA.P13, "BEPA.P14" = BEPA.P14, "BEPA.P15" = BEPA.P15, "BEPA.P16" = BEPA.P16, "BEPA.P17" = BEPA.P17, "BEPA.P18" = BEPA.P18, "BEPA.P19" = BEPA.P19, "BEPA.P20" = BEPA.P20, "BEPA.P21" = BEPA.P21, "BEPA.P22" = BEPA.P22, "BEPA.P23" = BEPA.P23, "BEPA.P24" =  BEPA.P24, "BEPA.P25" = BEPA.P25, "BEPA.P26" = BEPA.P26, "BEPA.P27" = BEPA.P27, "BEPA.P28" = BEPA.P28, "BEPA.P29" = BEPA.P29, "BEPA.P30" = BEPA.P30, "BEPA.P31" = BEPA.P31)
BEPA.P <- aictab(cand.set = Models.BEPA.P)
print(BEPA.P)
write.csv(BEPA.P, "output/AIC/BEPA_P.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(BEPA.P24)
summary(BEPA.P28)
summary(BEPA.P21)
summary(BEPA.P26)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.P24, which = "Nagelkerke")
PseudoR2(BEPA.P28, which = "Nagelkerke")
PseudoR2(BEPA.P21, which = "Nagelkerke")
PseudoR2(BEPA.P26, which = "Nagelkerke")
#%N
BEPA.N1 <- glm(N ~ Year * EVI * GDD * NDMI * Site, data = BEPA)
BEPA.N2 <- glm(N ~ Year * EVI * GDD * NDMI, data = BEPA)
BEPA.N3 <- glm(N ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.N4 <- glm(N ~ Year * EVI * Site * NDMI, data = BEPA)
BEPA.N5 <- glm(N ~ Year * Site * GDD * NDMI, data = BEPA) 
BEPA.N6 <- glm(N ~ Site * EVI * GDD * NDMI, data = BEPA) 
BEPA.N7 <- glm(N ~ Year * EVI * GDD, data = BEPA) 
BEPA.N8 <- glm(N ~ Year * EVI * NDMI, data = BEPA)
BEPA.N9 <- glm(N ~ Year * EVI * Site, data = BEPA)
BEPA.N10 <- glm(N ~ EVI * GDD * NDMI, data = BEPA)
BEPA.N11 <- glm(N ~ EVI * GDD * Site, data = BEPA)
BEPA.N12 <- glm(N ~ Year * GDD * NDMI, data = BEPA)
BEPA.N13 <- glm(N ~ Year * Site * GDD, data = BEPA)
BEPA.N14 <- glm(N ~ Year * NDMI * Site, data = BEPA)
BEPA.N15 <- glm(N ~ GDD * NDMI * Site, data = BEPA)
BEPA.N16 <- glm(N ~ Year * Site, data = BEPA)
BEPA.N17 <- glm(N ~ Year * EVI, data = BEPA)
BEPA.N18 <- glm(N ~ Year * NDMI, data = BEPA)
BEPA.N19 <- glm(N ~ Year * GDD, data = BEPA)
BEPA.N20 <- glm(N ~ EVI * Site, data = BEPA)
BEPA.N21 <- glm(N ~ NDMI * Site, data = BEPA)
BEPA.N22 <- glm(N ~ GDD * Site, data = BEPA)
BEPA.N23 <- glm(N ~ EVI * NDMI, data = BEPA)
BEPA.N24 <- glm(N ~ NDMI * GDD, data = BEPA)
BEPA.N25 <- glm(N ~ GDD * EVI, data = BEPA)
BEPA.N26 <- glm(N ~ Site, data = BEPA)
BEPA.N27 <- glm(N ~ NDMI, data = BEPA)
BEPA.N28 <- glm(N ~ GDD, data = BEPA)
BEPA.N29 <- glm(N ~ EVI, data = BEPA) 
BEPA.N30 <- glm(N ~ Year, data = BEPA) 
BEPA.N31 <- glm(N ~ 1, data =  BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Nmodels <- list(BEPA.N1, BEPA.N2, BEPA.N3, BEPA.N4, BEPA.N5, BEPA.N6, BEPA.N7, BEPA.N8, BEPA.N9, BEPA.N10, BEPA.N11, BEPA.N12, BEPA.N13, BEPA.N14, BEPA.N15, BEPA.N16, BEPA.N17, BEPA.N18, BEPA.N19, BEPA.N20, BEPA.N21, BEPA.N22, BEPA.N23, BEPA.N24, BEPA.N25, BEPA.N26, BEPA.N27, BEPA.N28, BEPA.N29, BEPA.N30, BEPA.N31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.N.residplots <- imap(BEPA.Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/BEPA_N_glm.pdf")
BEPA.N.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.N <- list("BEPA.N1" = BEPA.N1, "BEPA.N2" = BEPA.N2, "BEPA.N3" = BEPA.N3, "BEPA.N4" = BEPA.N4, "BEPA.N5" = BEPA.N5, "BEPA.N6" = BEPA.N6, "BEPA.N7" = BEPA.N7, "BEPA.N8" =  BEPA.N8, "BEPA.N9" = BEPA.N9, "BEPA.N10" = BEPA.N10, "BEPA.N11" = BEPA.N11, "BEPA.N12" = BEPA.N12, "BEPA.N13" = BEPA.N13, "BEPA.N14" = BEPA.N14, "BEPA.N15" = BEPA.N15, "BEPA.N16" = BEPA.N16, "BEPA.N17" = BEPA.N17, "BEPA.N18" = BEPA.N18, "BEPA.N19" = BEPA.N19, "BEPA.N20" = BEPA.N20, "BEPA.N21" = BEPA.N21, "BEPA.N22" = BEPA.N22, "BEPA.N23" = BEPA.N23, "BEPA.N24" =  BEPA.N24, "BEPA.N25" = BEPA.N25, "BEPA.N26" = BEPA.N26, "BEPA.N27" = BEPA.N27, "BEPA.N28" = BEPA.N28, "BEPA.N29" = BEPA.N29, "BEPA.N30" = BEPA.N30, "BEPA.N31" = BEPA.N31)
BEPA.N <- aictab(cand.set = Models.BEPA.N)
print(BEPA.N)
write.csv(BEPA.N, "output/AIC/BEPA_N.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(BEPA.N24)
summary(BEPA.N10)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.N24, which = "Nagelkerke")
PseudoR2(BEPA.N10, which = "Nagelkerke")
#C:N
BEPA.CN1 <- glm(CNRatio ~ Year * EVI * GDD * NDMI * Site, data = BEPA)
BEPA.CN2 <- glm(CNRatio ~ Year * EVI * GDD * NDMI, data = BEPA)
BEPA.CN3 <- glm(CNRatio ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.CN4 <- glm(CNRatio ~ Year * EVI * Site * NDMI, data = BEPA)
BEPA.CN5 <- glm(CNRatio ~ Year * Site * GDD * NDMI, data = BEPA) 
BEPA.CN6 <- glm(CNRatio ~ Site * EVI * GDD * NDMI, data = BEPA) 
BEPA.CN7 <- glm(CNRatio ~ Year * EVI * GDD, data = BEPA) 
BEPA.CN8 <- glm(CNRatio ~ Year * EVI * NDMI, data = BEPA)
BEPA.CN9 <- glm(CNRatio ~ Year * EVI * Site, data = BEPA)
BEPA.CN10 <- glm(CNRatio ~ EVI * GDD * NDMI, data = BEPA)
BEPA.CN11 <- glm(CNRatio ~ EVI * GDD * Site, data = BEPA)
BEPA.CN12 <- glm(CNRatio ~ Year * GDD * NDMI, data = BEPA)
BEPA.CN13 <- glm(CNRatio ~ Year * Site * GDD, data = BEPA)
BEPA.CN14 <- glm(CNRatio ~ Year * NDMI * Site, data = BEPA)
BEPA.CN15 <- glm(CNRatio ~ GDD * NDMI * Site, data = BEPA)
BEPA.CN16 <- glm(CNRatio ~ Year * Site, data = BEPA)
BEPA.CN17 <- glm(CNRatio ~ Year * EVI, data = BEPA)
BEPA.CN18 <- glm(CNRatio ~ Year * NDMI, data = BEPA)
BEPA.CN19 <- glm(CNRatio ~ Year * GDD, data = BEPA)
BEPA.CN20 <- glm(CNRatio ~ EVI * Site, data = BEPA)
BEPA.CN21 <- glm(CNRatio ~ NDMI * Site, data = BEPA)
BEPA.CN22 <- glm(CNRatio ~ GDD * Site, data = BEPA)
BEPA.CN23 <- glm(CNRatio ~ EVI * NDMI, data = BEPA)
BEPA.CN24 <- glm(CNRatio ~ NDMI * GDD, data = BEPA)
BEPA.CN25 <- glm(CNRatio ~ GDD * EVI, data = BEPA)
BEPA.CN26 <- glm(CNRatio ~ Site, data = BEPA)
BEPA.CN27 <- glm(CNRatio ~ NDMI, data = BEPA)
BEPA.CN28 <- glm(CNRatio ~ GDD, data = BEPA)
BEPA.CN29 <- glm(CNRatio ~ EVI, data = BEPA) 
BEPA.CN30 <- glm(CNRatio ~ Year, data = BEPA) 
BEPA.CN31 <- glm(CNRatio ~ 1, data =  BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.CNmodels <- list(BEPA.CN1, BEPA.CN2, BEPA.CN3, BEPA.CN4, BEPA.CN5, BEPA.CN6, BEPA.CN7, BEPA.CN8, BEPA.CN9, BEPA.CN10, BEPA.CN11, BEPA.CN12, BEPA.CN13, BEPA.CN14, BEPA.CN15, BEPA.CN16, BEPA.CN17, BEPA.CN18, BEPA.CN19, BEPA.CN20, BEPA.CN21, BEPA.CN22, BEPA.CN23, BEPA.CN24, BEPA.CN25, BEPA.CN26, BEPA.CN27, BEPA.CN28, BEPA.CN29, BEPA.CN30, BEPA.CN31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.CN.residplots <- imap(BEPA.CNmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/BEPA_CN_glm.pdf")
BEPA.CN.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.CN <- list("BEPA.CN1" = BEPA.CN1, "BEPA.CN2" = BEPA.CN2, "BEPA.CN3" = BEPA.CN3, "BEPA.CN4" = BEPA.CN4, "BEPA.CN5" = BEPA.CN5, "BEPA.CN6" = BEPA.CN6, "BEPA.CN7" = BEPA.CN7, "BEPA.CN8" =  BEPA.CN8, "BEPA.CN9" = BEPA.CN9, "BEPA.CN10" = BEPA.CN10, "BEPA.CN11" = BEPA.CN11, "BEPA.CN12" = BEPA.CN12, "BEPA.CN13" = BEPA.CN13, "BEPA.CN14" = BEPA.CN14, "BEPA.CN15" = BEPA.CN15, "BEPA.CN16" = BEPA.CN16, "BEPA.CN17" = BEPA.CN17, "BEPA.CN18" = BEPA.CN18, "BEPA.CN19" = BEPA.CN19, "BEPA.CN20" = BEPA.CN20, "BEPA.CN21" = BEPA.CN21, "BEPA.CN22" = BEPA.CN22, "BEPA.CN23" = BEPA.CN23, "BEPA.CN24" =  BEPA.CN24, "BEPA.CN25" = BEPA.CN25, "BEPA.CN26" = BEPA.CN26, "BEPA.CN27" = BEPA.CN27, "BEPA.CN28" = BEPA.CN28, "BEPA.CN29" = BEPA.CN29, "BEPA.CN30" = BEPA.CN30, "BEPA.CN31" = BEPA.CN31)
BEPA.CN <- aictab(cand.set = Models.BEPA.CN)
print(BEPA.CN)
write.csv(BEPA.CN, "output/AIC/BEPA_CN.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(BEPA.CN27)
summary(BEPA.CN24)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.CN27, which = "Nagelkerke")
PseudoR2(BEPA.CN24, which = "Nagelkerke")
#C:P
BEPA.CP1 <- glm(CPRatio ~ Year * EVI * GDD * NDMI * Site, data = BEPA)
BEPA.CP2 <- glm(CPRatio ~ Year * EVI * GDD * NDMI, data = BEPA)
BEPA.CP3 <- glm(CPRatio ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.CP4 <- glm(CPRatio ~ Year * EVI * Site * NDMI, data = BEPA)
BEPA.CP5 <- glm(CPRatio ~ Year * Site * GDD * NDMI, data = BEPA) 
BEPA.CP6 <- glm(CPRatio ~ Site * EVI * GDD * NDMI, data = BEPA) 
BEPA.CP7 <- glm(CPRatio ~ Year * EVI * GDD, data = BEPA) 
BEPA.CP8 <- glm(CPRatio ~ Year * EVI * NDMI, data = BEPA)
BEPA.CP9 <- glm(CPRatio ~ Year * EVI * Site, data = BEPA)
BEPA.CP10 <- glm(CPRatio ~ EVI * GDD * NDMI, data = BEPA)
BEPA.CP11 <- glm(CPRatio ~ EVI * GDD * Site, data = BEPA)
BEPA.CP12 <- glm(CPRatio ~ Year * GDD * NDMI, data = BEPA)
BEPA.CP13 <- glm(CPRatio ~ Year * Site * GDD, data = BEPA)
BEPA.CP14 <- glm(CPRatio ~ Year * NDMI * Site, data = BEPA)
BEPA.CP15 <- glm(CPRatio ~ GDD * NDMI * Site, data = BEPA)
BEPA.CP16 <- glm(CPRatio ~ Year * Site, data = BEPA)
BEPA.CP17 <- glm(CPRatio ~ Year * EVI, data = BEPA)
BEPA.CP18 <- glm(CPRatio ~ Year * NDMI, data = BEPA)
BEPA.CP19 <- glm(CPRatio ~ Year * GDD, data = BEPA)
BEPA.CP20 <- glm(CPRatio ~ EVI * Site, data = BEPA)
BEPA.CP21 <- glm(CPRatio ~ NDMI * Site, data = BEPA)
BEPA.CP22 <- glm(CPRatio ~ GDD * Site, data = BEPA)
BEPA.CP23 <- glm(CPRatio ~ EVI * NDMI, data = BEPA)
BEPA.CP24 <- glm(CPRatio ~ NDMI * GDD, data = BEPA)
BEPA.CP25 <- glm(CPRatio ~ GDD * EVI, data = BEPA)
BEPA.CP26 <- glm(CPRatio ~ Site, data = BEPA)
BEPA.CP27 <- glm(CPRatio ~ NDMI, data = BEPA)
BEPA.CP28 <- glm(CPRatio ~ GDD, data = BEPA)
BEPA.CP29 <- glm(CPRatio ~ EVI, data = BEPA) 
BEPA.CP30 <- glm(CPRatio ~ Year, data = BEPA) 
BEPA.CP31 <- glm(CPRatio ~ 1, data =  BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.CPmodels <- list(BEPA.CP1, BEPA.CP2, BEPA.CP3, BEPA.CP4, BEPA.CP5, BEPA.CP6, BEPA.CP7, BEPA.CP8, BEPA.CP9, BEPA.CP10, BEPA.CP11, BEPA.CP12, BEPA.CP13, BEPA.CP14, BEPA.CP15, BEPA.CP16, BEPA.CP17, BEPA.CP18, BEPA.CP19, BEPA.CP20, BEPA.CP21, BEPA.CP22, BEPA.CP23, BEPA.CP24, BEPA.CP25, BEPA.CP26, BEPA.CP27, BEPA.CP28, BEPA.CP29, BEPA.CP30, BEPA.CP31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.CP.residplots <- imap(BEPA.CPmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/BEPA_CP_glm.pdf")
BEPA.CP.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.CP <- list("BEPA.CP1" = BEPA.CP1, "BEPA.CP2" = BEPA.CP2, "BEPA.CP3" = BEPA.CP3, "BEPA.CP4" = BEPA.CP4, "BEPA.CP5" = BEPA.CP5, "BEPA.CP6" = BEPA.CP6, "BEPA.CP7" = BEPA.CP7, "BEPA.CP8" =  BEPA.CP8, "BEPA.CP9" = BEPA.CP9, "BEPA.CP10" = BEPA.CP10, "BEPA.CP11" = BEPA.CP11, "BEPA.CP12" = BEPA.CP12, "BEPA.CP13" = BEPA.CP13, "BEPA.CP14" = BEPA.CP14, "BEPA.CP15" = BEPA.CP15, "BEPA.CP16" = BEPA.CP16, "BEPA.CP17" = BEPA.CP17, "BEPA.CP18" = BEPA.CP18, "BEPA.CP19" = BEPA.CP19, "BEPA.CP20" = BEPA.CP20, "BEPA.CP21" = BEPA.CP21, "BEPA.CP22" = BEPA.CP22, "BEPA.CP23" = BEPA.CP23, "BEPA.CP24" =  BEPA.CP24, "BEPA.CP25" = BEPA.CP25, "BEPA.CP26" = BEPA.CP26, "BEPA.CP27" = BEPA.CP27, "BEPA.CP28" = BEPA.CP28, "BEPA.CP29" = BEPA.CP29, "BEPA.CP30" = BEPA.CP30, "BEPA.CP31" = BEPA.CP31)
BEPA.CP <- aictab(cand.set = Models.BEPA.CP)
print(BEPA.CP)
write.csv(BEPA.CP, "output/AIC/BEPA_CP.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(BEPA.CP27)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.CP27, which = "Nagelkerke")
#N:P
BEPA.NP1 <- glm(NPRatio ~ Year * EVI * GDD * NDMI * Site, data = BEPA)
BEPA.NP2 <- glm(NPRatio ~ Year * EVI * GDD * NDMI, data = BEPA)
BEPA.NP3 <- glm(NPRatio ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.NP4 <- glm(NPRatio ~ Year * EVI * Site * NDMI, data = BEPA)
BEPA.NP5 <- glm(NPRatio ~ Year * Site * GDD * NDMI, data = BEPA) 
BEPA.NP6 <- glm(NPRatio ~ Site * EVI * GDD * NDMI, data = BEPA) 
BEPA.NP7 <- glm(NPRatio ~ Year * EVI * GDD, data = BEPA) 
BEPA.NP8 <- glm(NPRatio ~ Year * EVI * NDMI, data = BEPA)
BEPA.NP9 <- glm(NPRatio ~ Year * EVI * Site, data = BEPA)
BEPA.NP10 <- glm(NPRatio ~ EVI * GDD * NDMI, data = BEPA)
BEPA.NP11 <- glm(NPRatio ~ EVI * GDD * Site, data = BEPA)
BEPA.NP12 <- glm(NPRatio ~ Year * GDD * NDMI, data = BEPA)
BEPA.NP13 <- glm(NPRatio ~ Year * Site * GDD, data = BEPA)
BEPA.NP14 <- glm(NPRatio ~ Year * NDMI * Site, data = BEPA)
BEPA.NP15 <- glm(NPRatio ~ GDD * NDMI * Site, data = BEPA)
BEPA.NP16 <- glm(NPRatio ~ Year * Site, data = BEPA)
BEPA.NP17 <- glm(NPRatio ~ Year * EVI, data = BEPA)
BEPA.NP18 <- glm(NPRatio ~ Year * NDMI, data = BEPA)
BEPA.NP19 <- glm(NPRatio ~ Year * GDD, data = BEPA)
BEPA.NP20 <- glm(NPRatio ~ EVI * Site, data = BEPA)
BEPA.NP21 <- glm(NPRatio ~ NDMI * Site, data = BEPA)
BEPA.NP22 <- glm(NPRatio ~ GDD * Site, data = BEPA)
BEPA.NP23 <- glm(NPRatio ~ EVI * NDMI, data = BEPA)
BEPA.NP24 <- glm(NPRatio ~ NDMI * GDD, data = BEPA)
BEPA.NP25 <- glm(NPRatio ~ GDD * EVI, data = BEPA)
BEPA.NP26 <- glm(NPRatio ~ Site, data = BEPA)
BEPA.NP27 <- glm(NPRatio ~ NDMI, data = BEPA)
BEPA.NP28 <- glm(NPRatio ~ GDD, data = BEPA)
BEPA.NP29 <- glm(NPRatio ~ EVI, data = BEPA) 
BEPA.NP30 <- glm(NPRatio ~ Year, data = BEPA) 
BEPA.NP31 <- glm(NPRatio ~ 1, data =  BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.NPmodels <- list(BEPA.NP1, BEPA.NP2, BEPA.NP3, BEPA.NP4, BEPA.NP5, BEPA.NP6, BEPA.NP7, BEPA.NP8, BEPA.NP9, BEPA.NP10, BEPA.NP11, BEPA.NP12, BEPA.NP13, BEPA.NP14, BEPA.NP15, BEPA.NP16, BEPA.NP17, BEPA.NP18, BEPA.NP19, BEPA.NP20, BEPA.NP21, BEPA.NP22, BEPA.NP23, BEPA.NP24, BEPA.NP25, BEPA.NP26, BEPA.NP27, BEPA.NP28, BEPA.NP29, BEPA.NP30, BEPA.NP31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.NP.residplots <- imap(BEPA.NPmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/BEPA_NP_glm.pdf")
BEPA.NP.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.NP <- list("BEPA.NP1" = BEPA.NP1, "BEPA.NP2" = BEPA.NP2, "BEPA.NP3" = BEPA.NP3, "BEPA.NP4" = BEPA.NP4, "BEPA.NP5" = BEPA.NP5, "BEPA.NP6" = BEPA.NP6, "BEPA.NP7" = BEPA.NP7, "BEPA.NP8" =  BEPA.NP8, "BEPA.NP9" = BEPA.NP9, "BEPA.NP10" = BEPA.NP10, "BEPA.NP11" = BEPA.NP11, "BEPA.NP12" = BEPA.NP12, "BEPA.NP13" = BEPA.NP13, "BEPA.NP14" = BEPA.NP14, "BEPA.NP15" = BEPA.NP15, "BEPA.NP16" = BEPA.NP16, "BEPA.NP17" = BEPA.NP17, "BEPA.NP18" = BEPA.NP18, "BEPA.NP19" = BEPA.NP19, "BEPA.NP20" = BEPA.NP20, "BEPA.NP21" = BEPA.NP21, "BEPA.NP22" = BEPA.NP22, "BEPA.NP23" = BEPA.NP23, "BEPA.NP24" =  BEPA.NP24, "BEPA.NP25" = BEPA.NP25, "BEPA.NP26" = BEPA.NP26, "BEPA.NP27" = BEPA.NP27, "BEPA.NP28" = BEPA.NP28, "BEPA.NP29" = BEPA.NP29, "BEPA.NP30" = BEPA.NP30, "BEPA.NP31" = BEPA.NP31)
BEPA.NP <- aictab(cand.set = Models.BEPA.NP)
print(BEPA.NP)
write.csv(BEPA.NP, "output/AIC/BEPA_NP.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(BEPA.NP21)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.NP21, which = "Nagelkerke")
#Qty_C
BEPA.Qty_C1 <- glm(Qty_C ~ Year * EVI * GDD * NDMI * Site, data = BEPA)
BEPA.Qty_C2 <- glm(Qty_C ~ Year * EVI * GDD * NDMI, data = BEPA)
BEPA.Qty_C3 <- glm(Qty_C ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.Qty_C4 <- glm(Qty_C ~ Year * EVI * Site * NDMI, data = BEPA)
BEPA.Qty_C5 <- glm(Qty_C ~ Year * Site * GDD * NDMI, data = BEPA) 
BEPA.Qty_C6 <- glm(Qty_C ~ Site * EVI * GDD * NDMI, data = BEPA) 
BEPA.Qty_C7 <- glm(Qty_C ~ Year * EVI * GDD, data = BEPA) 
BEPA.Qty_C8 <- glm(Qty_C ~ Year * EVI * NDMI, data = BEPA)
BEPA.Qty_C9 <- glm(Qty_C ~ Year * EVI * Site, data = BEPA)
BEPA.Qty_C10 <- glm(Qty_C ~ EVI * GDD * NDMI, data = BEPA)
BEPA.Qty_C11 <- glm(Qty_C ~ EVI * GDD * Site, data = BEPA)
BEPA.Qty_C12 <- glm(Qty_C ~ Year * GDD * NDMI, data = BEPA)
BEPA.Qty_C13 <- glm(Qty_C ~ Year * Site * GDD, data = BEPA)
BEPA.Qty_C14 <- glm(Qty_C ~ Year * NDMI * Site, data = BEPA)
BEPA.Qty_C15 <- glm(Qty_C ~ GDD * NDMI * Site, data = BEPA)
BEPA.Qty_C16 <- glm(Qty_C ~ Year * Site, data = BEPA)
BEPA.Qty_C17 <- glm(Qty_C ~ Year * EVI, data = BEPA)
BEPA.Qty_C18 <- glm(Qty_C ~ Year * NDMI, data = BEPA)
BEPA.Qty_C19 <- glm(Qty_C ~ Year * GDD, data = BEPA)
BEPA.Qty_C20 <- glm(Qty_C ~ EVI * Site, data = BEPA)
BEPA.Qty_C21 <- glm(Qty_C ~ NDMI * Site, data = BEPA)
BEPA.Qty_C22 <- glm(Qty_C ~ GDD * Site, data = BEPA)
BEPA.Qty_C23 <- glm(Qty_C ~ EVI * NDMI, data = BEPA)
BEPA.Qty_C24 <- glm(Qty_C ~ NDMI * GDD, data = BEPA)
BEPA.Qty_C25 <- glm(Qty_C ~ GDD * EVI, data = BEPA)
BEPA.Qty_C26 <- glm(Qty_C ~ Site, data = BEPA)
BEPA.Qty_C27 <- glm(Qty_C ~ NDMI, data = BEPA)
BEPA.Qty_C28 <- glm(Qty_C ~ GDD, data = BEPA)
BEPA.Qty_C29 <- glm(Qty_C ~ EVI, data = BEPA) 
BEPA.Qty_C30 <- glm(Qty_C ~ Year, data = BEPA) 
BEPA.Qty_C31 <- glm(Qty_C ~ 1, data =  BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Qty_Cmodels <- list(BEPA.Qty_C1, BEPA.Qty_C2, BEPA.Qty_C3, BEPA.Qty_C4, BEPA.Qty_C5, BEPA.Qty_C6, BEPA.Qty_C7, BEPA.Qty_C8, BEPA.Qty_C9, BEPA.Qty_C10, BEPA.Qty_C11, BEPA.Qty_C12, BEPA.Qty_C13, BEPA.Qty_C14, BEPA.Qty_C15, BEPA.Qty_C16, BEPA.Qty_C17, BEPA.Qty_C18, BEPA.Qty_C19, BEPA.Qty_C20, BEPA.Qty_C21, BEPA.Qty_C22, BEPA.Qty_C23, BEPA.Qty_C24, BEPA.Qty_C25, BEPA.Qty_C26, BEPA.Qty_C27, BEPA.Qty_C28, BEPA.Qty_C29, BEPA.Qty_C30, BEPA.Qty_C31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.Qty_C.residplots <- imap(BEPA.Qty_Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/BEPA_QtyC_glm.pdf")
BEPA.Qty_C.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.Qty_C <- list("BEPA.Qty_C1" = BEPA.Qty_C1, "BEPA.Qty_C2" = BEPA.Qty_C2, "BEPA.Qty_C3" = BEPA.Qty_C3, "BEPA.Qty_C4" = BEPA.Qty_C4, "BEPA.Qty_C5" = BEPA.Qty_C5, "BEPA.Qty_C6" = BEPA.Qty_C6, "BEPA.Qty_C7" = BEPA.Qty_C7, "BEPA.Qty_C8" =  BEPA.Qty_C8, "BEPA.Qty_C9" = BEPA.Qty_C9, "BEPA.Qty_C10" = BEPA.Qty_C10, "BEPA.Qty_C11" = BEPA.Qty_C11, "BEPA.Qty_C12" = BEPA.Qty_C12, "BEPA.Qty_C13" = BEPA.Qty_C13, "BEPA.Qty_C14" = BEPA.Qty_C14, "BEPA.Qty_C15" = BEPA.Qty_C15, "BEPA.Qty_C16" = BEPA.Qty_C16, "BEPA.Qty_C17" = BEPA.Qty_C17, "BEPA.Qty_C18" = BEPA.Qty_C18, "BEPA.Qty_C19" = BEPA.Qty_C19, "BEPA.Qty_C20" = BEPA.Qty_C20, "BEPA.Qty_C21" = BEPA.Qty_C21, "BEPA.Qty_C22" = BEPA.Qty_C22, "BEPA.Qty_C23" = BEPA.Qty_C23, "BEPA.Qty_C24" =  BEPA.Qty_C24, "BEPA.Qty_C25" = BEPA.Qty_C25, "BEPA.Qty_C26" = BEPA.Qty_C26, "BEPA.Qty_C27" = BEPA.Qty_C27, "BEPA.Qty_C28" = BEPA.Qty_C28, "BEPA.Qty_C29" = BEPA.Qty_C29, "BEPA.Qty_C30" = BEPA.Qty_C30, "BEPA.Qty_C31" = BEPA.Qty_C31)
BEPA.Qty_C <- aictab(cand.set = Models.BEPA.Qty_C)
print(BEPA.Qty_C)
write.csv(BEPA.Qty_C, "output/AIC/BEPA_QtyC.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(BEPA.Qty_C31)
summary(BEPA.Qty_C28)
summary(BEPA.Qty_C29)
summary(BEPA.Qty_C30)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.Qty_C31, which = "Nagelkerke")
PseudoR2(BEPA.Qty_C28, which = "Nagelkerke")
PseudoR2(BEPA.Qty_C29, which = "Nagelkerke")
PseudoR2(BEPA.Qty_C30, which = "Nagelkerke")
#Qty_P
BEPA.Qty_P1 <- glm(Qty_P ~ Year * EVI * GDD * NDMI * Site, data = BEPA)
BEPA.Qty_P2 <- glm(Qty_P ~ Year * EVI * GDD * NDMI, data = BEPA)
BEPA.Qty_P3 <- glm(Qty_P ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.Qty_P4 <- glm(Qty_P ~ Year * EVI * Site * NDMI, data = BEPA)
BEPA.Qty_P5 <- glm(Qty_P ~ Year * Site * GDD * NDMI, data = BEPA) 
BEPA.Qty_P6 <- glm(Qty_P ~ Site * EVI * GDD * NDMI, data = BEPA) 
BEPA.Qty_P7 <- glm(Qty_P ~ Year * EVI * GDD, data = BEPA) 
BEPA.Qty_P8 <- glm(Qty_P ~ Year * EVI * NDMI, data = BEPA)
BEPA.Qty_P9 <- glm(Qty_P ~ Year * EVI * Site, data = BEPA)
BEPA.Qty_P10 <- glm(Qty_P ~ EVI * GDD * NDMI, data = BEPA)
BEPA.Qty_P11 <- glm(Qty_P ~ EVI * GDD * Site, data = BEPA)
BEPA.Qty_P12 <- glm(Qty_P ~ Year * GDD * NDMI, data = BEPA)
BEPA.Qty_P13 <- glm(Qty_P ~ Year * Site * GDD, data = BEPA)
BEPA.Qty_P14 <- glm(Qty_P ~ Year * NDMI * Site, data = BEPA)
BEPA.Qty_P15 <- glm(Qty_P ~ GDD * NDMI * Site, data = BEPA)
BEPA.Qty_P16 <- glm(Qty_P ~ Year * Site, data = BEPA)
BEPA.Qty_P17 <- glm(Qty_P ~ Year * EVI, data = BEPA)
BEPA.Qty_P18 <- glm(Qty_P ~ Year * NDMI, data = BEPA)
BEPA.Qty_P19 <- glm(Qty_P ~ Year * GDD, data = BEPA)
BEPA.Qty_P20 <- glm(Qty_P ~ EVI * Site, data = BEPA)
BEPA.Qty_P21 <- glm(Qty_P ~ NDMI * Site, data = BEPA)
BEPA.Qty_P22 <- glm(Qty_P ~ GDD * Site, data = BEPA)
BEPA.Qty_P23 <- glm(Qty_P ~ EVI * NDMI, data = BEPA)
BEPA.Qty_P24 <- glm(Qty_P ~ NDMI * GDD, data = BEPA)
BEPA.Qty_P25 <- glm(Qty_P ~ GDD * EVI, data = BEPA)
BEPA.Qty_P26 <- glm(Qty_P ~ Site, data = BEPA)
BEPA.Qty_P27 <- glm(Qty_P ~ NDMI, data = BEPA)
BEPA.Qty_P28 <- glm(Qty_P ~ GDD, data = BEPA)
BEPA.Qty_P29 <- glm(Qty_P ~ EVI, data = BEPA) 
BEPA.Qty_P30 <- glm(Qty_P ~ Year, data = BEPA) 
BEPA.Qty_P31 <- glm(Qty_P ~ 1, data =  BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Qty_Pmodels <- list(BEPA.Qty_P1, BEPA.Qty_P2, BEPA.Qty_P3, BEPA.Qty_P4, BEPA.Qty_P5, BEPA.Qty_P6, BEPA.Qty_P7, BEPA.Qty_P8, BEPA.Qty_P9, BEPA.Qty_P10, BEPA.Qty_P11, BEPA.Qty_P12, BEPA.Qty_P13, BEPA.Qty_P14, BEPA.Qty_P15, BEPA.Qty_P16, BEPA.Qty_P17, BEPA.Qty_P18, BEPA.Qty_P19, BEPA.Qty_P20, BEPA.Qty_P21, BEPA.Qty_P22, BEPA.Qty_P23, BEPA.Qty_P24, BEPA.Qty_P25, BEPA.Qty_P26, BEPA.Qty_P27, BEPA.Qty_P28, BEPA.Qty_P29, BEPA.Qty_P30, BEPA.Qty_P31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.Qty_P.residplots <- imap(BEPA.Qty_Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/BEPA_QtyP_glm.pdf")
BEPA.Qty_P.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.Qty_P <- list("BEPA.Qty_P1" = BEPA.Qty_P1, "BEPA.Qty_P2" = BEPA.Qty_P2, "BEPA.Qty_P3" = BEPA.Qty_P3, "BEPA.Qty_P4" = BEPA.Qty_P4, "BEPA.Qty_P5" = BEPA.Qty_P5, "BEPA.Qty_P6" = BEPA.Qty_P6, "BEPA.Qty_P7" = BEPA.Qty_P7, "BEPA.Qty_P8" =  BEPA.Qty_P8, "BEPA.Qty_P9" = BEPA.Qty_P9, "BEPA.Qty_P10" = BEPA.Qty_P10, "BEPA.Qty_P11" = BEPA.Qty_P11, "BEPA.Qty_P12" = BEPA.Qty_P12, "BEPA.Qty_P13" = BEPA.Qty_P13, "BEPA.Qty_P14" = BEPA.Qty_P14, "BEPA.Qty_P15" = BEPA.Qty_P15, "BEPA.Qty_P16" = BEPA.Qty_P16, "BEPA.Qty_P17" = BEPA.Qty_P17, "BEPA.Qty_P18" = BEPA.Qty_P18, "BEPA.Qty_P19" = BEPA.Qty_P19, "BEPA.Qty_P20" = BEPA.Qty_P20, "BEPA.Qty_P21" = BEPA.Qty_P21, "BEPA.Qty_P22" = BEPA.Qty_P22, "BEPA.Qty_P23" = BEPA.Qty_P23, "BEPA.Qty_P24" =  BEPA.Qty_P24, "BEPA.Qty_P25" = BEPA.Qty_P25, "BEPA.Qty_P26" = BEPA.Qty_P26, "BEPA.Qty_P27" = BEPA.Qty_P27, "BEPA.Qty_P28" = BEPA.Qty_P28, "BEPA.Qty_P29" = BEPA.Qty_P29, "BEPA.Qty_P30" = BEPA.Qty_P30, "BEPA.Qty_P31" = BEPA.Qty_P31)
BEPA.Qty_P <- aictab(cand.set = Models.BEPA.Qty_P)
print(BEPA.Qty_P)
write.csv(BEPA.Qty_P, "output/AIC/BEPA_QtyP.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(BEPA.Qty_P31)
summary(BEPA.Qty_P30)
summary(BEPA.Qty_P27)
summary(BEPA.Qty_P28)
summary(BEPA.Qty_P29)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.Qty_P31, which = "Nagelkerke")
PseudoR2(BEPA.Qty_P30, which = "Nagelkerke")
PseudoR2(BEPA.Qty_P27, which = "Nagelkerke")
PseudoR2(BEPA.Qty_P28, which = "Nagelkerke")
PseudoR2(BEPA.Qty_P29, which = "Nagelkerke")
#Qty_N
BEPA.Qty_N1 <- glm(Qty_N ~ Year * EVI * GDD * NDMI * Site, data = BEPA)
BEPA.Qty_N2 <- glm(Qty_N ~ Year * EVI * GDD * NDMI, data = BEPA)
BEPA.Qty_N3 <- glm(Qty_N ~ Year * EVI * GDD * Site, data = BEPA)
BEPA.Qty_N4 <- glm(Qty_N ~ Year * EVI * Site * NDMI, data = BEPA)
BEPA.Qty_N5 <- glm(Qty_N ~ Year * Site * GDD * NDMI, data = BEPA) 
BEPA.Qty_N6 <- glm(Qty_N ~ Site * EVI * GDD * NDMI, data = BEPA) 
BEPA.Qty_N7 <- glm(Qty_N ~ Year * EVI * GDD, data = BEPA) 
BEPA.Qty_N8 <- glm(Qty_N ~ Year * EVI * NDMI, data = BEPA)
BEPA.Qty_N9 <- glm(Qty_N ~ Year * EVI * Site, data = BEPA)
BEPA.Qty_N10 <- glm(Qty_N ~ EVI * GDD * NDMI, data = BEPA)
BEPA.Qty_N11 <- glm(Qty_N ~ EVI * GDD * Site, data = BEPA)
BEPA.Qty_N12 <- glm(Qty_N ~ Year * GDD * NDMI, data = BEPA)
BEPA.Qty_N13 <- glm(Qty_N ~ Year * Site * GDD, data = BEPA)
BEPA.Qty_N14 <- glm(Qty_N ~ Year * NDMI * Site, data = BEPA)
BEPA.Qty_N15 <- glm(Qty_N ~ GDD * NDMI * Site, data = BEPA)
BEPA.Qty_N16 <- glm(Qty_N ~ Year * Site, data = BEPA)
BEPA.Qty_N17 <- glm(Qty_N ~ Year * EVI, data = BEPA)
BEPA.Qty_N18 <- glm(Qty_N ~ Year * NDMI, data = BEPA)
BEPA.Qty_N19 <- glm(Qty_N ~ Year * GDD, data = BEPA)
BEPA.Qty_N20 <- glm(Qty_N ~ EVI * Site, data = BEPA)
BEPA.Qty_N21 <- glm(Qty_N ~ NDMI * Site, data = BEPA)
BEPA.Qty_N22 <- glm(Qty_N ~ GDD * Site, data = BEPA)
BEPA.Qty_N23 <- glm(Qty_N ~ EVI * NDMI, data = BEPA)
BEPA.Qty_N24 <- glm(Qty_N ~ NDMI * GDD, data = BEPA)
BEPA.Qty_N25 <- glm(Qty_N ~ GDD * EVI, data = BEPA)
BEPA.Qty_N26 <- glm(Qty_N ~ Site, data = BEPA)
BEPA.Qty_N27 <- glm(Qty_N ~ NDMI, data = BEPA)
BEPA.Qty_N28 <- glm(Qty_N ~ GDD, data = BEPA)
BEPA.Qty_N29 <- glm(Qty_N ~ EVI, data = BEPA) 
BEPA.Qty_N30 <- glm(Qty_N ~ Year, data = BEPA) 
BEPA.Qty_N31 <- glm(Qty_N ~ 1, data =  BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Qty_Nmodels <- list(BEPA.Qty_N1, BEPA.Qty_N2, BEPA.Qty_N3, BEPA.Qty_N4, BEPA.Qty_N5, BEPA.Qty_N6, BEPA.Qty_N7, BEPA.Qty_N8, BEPA.Qty_N9, BEPA.Qty_N10, BEPA.Qty_N11, BEPA.Qty_N12, BEPA.Qty_N13, BEPA.Qty_N14, BEPA.Qty_N15, BEPA.Qty_N16, BEPA.Qty_N17, BEPA.Qty_N18, BEPA.Qty_N19, BEPA.Qty_N20, BEPA.Qty_N21, BEPA.Qty_N22, BEPA.Qty_N23, BEPA.Qty_N24, BEPA.Qty_N25, BEPA.Qty_N26, BEPA.Qty_N27, BEPA.Qty_N28, BEPA.Qty_N29, BEPA.Qty_N30, BEPA.Qty_N31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.Qty_N.residplots <- imap(BEPA.Qty_Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/BEPA_QtyN_glm.pdf")
BEPA.Qty_N.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.Qty_N <- list("BEPA.Qty_N1" = BEPA.Qty_N1, "BEPA.Qty_N2" = BEPA.Qty_N2, "BEPA.Qty_N3" = BEPA.Qty_N3, "BEPA.Qty_N4" = BEPA.Qty_N4, "BEPA.Qty_N5" = BEPA.Qty_N5, "BEPA.Qty_N6" = BEPA.Qty_N6, "BEPA.Qty_N7" = BEPA.Qty_N7, "BEPA.Qty_N8" =  BEPA.Qty_N8, "BEPA.Qty_N9" = BEPA.Qty_N9, "BEPA.Qty_N10" = BEPA.Qty_N10, "BEPA.Qty_N11" = BEPA.Qty_N11, "BEPA.Qty_N12" = BEPA.Qty_N12, "BEPA.Qty_N13" = BEPA.Qty_N13, "BEPA.Qty_N14" = BEPA.Qty_N14, "BEPA.Qty_N15" = BEPA.Qty_N15, "BEPA.Qty_N16" = BEPA.Qty_N16, "BEPA.Qty_N17" = BEPA.Qty_N17, "BEPA.Qty_N18" = BEPA.Qty_N18, "BEPA.Qty_N19" = BEPA.Qty_N19, "BEPA.Qty_N20" = BEPA.Qty_N20, "BEPA.Qty_N21" = BEPA.Qty_N21, "BEPA.Qty_N22" = BEPA.Qty_N22, "BEPA.Qty_N23" = BEPA.Qty_N23, "BEPA.Qty_N24" =  BEPA.Qty_N24, "BEPA.Qty_N25" = BEPA.Qty_N25, "BEPA.Qty_N26" = BEPA.Qty_N26, "BEPA.Qty_N27" = BEPA.Qty_N27, "BEPA.Qty_N28" = BEPA.Qty_N28, "BEPA.Qty_N29" = BEPA.Qty_N29, "BEPA.Qty_N30" = BEPA.Qty_N30, "BEPA.Qty_N31" = BEPA.Qty_N31)
BEPA.Qty_N <- aictab(cand.set = Models.BEPA.Qty_N)
print(BEPA.Qty_N)
write.csv(BEPA.Qty_N, "output/AIC/BEPA_QtyN.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(BEPA.Qty_N31)
summary(BEPA.Qty_N27)
summary(BEPA.Qty_N30)
summary(BEPA.Qty_N29)
summary(BEPA.Qty_N28)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.Qty_N31, which = "Nagelkerke")
PseudoR2(BEPA.Qty_N27, which = "Nagelkerke")
PseudoR2(BEPA.Qty_N30, which = "Nagelkerke")
PseudoR2(BEPA.Qty_N29, which = "Nagelkerke")
PseudoR2(BEPA.Qty_N28, which = "Nagelkerke")

# VAAN
# %C
VAAN.C1 <- glm(C ~ Year * EVI * GDD * NDMI * Site, data = VAAN)
VAAN.C2 <- glm(C ~ Year * EVI * GDD * NDMI, data = VAAN)
VAAN.C3 <- glm(C ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.C4 <- glm(C ~ Year * EVI * Site * NDMI, data = VAAN)
VAAN.C5 <- glm(C ~ Year * Site * GDD * NDMI, data = VAAN) 
VAAN.C6 <- glm(C ~ Site * EVI * GDD * NDMI, data = VAAN) 
VAAN.C7 <- glm(C ~ Year * EVI * GDD, data = VAAN) 
VAAN.C8 <- glm(C ~ Year * EVI * NDMI, data = VAAN)
VAAN.C9 <- glm(C ~ Year * EVI * Site, data = VAAN)
VAAN.C10 <- glm(C ~ EVI * GDD * NDMI, data = VAAN)
VAAN.C11 <- glm(C ~ EVI * GDD * Site, data = VAAN)
VAAN.C12 <- glm(C ~ Year * GDD * NDMI, data = VAAN)
VAAN.C13 <- glm(C ~ Year * Site * GDD, data = VAAN)
VAAN.C14 <- glm(C ~ Year * NDMI * Site, data = VAAN)
VAAN.C15 <- glm(C ~ GDD * NDMI * Site, data = VAAN)
VAAN.C16 <- glm(C ~ Year * Site, data = VAAN)
VAAN.C17 <- glm(C ~ Year * EVI, data = VAAN)
VAAN.C18 <- glm(C ~ Year * NDMI, data = VAAN)
VAAN.C19 <- glm(C ~ Year * GDD, data = VAAN)
VAAN.C20 <- glm(C ~ EVI * Site, data = VAAN)
VAAN.C21 <- glm(C ~ NDMI * Site, data = VAAN)
VAAN.C22 <- glm(C ~ GDD * Site, data = VAAN)
VAAN.C23 <- glm(C ~ EVI * NDMI, data = VAAN)
VAAN.C24 <- glm(C ~ NDMI * GDD, data = VAAN)
VAAN.C25 <- glm(C ~ GDD * EVI, data = VAAN)
VAAN.C26 <- glm(C ~ Site, data = VAAN)
VAAN.C27 <- glm(C ~ NDMI, data = VAAN)
VAAN.C28 <- glm(C ~ GDD, data = VAAN)
VAAN.C29 <- glm(C ~ EVI, data = VAAN) 
VAAN.C30 <- glm(C ~ Year, data = VAAN) 
VAAN.C31 <- glm(C ~ 1, data =  VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Cmodels <- list(VAAN.C1, VAAN.C2, VAAN.C3, VAAN.C4, VAAN.C5, VAAN.C6, VAAN.C7, VAAN.C8, VAAN.C9, VAAN.C10, VAAN.C11, VAAN.C12, VAAN.C13, VAAN.C14, VAAN.C15, VAAN.C16, VAAN.C17, VAAN.C18, VAAN.C19, VAAN.C20, VAAN.C21, VAAN.C22, VAAN.C23, VAAN.C24, VAAN.C25, VAAN.C26, VAAN.C27, VAAN.C28, VAAN.C29, VAAN.C30, VAAN.C31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.C.residplots <- imap(VAAN.Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/VAAN_C_glm.pdf")
VAAN.C.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.C <- list("VAAN.C1" = VAAN.C1, "VAAN.C2" = VAAN.C2, "VAAN.C3" = VAAN.C3, "VAAN.C4" = VAAN.C4, "VAAN.C5" = VAAN.C5, "VAAN.C6" = VAAN.C6, "VAAN.C7" = VAAN.C7, "VAAN.C8" =  VAAN.C8, "VAAN.C9" = VAAN.C9, "VAAN.C10" = VAAN.C10, "VAAN.C11" = VAAN.C11, "VAAN.C12" = VAAN.C12, "VAAN.C13" = VAAN.C13, "VAAN.C14" = VAAN.C14, "VAAN.C15" = VAAN.C15, "VAAN.C16" = VAAN.C16, "VAAN.C17" = VAAN.C17, "VAAN.C18" = VAAN.C18, "VAAN.C19" = VAAN.C19, "VAAN.C20" = VAAN.C20, "VAAN.C21" = VAAN.C21, "VAAN.C22" = VAAN.C22, "VAAN.C23" = VAAN.C23, "VAAN.C24" =  VAAN.C24, "VAAN.C25" = VAAN.C25, "VAAN.C26" = VAAN.C26, "VAAN.C27" = VAAN.C27, "VAAN.C28" = VAAN.C28, "VAAN.C29" = VAAN.C29, "VAAN.C30" = VAAN.C30, "VAAN.C31" = VAAN.C31)
VAAN.C <- aictab(cand.set = Models.VAAN.C)
print(VAAN.C)
write.csv(VAAN.C, "output/AIC/VAAN_C.csv")
# summary of the most parsimonious models - delta AICc of less than 2
summary(VAAN.C1)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.C1, which = "Nagelkerke")
#%P
VAAN.P1 <- glm(P ~ Year * EVI * GDD * NDMI * Site, data = VAAN)
VAAN.P2 <- glm(P ~ Year * EVI * GDD * NDMI, data = VAAN)
VAAN.P3 <- glm(P ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.P4 <- glm(P ~ Year * EVI * Site * NDMI, data = VAAN)
VAAN.P5 <- glm(P ~ Year * Site * GDD * NDMI, data = VAAN) 
VAAN.P6 <- glm(P ~ Site * EVI * GDD * NDMI, data = VAAN) 
VAAN.P7 <- glm(P ~ Year * EVI * GDD, data = VAAN) 
VAAN.P8 <- glm(P ~ Year * EVI * NDMI, data = VAAN)
VAAN.P9 <- glm(P ~ Year * EVI * Site, data = VAAN)
VAAN.P10 <- glm(P ~ EVI * GDD * NDMI, data = VAAN)
VAAN.P11 <- glm(P ~ EVI * GDD * Site, data = VAAN)
VAAN.P12 <- glm(P ~ Year * GDD * NDMI, data = VAAN)
VAAN.P13 <- glm(P ~ Year * Site * GDD, data = VAAN)
VAAN.P14 <- glm(P ~ Year * NDMI * Site, data = VAAN)
VAAN.P15 <- glm(P ~ GDD * NDMI * Site, data = VAAN)
VAAN.P16 <- glm(P ~ Year * Site, data = VAAN)
VAAN.P17 <- glm(P ~ Year * EVI, data = VAAN)
VAAN.P18 <- glm(P ~ Year * NDMI, data = VAAN)
VAAN.P19 <- glm(P ~ Year * GDD, data = VAAN)
VAAN.P20 <- glm(P ~ EVI * Site, data = VAAN)
VAAN.P21 <- glm(P ~ NDMI * Site, data = VAAN)
VAAN.P22 <- glm(P ~ GDD * Site, data = VAAN)
VAAN.P23 <- glm(P ~ EVI * NDMI, data = VAAN)
VAAN.P24 <- glm(P ~ NDMI * GDD, data = VAAN)
VAAN.P25 <- glm(P ~ GDD * EVI, data = VAAN)
VAAN.P26 <- glm(P ~ Site, data = VAAN)
VAAN.P27 <- glm(P ~ NDMI, data = VAAN)
VAAN.P28 <- glm(P ~ GDD, data = VAAN)
VAAN.P29 <- glm(P ~ EVI, data = VAAN) 
VAAN.P30 <- glm(P ~ Year, data = VAAN) 
VAAN.P31 <- glm(P ~ 1, data =  VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Pmodels <- list(VAAN.P1, VAAN.P2, VAAN.P3, VAAN.P4, VAAN.P5, VAAN.P6, VAAN.P7, VAAN.P8, VAAN.P9, VAAN.P10, VAAN.P11, VAAN.P12, VAAN.P13, VAAN.P14, VAAN.P15, VAAN.P16, VAAN.P17, VAAN.P18, VAAN.P19, VAAN.P20, VAAN.P21, VAAN.P22, VAAN.P23, VAAN.P24, VAAN.P25, VAAN.P26, VAAN.P27, VAAN.P28, VAAN.P29, VAAN.P30, VAAN.P31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.P.residplots <- imap(VAAN.Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/VAAN_P_glm.pdf")
VAAN.P.residplots
dev.off()

# create an AIPc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.P <- list("VAAN.P1" = VAAN.P1, "VAAN.P2" = VAAN.P2, "VAAN.P3" = VAAN.P3, "VAAN.P4" = VAAN.P4, "VAAN.P5" = VAAN.P5, "VAAN.P6" = VAAN.P6, "VAAN.P7" = VAAN.P7, "VAAN.P8" =  VAAN.P8, "VAAN.P9" = VAAN.P9, "VAAN.P10" = VAAN.P10, "VAAN.P11" = VAAN.P11, "VAAN.P12" = VAAN.P12, "VAAN.P13" = VAAN.P13, "VAAN.P14" = VAAN.P14, "VAAN.P15" = VAAN.P15, "VAAN.P16" = VAAN.P16, "VAAN.P17" = VAAN.P17, "VAAN.P18" = VAAN.P18, "VAAN.P19" = VAAN.P19, "VAAN.P20" = VAAN.P20, "VAAN.P21" = VAAN.P21, "VAAN.P22" = VAAN.P22, "VAAN.P23" = VAAN.P23, "VAAN.P24" =  VAAN.P24, "VAAN.P25" = VAAN.P25, "VAAN.P26" = VAAN.P26, "VAAN.P27" = VAAN.P27, "VAAN.P28" = VAAN.P28, "VAAN.P29" = VAAN.P29, "VAAN.P30" = VAAN.P30, "VAAN.P31" = VAAN.P31)
VAAN.P <- aictab(cand.set = Models.VAAN.P)
print(VAAN.P)
write.csv(VAAN.P, "output/AIC/VAAN_P.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(VAAN.P22)
summary(VAAN.P24)
summary(VAAN.P20)
summary(VAAN.P26)
summary(VAAN.P10)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.P22, which = "Nagelkerke")
PseudoR2(VAAN.P24, which = "Nagelkerke")
PseudoR2(VAAN.P20, which = "Nagelkerke")
PseudoR2(VAAN.P26, which = "Nagelkerke")
PseudoR2(VAAN.P10, which = "Nagelkerke")
#%N
VAAN.N1 <- glm(N ~ Year * EVI * GDD * NDMI * Site, data = VAAN)
VAAN.N2 <- glm(N ~ Year * EVI * GDD * NDMI, data = VAAN)
VAAN.N3 <- glm(N ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.N4 <- glm(N ~ Year * EVI * Site * NDMI, data = VAAN)
VAAN.N5 <- glm(N ~ Year * Site * GDD * NDMI, data = VAAN) 
VAAN.N6 <- glm(N ~ Site * EVI * GDD * NDMI, data = VAAN) 
VAAN.N7 <- glm(N ~ Year * EVI * GDD, data = VAAN) 
VAAN.N8 <- glm(N ~ Year * EVI * NDMI, data = VAAN)
VAAN.N9 <- glm(N ~ Year * EVI * Site, data = VAAN)
VAAN.N10 <- glm(N ~ EVI * GDD * NDMI, data = VAAN)
VAAN.N11 <- glm(N ~ EVI * GDD * Site, data = VAAN)
VAAN.N12 <- glm(N ~ Year * GDD * NDMI, data = VAAN)
VAAN.N13 <- glm(N ~ Year * Site * GDD, data = VAAN)
VAAN.N14 <- glm(N ~ Year * NDMI * Site, data = VAAN)
VAAN.N15 <- glm(N ~ GDD * NDMI * Site, data = VAAN)
VAAN.N16 <- glm(N ~ Year * Site, data = VAAN)
VAAN.N17 <- glm(N ~ Year * EVI, data = VAAN)
VAAN.N18 <- glm(N ~ Year * NDMI, data = VAAN)
VAAN.N19 <- glm(N ~ Year * GDD, data = VAAN)
VAAN.N20 <- glm(N ~ EVI * Site, data = VAAN)
VAAN.N21 <- glm(N ~ NDMI * Site, data = VAAN)
VAAN.N22 <- glm(N ~ GDD * Site, data = VAAN)
VAAN.N23 <- glm(N ~ EVI * NDMI, data = VAAN)
VAAN.N24 <- glm(N ~ NDMI * GDD, data = VAAN)
VAAN.N25 <- glm(N ~ GDD * EVI, data = VAAN)
VAAN.N26 <- glm(N ~ Site, data = VAAN)
VAAN.N27 <- glm(N ~ NDMI, data = VAAN)
VAAN.N28 <- glm(N ~ GDD, data = VAAN)
VAAN.N29 <- glm(N ~ EVI, data = VAAN) 
VAAN.N30 <- glm(N ~ Year, data = VAAN) 
VAAN.N31 <- glm(N ~ 1, data =  VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Nmodels <- list(VAAN.N1, VAAN.N2, VAAN.N3, VAAN.N4, VAAN.N5, VAAN.N6, VAAN.N7, VAAN.N8, VAAN.N9, VAAN.N10, VAAN.N11, VAAN.N12, VAAN.N13, VAAN.N14, VAAN.N15, VAAN.N16, VAAN.N17, VAAN.N18, VAAN.N19, VAAN.N20, VAAN.N21, VAAN.N22, VAAN.N23, VAAN.N24, VAAN.N25, VAAN.N26, VAAN.N27, VAAN.N28, VAAN.N29, VAAN.N30, VAAN.N31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.N.residplots <- imap(VAAN.Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/VAAN_N_glm.pdf")
VAAN.N.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.N <- list("VAAN.N1" = VAAN.N1, "VAAN.N2" = VAAN.N2, "VAAN.N3" = VAAN.N3, "VAAN.N4" = VAAN.N4, "VAAN.N5" = VAAN.N5, "VAAN.N6" = VAAN.N6, "VAAN.N7" = VAAN.N7, "VAAN.N8" =  VAAN.N8, "VAAN.N9" = VAAN.N9, "VAAN.N10" = VAAN.N10, "VAAN.N11" = VAAN.N11, "VAAN.N12" = VAAN.N12, "VAAN.N13" = VAAN.N13, "VAAN.N14" = VAAN.N14, "VAAN.N15" = VAAN.N15, "VAAN.N16" = VAAN.N16, "VAAN.N17" = VAAN.N17, "VAAN.N18" = VAAN.N18, "VAAN.N19" = VAAN.N19, "VAAN.N20" = VAAN.N20, "VAAN.N21" = VAAN.N21, "VAAN.N22" = VAAN.N22, "VAAN.N23" = VAAN.N23, "VAAN.N24" =  VAAN.N24, "VAAN.N25" = VAAN.N25, "VAAN.N26" = VAAN.N26, "VAAN.N27" = VAAN.N27, "VAAN.N28" = VAAN.N28, "VAAN.N29" = VAAN.N29, "VAAN.N30" = VAAN.N30, "VAAN.N31" = VAAN.N31)
VAAN.N <- aictab(cand.set = Models.VAAN.N)
print(VAAN.N)
write.csv(VAAN.N, "output/AIC/VAAN_N.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(VAAN.N25)
summary(VAAN.N10)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.N25, which = "Nagelkerke")
PseudoR2(VAAN.N10, which = "Nagelkerke")
#C:N
VAAN.CN1 <- glm(CNRatio ~ Year * EVI * GDD * NDMI * Site, data = VAAN)
VAAN.CN2 <- glm(CNRatio ~ Year * EVI * GDD * NDMI, data = VAAN)
VAAN.CN3 <- glm(CNRatio ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.CN4 <- glm(CNRatio ~ Year * EVI * Site * NDMI, data = VAAN)
VAAN.CN5 <- glm(CNRatio ~ Year * Site * GDD * NDMI, data = VAAN) 
VAAN.CN6 <- glm(CNRatio ~ Site * EVI * GDD * NDMI, data = VAAN) 
VAAN.CN7 <- glm(CNRatio ~ Year * EVI * GDD, data = VAAN) 
VAAN.CN8 <- glm(CNRatio ~ Year * EVI * NDMI, data = VAAN)
VAAN.CN9 <- glm(CNRatio ~ Year * EVI * Site, data = VAAN)
VAAN.CN10 <- glm(CNRatio ~ EVI * GDD * NDMI, data = VAAN)
VAAN.CN11 <- glm(CNRatio ~ EVI * GDD * Site, data = VAAN)
VAAN.CN12 <- glm(CNRatio ~ Year * GDD * NDMI, data = VAAN)
VAAN.CN13 <- glm(CNRatio ~ Year * Site * GDD, data = VAAN)
VAAN.CN14 <- glm(CNRatio ~ Year * NDMI * Site, data = VAAN)
VAAN.CN15 <- glm(CNRatio ~ GDD * NDMI * Site, data = VAAN)
VAAN.CN16 <- glm(CNRatio ~ Year * Site, data = VAAN)
VAAN.CN17 <- glm(CNRatio ~ Year * EVI, data = VAAN)
VAAN.CN18 <- glm(CNRatio ~ Year * NDMI, data = VAAN)
VAAN.CN19 <- glm(CNRatio ~ Year * GDD, data = VAAN)
VAAN.CN20 <- glm(CNRatio ~ EVI * Site, data = VAAN)
VAAN.CN21 <- glm(CNRatio ~ NDMI * Site, data = VAAN)
VAAN.CN22 <- glm(CNRatio ~ GDD * Site, data = VAAN)
VAAN.CN23 <- glm(CNRatio ~ EVI * NDMI, data = VAAN)
VAAN.CN24 <- glm(CNRatio ~ NDMI * GDD, data = VAAN)
VAAN.CN25 <- glm(CNRatio ~ GDD * EVI, data = VAAN)
VAAN.CN26 <- glm(CNRatio ~ Site, data = VAAN)
VAAN.CN27 <- glm(CNRatio ~ NDMI, data = VAAN)
VAAN.CN28 <- glm(CNRatio ~ GDD, data = VAAN)
VAAN.CN29 <- glm(CNRatio ~ EVI, data = VAAN) 
VAAN.CN30 <- glm(CNRatio ~ Year, data = VAAN) 
VAAN.CN31 <- glm(CNRatio ~ 1, data =  VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.CNmodels <- list(VAAN.CN1, VAAN.CN2, VAAN.CN3, VAAN.CN4, VAAN.CN5, VAAN.CN6, VAAN.CN7, VAAN.CN8, VAAN.CN9, VAAN.CN10, VAAN.CN11, VAAN.CN12, VAAN.CN13, VAAN.CN14, VAAN.CN15, VAAN.CN16, VAAN.CN17, VAAN.CN18, VAAN.CN19, VAAN.CN20, VAAN.CN21, VAAN.CN22, VAAN.CN23, VAAN.CN24, VAAN.CN25, VAAN.CN26, VAAN.CN27, VAAN.CN28, VAAN.CN29, VAAN.CN30, VAAN.CN31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.CN.residplots <- imap(VAAN.CNmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/VAAN_CN_glm.pdf")
VAAN.CN.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.CN <- list("VAAN.CN1" = VAAN.CN1, "VAAN.CN2" = VAAN.CN2, "VAAN.CN3" = VAAN.CN3, "VAAN.CN4" = VAAN.CN4, "VAAN.CN5" = VAAN.CN5, "VAAN.CN6" = VAAN.CN6, "VAAN.CN7" = VAAN.CN7, "VAAN.CN8" =  VAAN.CN8, "VAAN.CN9" = VAAN.CN9, "VAAN.CN10" = VAAN.CN10, "VAAN.CN11" = VAAN.CN11, "VAAN.CN12" = VAAN.CN12, "VAAN.CN13" = VAAN.CN13, "VAAN.CN14" = VAAN.CN14, "VAAN.CN15" = VAAN.CN15, "VAAN.CN16" = VAAN.CN16, "VAAN.CN17" = VAAN.CN17, "VAAN.CN18" = VAAN.CN18, "VAAN.CN19" = VAAN.CN19, "VAAN.CN20" = VAAN.CN20, "VAAN.CN21" = VAAN.CN21, "VAAN.CN22" = VAAN.CN22, "VAAN.CN23" = VAAN.CN23, "VAAN.CN24" =  VAAN.CN24, "VAAN.CN25" = VAAN.CN25, "VAAN.CN26" = VAAN.CN26, "VAAN.CN27" = VAAN.CN27, "VAAN.CN28" = VAAN.CN28, "VAAN.CN29" = VAAN.CN29, "VAAN.CN30" = VAAN.CN30, "VAAN.CN31" = VAAN.CN31)
VAAN.CN <- aictab(cand.set = Models.VAAN.CN)
print(VAAN.CN)
write.csv(VAAN.CN, "output/AIC/VAAN_CN.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(VAAN.CN25)
summary(VAAN.CN10)
summary(VAAN.CN3)
summary(VAAN.CN24)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.CN25, which = "Nagelkerke")
PseudoR2(VAAN.CN10, which = "Nagelkerke")
PseudoR2(VAAN.CN3, which = "Nagelkerke")
PseudoR2(VAAN.CN24, which = "Nagelkerke")
#C:P
VAAN.CP1 <- glm(CPRatio ~ Year * EVI * GDD * NDMI * Site, data = VAAN)
VAAN.CP2 <- glm(CPRatio ~ Year * EVI * GDD * NDMI, data = VAAN)
VAAN.CP3 <- glm(CPRatio ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.CP4 <- glm(CPRatio ~ Year * EVI * Site * NDMI, data = VAAN)
VAAN.CP5 <- glm(CPRatio ~ Year * Site * GDD * NDMI, data = VAAN) 
VAAN.CP6 <- glm(CPRatio ~ Site * EVI * GDD * NDMI, data = VAAN) 
VAAN.CP7 <- glm(CPRatio ~ Year * EVI * GDD, data = VAAN) 
VAAN.CP8 <- glm(CPRatio ~ Year * EVI * NDMI, data = VAAN)
VAAN.CP9 <- glm(CPRatio ~ Year * EVI * Site, data = VAAN)
VAAN.CP10 <- glm(CPRatio ~ EVI * GDD * NDMI, data = VAAN)
VAAN.CP11 <- glm(CPRatio ~ EVI * GDD * Site, data = VAAN)
VAAN.CP12 <- glm(CPRatio ~ Year * GDD * NDMI, data = VAAN)
VAAN.CP13 <- glm(CPRatio ~ Year * Site * GDD, data = VAAN)
VAAN.CP14 <- glm(CPRatio ~ Year * NDMI * Site, data = VAAN)
VAAN.CP15 <- glm(CPRatio ~ GDD * NDMI * Site, data = VAAN)
VAAN.CP16 <- glm(CPRatio ~ Year * Site, data = VAAN)
VAAN.CP17 <- glm(CPRatio ~ Year * EVI, data = VAAN)
VAAN.CP18 <- glm(CPRatio ~ Year * NDMI, data = VAAN)
VAAN.CP19 <- glm(CPRatio ~ Year * GDD, data = VAAN)
VAAN.CP20 <- glm(CPRatio ~ EVI * Site, data = VAAN)
VAAN.CP21 <- glm(CPRatio ~ NDMI * Site, data = VAAN)
VAAN.CP22 <- glm(CPRatio ~ GDD * Site, data = VAAN)
VAAN.CP23 <- glm(CPRatio ~ EVI * NDMI, data = VAAN)
VAAN.CP24 <- glm(CPRatio ~ NDMI * GDD, data = VAAN)
VAAN.CP25 <- glm(CPRatio ~ GDD * EVI, data = VAAN)
VAAN.CP26 <- glm(CPRatio ~ Site, data = VAAN)
VAAN.CP27 <- glm(CPRatio ~ NDMI, data = VAAN)
VAAN.CP28 <- glm(CPRatio ~ GDD, data = VAAN)
VAAN.CP29 <- glm(CPRatio ~ EVI, data = VAAN) 
VAAN.CP30 <- glm(CPRatio ~ Year, data = VAAN) 
VAAN.CP31 <- glm(CPRatio ~ 1, data =  VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.CPmodels <- list(VAAN.CP1, VAAN.CP2, VAAN.CP3, VAAN.CP4, VAAN.CP5, VAAN.CP6, VAAN.CP7, VAAN.CP8, VAAN.CP9, VAAN.CP10, VAAN.CP11, VAAN.CP12, VAAN.CP13, VAAN.CP14, VAAN.CP15, VAAN.CP16, VAAN.CP17, VAAN.CP18, VAAN.CP19, VAAN.CP20, VAAN.CP21, VAAN.CP22, VAAN.CP23, VAAN.CP24, VAAN.CP25, VAAN.CP26, VAAN.CP27, VAAN.CP28, VAAN.CP29, VAAN.CP30, VAAN.CP31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.CP.residplots <- imap(VAAN.CPmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/VAAN_CP_glm.pdf")
VAAN.CP.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.CP <- list("VAAN.CP1" = VAAN.CP1, "VAAN.CP2" = VAAN.CP2, "VAAN.CP3" = VAAN.CP3, "VAAN.CP4" = VAAN.CP4, "VAAN.CP5" = VAAN.CP5, "VAAN.CP6" = VAAN.CP6, "VAAN.CP7" = VAAN.CP7, "VAAN.CP8" =  VAAN.CP8, "VAAN.CP9" = VAAN.CP9, "VAAN.CP10" = VAAN.CP10, "VAAN.CP11" = VAAN.CP11, "VAAN.CP12" = VAAN.CP12, "VAAN.CP13" = VAAN.CP13, "VAAN.CP14" = VAAN.CP14, "VAAN.CP15" = VAAN.CP15, "VAAN.CP16" = VAAN.CP16, "VAAN.CP17" = VAAN.CP17, "VAAN.CP18" = VAAN.CP18, "VAAN.CP19" = VAAN.CP19, "VAAN.CP20" = VAAN.CP20, "VAAN.CP21" = VAAN.CP21, "VAAN.CP22" = VAAN.CP22, "VAAN.CP23" = VAAN.CP23, "VAAN.CP24" =  VAAN.CP24, "VAAN.CP25" = VAAN.CP25, "VAAN.CP26" = VAAN.CP26, "VAAN.CP27" = VAAN.CP27, "VAAN.CP28" = VAAN.CP28, "VAAN.CP29" = VAAN.CP29, "VAAN.CP30" = VAAN.CP30, "VAAN.CP31" = VAAN.CP31)
VAAN.CP <- aictab(cand.set = Models.VAAN.CP)
print(VAAN.CP)
write.csv(VAAN.CP, "output/AIC/VAAN_CP.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(VAAN.CP10)
summary(VAAN.CP20)
summary(VAAN.CP24)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.CP10, which = "Nagelkerke")
PseudoR2(VAAN.CP20, which = "Nagelkerke")
PseudoR2(VAAN.CP24, which = "Nagelkerke")
#N:P
VAAN.NP1 <- glm(NPRatio ~ Year * EVI * GDD * NDMI * Site, data = VAAN)
VAAN.NP2 <- glm(NPRatio ~ Year * EVI * GDD * NDMI, data = VAAN)
VAAN.NP3 <- glm(NPRatio ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.NP4 <- glm(NPRatio ~ Year * EVI * Site * NDMI, data = VAAN)
VAAN.NP5 <- glm(NPRatio ~ Year * Site * GDD * NDMI, data = VAAN) 
VAAN.NP6 <- glm(NPRatio ~ Site * EVI * GDD * NDMI, data = VAAN) 
VAAN.NP7 <- glm(NPRatio ~ Year * EVI * GDD, data = VAAN) 
VAAN.NP8 <- glm(NPRatio ~ Year * EVI * NDMI, data = VAAN)
VAAN.NP9 <- glm(NPRatio ~ Year * EVI * Site, data = VAAN)
VAAN.NP10 <- glm(NPRatio ~ EVI * GDD * NDMI, data = VAAN)
VAAN.NP11 <- glm(NPRatio ~ EVI * GDD * Site, data = VAAN)
VAAN.NP12 <- glm(NPRatio ~ Year * GDD * NDMI, data = VAAN)
VAAN.NP13 <- glm(NPRatio ~ Year * Site * GDD, data = VAAN)
VAAN.NP14 <- glm(NPRatio ~ Year * NDMI * Site, data = VAAN)
VAAN.NP15 <- glm(NPRatio ~ GDD * NDMI * Site, data = VAAN)
VAAN.NP16 <- glm(NPRatio ~ Year * Site, data = VAAN)
VAAN.NP17 <- glm(NPRatio ~ Year * EVI, data = VAAN)
VAAN.NP18 <- glm(NPRatio ~ Year * NDMI, data = VAAN)
VAAN.NP19 <- glm(NPRatio ~ Year * GDD, data = VAAN)
VAAN.NP20 <- glm(NPRatio ~ EVI * Site, data = VAAN)
VAAN.NP21 <- glm(NPRatio ~ NDMI * Site, data = VAAN)
VAAN.NP22 <- glm(NPRatio ~ GDD * Site, data = VAAN)
VAAN.NP23 <- glm(NPRatio ~ EVI * NDMI, data = VAAN)
VAAN.NP24 <- glm(NPRatio ~ NDMI * GDD, data = VAAN)
VAAN.NP25 <- glm(NPRatio ~ GDD * EVI, data = VAAN)
VAAN.NP26 <- glm(NPRatio ~ Site, data = VAAN)
VAAN.NP27 <- glm(NPRatio ~ NDMI, data = VAAN)
VAAN.NP28 <- glm(NPRatio ~ GDD, data = VAAN)
VAAN.NP29 <- glm(NPRatio ~ EVI, data = VAAN) 
VAAN.NP30 <- glm(NPRatio ~ Year, data = VAAN) 
VAAN.NP31 <- glm(NPRatio ~ 1, data =  VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.NPmodels <- list(VAAN.NP1, VAAN.NP2, VAAN.NP3, VAAN.NP4, VAAN.NP5, VAAN.NP6, VAAN.NP7, VAAN.NP8, VAAN.NP9, VAAN.NP10, VAAN.NP11, VAAN.NP12, VAAN.NP13, VAAN.NP14, VAAN.NP15, VAAN.NP16, VAAN.NP17, VAAN.NP18, VAAN.NP19, VAAN.NP20, VAAN.NP21, VAAN.NP22, VAAN.NP23, VAAN.NP24, VAAN.NP25, VAAN.NP26, VAAN.NP27, VAAN.NP28, VAAN.NP29, VAAN.NP30, VAAN.NP31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.NP.residplots <- imap(VAAN.NPmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/VAAN_NP_glm.pdf")
VAAN.NP.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.NP <- list("VAAN.NP1" = VAAN.NP1, "VAAN.NP2" = VAAN.NP2, "VAAN.NP3" = VAAN.NP3, "VAAN.NP4" = VAAN.NP4, "VAAN.NP5" = VAAN.NP5, "VAAN.NP6" = VAAN.NP6, "VAAN.NP7" = VAAN.NP7, "VAAN.NP8" =  VAAN.NP8, "VAAN.NP9" = VAAN.NP9, "VAAN.NP10" = VAAN.NP10, "VAAN.NP11" = VAAN.NP11, "VAAN.NP12" = VAAN.NP12, "VAAN.NP13" = VAAN.NP13, "VAAN.NP14" = VAAN.NP14, "VAAN.NP15" = VAAN.NP15, "VAAN.NP16" = VAAN.NP16, "VAAN.NP17" = VAAN.NP17, "VAAN.NP18" = VAAN.NP18, "VAAN.NP19" = VAAN.NP19, "VAAN.NP20" = VAAN.NP20, "VAAN.NP21" = VAAN.NP21, "VAAN.NP22" = VAAN.NP22, "VAAN.NP23" = VAAN.NP23, "VAAN.NP24" =  VAAN.NP24, "VAAN.NP25" = VAAN.NP25, "VAAN.NP26" = VAAN.NP26, "VAAN.NP27" = VAAN.NP27, "VAAN.NP28" = VAAN.NP28, "VAAN.NP29" = VAAN.NP29, "VAAN.NP30" = VAAN.NP30, "VAAN.NP31" = VAAN.NP31)
VAAN.NP <- aictab(cand.set = Models.VAAN.NP)
print(VAAN.NP)
write.csv(VAAN.NP, "output/AIC/VAAN_NP.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(VAAN.NP20)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.NP20, which = "Nagelkerke")
#Qty_C
VAAN.Qty_C1 <- glm(Qty_C ~ Year * EVI * GDD * NDMI * Site, data = VAAN)
VAAN.Qty_C2 <- glm(Qty_C ~ Year * EVI * GDD * NDMI, data = VAAN)
VAAN.Qty_C3 <- glm(Qty_C ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.Qty_C4 <- glm(Qty_C ~ Year * EVI * Site * NDMI, data = VAAN)
VAAN.Qty_C5 <- glm(Qty_C ~ Year * Site * GDD * NDMI, data = VAAN) 
VAAN.Qty_C6 <- glm(Qty_C ~ Site * EVI * GDD * NDMI, data = VAAN) 
VAAN.Qty_C7 <- glm(Qty_C ~ Year * EVI * GDD, data = VAAN) 
VAAN.Qty_C8 <- glm(Qty_C ~ Year * EVI * NDMI, data = VAAN)
VAAN.Qty_C9 <- glm(Qty_C ~ Year * EVI * Site, data = VAAN)
VAAN.Qty_C10 <- glm(Qty_C ~ EVI * GDD * NDMI, data = VAAN)
VAAN.Qty_C11 <- glm(Qty_C ~ EVI * GDD * Site, data = VAAN)
VAAN.Qty_C12 <- glm(Qty_C ~ Year * GDD * NDMI, data = VAAN)
VAAN.Qty_C13 <- glm(Qty_C ~ Year * Site * GDD, data = VAAN)
VAAN.Qty_C14 <- glm(Qty_C ~ Year * NDMI * Site, data = VAAN)
VAAN.Qty_C15 <- glm(Qty_C ~ GDD * NDMI * Site, data = VAAN)
VAAN.Qty_C16 <- glm(Qty_C ~ Year * Site, data = VAAN)
VAAN.Qty_C17 <- glm(Qty_C ~ Year * EVI, data = VAAN)
VAAN.Qty_C18 <- glm(Qty_C ~ Year * NDMI, data = VAAN)
VAAN.Qty_C19 <- glm(Qty_C ~ Year * GDD, data = VAAN)
VAAN.Qty_C20 <- glm(Qty_C ~ EVI * Site, data = VAAN)
VAAN.Qty_C21 <- glm(Qty_C ~ NDMI * Site, data = VAAN)
VAAN.Qty_C22 <- glm(Qty_C ~ GDD * Site, data = VAAN)
VAAN.Qty_C23 <- glm(Qty_C ~ EVI * NDMI, data = VAAN)
VAAN.Qty_C24 <- glm(Qty_C ~ NDMI * GDD, data = VAAN)
VAAN.Qty_C25 <- glm(Qty_C ~ GDD * EVI, data = VAAN)
VAAN.Qty_C26 <- glm(Qty_C ~ Site, data = VAAN)
VAAN.Qty_C27 <- glm(Qty_C ~ NDMI, data = VAAN)
VAAN.Qty_C28 <- glm(Qty_C ~ GDD, data = VAAN)
VAAN.Qty_C29 <- glm(Qty_C ~ EVI, data = VAAN) 
VAAN.Qty_C30 <- glm(Qty_C ~ Year, data = VAAN) 
VAAN.Qty_C31 <- glm(Qty_C ~ 1, data =  VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Qty_Cmodels <- list(VAAN.Qty_C1, VAAN.Qty_C2, VAAN.Qty_C3, VAAN.Qty_C4, VAAN.Qty_C5, VAAN.Qty_C6, VAAN.Qty_C7, VAAN.Qty_C8, VAAN.Qty_C9, VAAN.Qty_C10, VAAN.Qty_C11, VAAN.Qty_C12, VAAN.Qty_C13, VAAN.Qty_C14, VAAN.Qty_C15, VAAN.Qty_C16, VAAN.Qty_C17, VAAN.Qty_C18, VAAN.Qty_C19, VAAN.Qty_C20, VAAN.Qty_C21, VAAN.Qty_C22, VAAN.Qty_C23, VAAN.Qty_C24, VAAN.Qty_C25, VAAN.Qty_C26, VAAN.Qty_C27, VAAN.Qty_C28, VAAN.Qty_C29, VAAN.Qty_C30, VAAN.Qty_C31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.Qty_C.residplots <- imap(VAAN.Qty_Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/VAAN_QtyC_glm.pdf")
VAAN.Qty_C.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.Qty_C <- list("VAAN.Qty_C1" = VAAN.Qty_C1, "VAAN.Qty_C2" = VAAN.Qty_C2, "VAAN.Qty_C3" = VAAN.Qty_C3, "VAAN.Qty_C4" = VAAN.Qty_C4, "VAAN.Qty_C5" = VAAN.Qty_C5, "VAAN.Qty_C6" = VAAN.Qty_C6, "VAAN.Qty_C7" = VAAN.Qty_C7, "VAAN.Qty_C8" =  VAAN.Qty_C8, "VAAN.Qty_C9" = VAAN.Qty_C9, "VAAN.Qty_C10" = VAAN.Qty_C10, "VAAN.Qty_C11" = VAAN.Qty_C11, "VAAN.Qty_C12" = VAAN.Qty_C12, "VAAN.Qty_C13" = VAAN.Qty_C13, "VAAN.Qty_C14" = VAAN.Qty_C14, "VAAN.Qty_C15" = VAAN.Qty_C15, "VAAN.Qty_C16" = VAAN.Qty_C16, "VAAN.Qty_C17" = VAAN.Qty_C17, "VAAN.Qty_C18" = VAAN.Qty_C18, "VAAN.Qty_C19" = VAAN.Qty_C19, "VAAN.Qty_C20" = VAAN.Qty_C20, "VAAN.Qty_C21" = VAAN.Qty_C21, "VAAN.Qty_C22" = VAAN.Qty_C22, "VAAN.Qty_C23" = VAAN.Qty_C23, "VAAN.Qty_C24" =  VAAN.Qty_C24, "VAAN.Qty_C25" = VAAN.Qty_C25, "VAAN.Qty_C26" = VAAN.Qty_C26, "VAAN.Qty_C27" = VAAN.Qty_C27, "VAAN.Qty_C28" = VAAN.Qty_C28, "VAAN.Qty_C29" = VAAN.Qty_C29, "VAAN.Qty_C30" = VAAN.Qty_C30, "VAAN.Qty_C31" = VAAN.Qty_C31)
VAAN.Qty_C <- aictab(cand.set = Models.VAAN.Qty_C)
print(VAAN.Qty_C)
write.csv(VAAN.Qty_C, "output/AIC/VAAN_QtyC.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(VAAN.Qty_C3)
summary(VAAN.Qty_C21)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.Qty_C3, which = "Nagelkerke")
PseudoR2(VAAN.Qty_C21, which = "Nagelkerke")
#Qty_P
VAAN.Qty_P1 <- glm(Qty_P ~ Year * EVI * GDD * NDMI * Site, data = VAAN)
VAAN.Qty_P2 <- glm(Qty_P ~ Year * EVI * GDD * NDMI, data = VAAN)
VAAN.Qty_P3 <- glm(Qty_P ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.Qty_P4 <- glm(Qty_P ~ Year * EVI * Site * NDMI, data = VAAN)
VAAN.Qty_P5 <- glm(Qty_P ~ Year * Site * GDD * NDMI, data = VAAN) 
VAAN.Qty_P6 <- glm(Qty_P ~ Site * EVI * GDD * NDMI, data = VAAN) 
VAAN.Qty_P7 <- glm(Qty_P ~ Year * EVI * GDD, data = VAAN) 
VAAN.Qty_P8 <- glm(Qty_P ~ Year * EVI * NDMI, data = VAAN)
VAAN.Qty_P9 <- glm(Qty_P ~ Year * EVI * Site, data = VAAN)
VAAN.Qty_P10 <- glm(Qty_P ~ EVI * GDD * NDMI, data = VAAN)
VAAN.Qty_P11 <- glm(Qty_P ~ EVI * GDD * Site, data = VAAN)
VAAN.Qty_P12 <- glm(Qty_P ~ Year * GDD * NDMI, data = VAAN)
VAAN.Qty_P13 <- glm(Qty_P ~ Year * Site * GDD, data = VAAN)
VAAN.Qty_P14 <- glm(Qty_P ~ Year * NDMI * Site, data = VAAN)
VAAN.Qty_P15 <- glm(Qty_P ~ GDD * NDMI * Site, data = VAAN)
VAAN.Qty_P16 <- glm(Qty_P ~ Year * Site, data = VAAN)
VAAN.Qty_P17 <- glm(Qty_P ~ Year * EVI, data = VAAN)
VAAN.Qty_P18 <- glm(Qty_P ~ Year * NDMI, data = VAAN)
VAAN.Qty_P19 <- glm(Qty_P ~ Year * GDD, data = VAAN)
VAAN.Qty_P20 <- glm(Qty_P ~ EVI * Site, data = VAAN)
VAAN.Qty_P21 <- glm(Qty_P ~ NDMI * Site, data = VAAN)
VAAN.Qty_P22 <- glm(Qty_P ~ GDD * Site, data = VAAN)
VAAN.Qty_P23 <- glm(Qty_P ~ EVI * NDMI, data = VAAN)
VAAN.Qty_P24 <- glm(Qty_P ~ NDMI * GDD, data = VAAN)
VAAN.Qty_P25 <- glm(Qty_P ~ GDD * EVI, data = VAAN)
VAAN.Qty_P26 <- glm(Qty_P ~ Site, data = VAAN)
VAAN.Qty_P27 <- glm(Qty_P ~ NDMI, data = VAAN)
VAAN.Qty_P28 <- glm(Qty_P ~ GDD, data = VAAN)
VAAN.Qty_P29 <- glm(Qty_P ~ EVI, data = VAAN) 
VAAN.Qty_P30 <- glm(Qty_P ~ Year, data = VAAN) 
VAAN.Qty_P31 <- glm(Qty_P ~ 1, data =  VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Qty_Pmodels <- list(VAAN.Qty_P1, VAAN.Qty_P2, VAAN.Qty_P3, VAAN.Qty_P4, VAAN.Qty_P5, VAAN.Qty_P6, VAAN.Qty_P7, VAAN.Qty_P8, VAAN.Qty_P9, VAAN.Qty_P10, VAAN.Qty_P11, VAAN.Qty_P12, VAAN.Qty_P13, VAAN.Qty_P14, VAAN.Qty_P15, VAAN.Qty_P16, VAAN.Qty_P17, VAAN.Qty_P18, VAAN.Qty_P19, VAAN.Qty_P20, VAAN.Qty_P21, VAAN.Qty_P22, VAAN.Qty_P23, VAAN.Qty_P24, VAAN.Qty_P25, VAAN.Qty_P26, VAAN.Qty_P27, VAAN.Qty_P28, VAAN.Qty_P29, VAAN.Qty_P30, VAAN.Qty_P31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.Qty_P.residplots <- imap(VAAN.Qty_Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/VAAN_QtyP_glm.pdf")
VAAN.Qty_P.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.Qty_P <- list("VAAN.Qty_P1" = VAAN.Qty_P1, "VAAN.Qty_P2" = VAAN.Qty_P2, "VAAN.Qty_P3" = VAAN.Qty_P3, "VAAN.Qty_P4" = VAAN.Qty_P4, "VAAN.Qty_P5" = VAAN.Qty_P5, "VAAN.Qty_P6" = VAAN.Qty_P6, "VAAN.Qty_P7" = VAAN.Qty_P7, "VAAN.Qty_P8" =  VAAN.Qty_P8, "VAAN.Qty_P9" = VAAN.Qty_P9, "VAAN.Qty_P10" = VAAN.Qty_P10, "VAAN.Qty_P11" = VAAN.Qty_P11, "VAAN.Qty_P12" = VAAN.Qty_P12, "VAAN.Qty_P13" = VAAN.Qty_P13, "VAAN.Qty_P14" = VAAN.Qty_P14, "VAAN.Qty_P15" = VAAN.Qty_P15, "VAAN.Qty_P16" = VAAN.Qty_P16, "VAAN.Qty_P17" = VAAN.Qty_P17, "VAAN.Qty_P18" = VAAN.Qty_P18, "VAAN.Qty_P19" = VAAN.Qty_P19, "VAAN.Qty_P20" = VAAN.Qty_P20, "VAAN.Qty_P21" = VAAN.Qty_P21, "VAAN.Qty_P22" = VAAN.Qty_P22, "VAAN.Qty_P23" = VAAN.Qty_P23, "VAAN.Qty_P24" =  VAAN.Qty_P24, "VAAN.Qty_P25" = VAAN.Qty_P25, "VAAN.Qty_P26" = VAAN.Qty_P26, "VAAN.Qty_P27" = VAAN.Qty_P27, "VAAN.Qty_P28" = VAAN.Qty_P28, "VAAN.Qty_P29" = VAAN.Qty_P29, "VAAN.Qty_P30" = VAAN.Qty_P30, "VAAN.Qty_P31" = VAAN.Qty_P31)
VAAN.Qty_P <- aictab(cand.set = Models.VAAN.Qty_P)
print(VAAN.Qty_P)
write.csv(VAAN.Qty_P, "output/AIC/VAAN_QtyP.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(VAAN.Qty_P3)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.Qty_P3, which = "Nagelkerke")
#Qty_N
VAAN.Qty_N1 <- glm(Qty_N ~ Year * EVI * GDD * NDMI * Site, data = VAAN)
VAAN.Qty_N2 <- glm(Qty_N ~ Year * EVI * GDD * NDMI, data = VAAN)
VAAN.Qty_N3 <- glm(Qty_N ~ Year * EVI * GDD * Site, data = VAAN)
VAAN.Qty_N4 <- glm(Qty_N ~ Year * EVI * Site * NDMI, data = VAAN)
VAAN.Qty_N5 <- glm(Qty_N ~ Year * Site * GDD * NDMI, data = VAAN) 
VAAN.Qty_N6 <- glm(Qty_N ~ Site * EVI * GDD * NDMI, data = VAAN) 
VAAN.Qty_N7 <- glm(Qty_N ~ Year * EVI * GDD, data = VAAN) 
VAAN.Qty_N8 <- glm(Qty_N ~ Year * EVI * NDMI, data = VAAN)
VAAN.Qty_N9 <- glm(Qty_N ~ Year * EVI * Site, data = VAAN)
VAAN.Qty_N10 <- glm(Qty_N ~ EVI * GDD * NDMI, data = VAAN)
VAAN.Qty_N11 <- glm(Qty_N ~ EVI * GDD * Site, data = VAAN)
VAAN.Qty_N12 <- glm(Qty_N ~ Year * GDD * NDMI, data = VAAN)
VAAN.Qty_N13 <- glm(Qty_N ~ Year * Site * GDD, data = VAAN)
VAAN.Qty_N14 <- glm(Qty_N ~ Year * NDMI * Site, data = VAAN)
VAAN.Qty_N15 <- glm(Qty_N ~ GDD * NDMI * Site, data = VAAN)
VAAN.Qty_N16 <- glm(Qty_N ~ Year * Site, data = VAAN)
VAAN.Qty_N17 <- glm(Qty_N ~ Year * EVI, data = VAAN)
VAAN.Qty_N18 <- glm(Qty_N ~ Year * NDMI, data = VAAN)
VAAN.Qty_N19 <- glm(Qty_N ~ Year * GDD, data = VAAN)
VAAN.Qty_N20 <- glm(Qty_N ~ EVI * Site, data = VAAN)
VAAN.Qty_N21 <- glm(Qty_N ~ NDMI * Site, data = VAAN)
VAAN.Qty_N22 <- glm(Qty_N ~ GDD * Site, data = VAAN)
VAAN.Qty_N23 <- glm(Qty_N ~ EVI * NDMI, data = VAAN)
VAAN.Qty_N24 <- glm(Qty_N ~ NDMI * GDD, data = VAAN)
VAAN.Qty_N25 <- glm(Qty_N ~ GDD * EVI, data = VAAN)
VAAN.Qty_N26 <- glm(Qty_N ~ Site, data = VAAN)
VAAN.Qty_N27 <- glm(Qty_N ~ NDMI, data = VAAN)
VAAN.Qty_N28 <- glm(Qty_N ~ GDD, data = VAAN)
VAAN.Qty_N29 <- glm(Qty_N ~ EVI, data = VAAN) 
VAAN.Qty_N30 <- glm(Qty_N ~ Year, data = VAAN) 
VAAN.Qty_N31 <- glm(Qty_N ~ 1, data =  VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Qty_Nmodels <- list(VAAN.Qty_N1, VAAN.Qty_N2, VAAN.Qty_N3, VAAN.Qty_N4, VAAN.Qty_N5, VAAN.Qty_N6, VAAN.Qty_N7, VAAN.Qty_N8, VAAN.Qty_N9, VAAN.Qty_N10, VAAN.Qty_N11, VAAN.Qty_N12, VAAN.Qty_N13, VAAN.Qty_N14, VAAN.Qty_N15, VAAN.Qty_N16, VAAN.Qty_N17, VAAN.Qty_N18, VAAN.Qty_N19, VAAN.Qty_N20, VAAN.Qty_N21, VAAN.Qty_N22, VAAN.Qty_N23, VAAN.Qty_N24, VAAN.Qty_N25, VAAN.Qty_N26, VAAN.Qty_N27, VAAN.Qty_N28, VAAN.Qty_N29, VAAN.Qty_N30, VAAN.Qty_N31)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.Qty_N.residplots <- imap(VAAN.Qty_Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels/ModelDiagnostics/VAAN_QtyN_glm.pdf")
VAAN.Qty_N.residplots
dev.off()

# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.Qty_N <- list("VAAN.Qty_N1" = VAAN.Qty_N1, "VAAN.Qty_N2" = VAAN.Qty_N2, "VAAN.Qty_N3" = VAAN.Qty_N3, "VAAN.Qty_N4" = VAAN.Qty_N4, "VAAN.Qty_N5" = VAAN.Qty_N5, "VAAN.Qty_N6" = VAAN.Qty_N6, "VAAN.Qty_N7" = VAAN.Qty_N7, "VAAN.Qty_N8" =  VAAN.Qty_N8, "VAAN.Qty_N9" = VAAN.Qty_N9, "VAAN.Qty_N10" = VAAN.Qty_N10, "VAAN.Qty_N11" = VAAN.Qty_N11, "VAAN.Qty_N12" = VAAN.Qty_N12, "VAAN.Qty_N13" = VAAN.Qty_N13, "VAAN.Qty_N14" = VAAN.Qty_N14, "VAAN.Qty_N15" = VAAN.Qty_N15, "VAAN.Qty_N16" = VAAN.Qty_N16, "VAAN.Qty_N17" = VAAN.Qty_N17, "VAAN.Qty_N18" = VAAN.Qty_N18, "VAAN.Qty_N19" = VAAN.Qty_N19, "VAAN.Qty_N20" = VAAN.Qty_N20, "VAAN.Qty_N21" = VAAN.Qty_N21, "VAAN.Qty_N22" = VAAN.Qty_N22, "VAAN.Qty_N23" = VAAN.Qty_N23, "VAAN.Qty_N24" =  VAAN.Qty_N24, "VAAN.Qty_N25" = VAAN.Qty_N25, "VAAN.Qty_N26" = VAAN.Qty_N26, "VAAN.Qty_N27" = VAAN.Qty_N27, "VAAN.Qty_N28" = VAAN.Qty_N28, "VAAN.Qty_N29" = VAAN.Qty_N29, "VAAN.Qty_N30" = VAAN.Qty_N30, "VAAN.Qty_N31" = VAAN.Qty_N31)
VAAN.Qty_N <- aictab(cand.set = Models.VAAN.Qty_N)
print(VAAN.Qty_N)
write.csv(VAAN.Qty_N, "output/AIC/VAAN_QtyN.csv")
# summary of the most parsimonious models - less than 2 delta AICc
summary(VAAN.Qty_N3)
summary(VAAN.Qty_N13)
summary(VAAN.Qty_N26)
summary(VAAN.Qty_N21)
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.Qty_N3, which = "Nagelkerke")
PseudoR2(VAAN.Qty_N13, which = "Nagelkerke")
PseudoR2(VAAN.Qty_N26, which = "Nagelkerke")
PseudoR2(VAAN.Qty_N21, which = "Nagelkerke")