# Author: Isabella Richmond
# Last edited: March 23, 2020

# This code was for the creation and evaluation of my temporal stoichiometry models. A lot of 
# code was provided by Travis Heckford (twitter.com/travheckford)
# Models evaluate the response of stoichiometry in four boreal plant species with year and site 
# If year is found to be in the top model when compared using AICc, another model is conducted 
# where mechanisms are investigated (productivity, site, moisture, weather)

#### Data Preparation ####
# load packages
install.packages("easypackages")
library(easypackages)
install_packages("sjPlot", "MuMIn", "purrr", "ggcorrplot", "purrr", "broom", "patchwork")
libraries("sjPlot", "purrr", "patchwork", "broom", "ggcorrplot", "ggplot2","dplyr", "tibble", "readr", "plyr", "ggpol", "ggpubr", "MuMIn", "AICcmodavg", "texreg", "kimisc", "psych", "DescTools")

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

### Models ###
# models are run individually for each species
# using AICc to evaluate all variables and combinations of models 
# fit models of c, N, P, Qty C, Qty N, Qty P, C:N, or C:P, and N:P as a function of covariates for each species
# evaluate with only Site and Year first, then if Year is in top model continue with 
# EVI, NDMI, GDD, etc. 

# ABBA
# % Carbon
ABBA.C1 <- glm(C ~ Year*Site, data = ABBA)
ABBA.C2 <- glm(C ~ Year, data = ABBA)
ABBA.C3 <- glm(C ~ Site, data = ABBA)
ABBA.C4 <- glm(C ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Cmodels <- list(ABBA.C1, ABBA.C2, ABBA.C3, ABBA.C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.C.residplots <- imap(ABBA.Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_C_glm.pdf")
ABBA.C.residplots
dev.off()
# if models pass assumptions, proceed. If not, use different error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.C <- list("ABBA.C1 = Year*Site" = ABBA.C1, "ABBA.C2 = Year" = ABBA.C2, "ABBA.C3 = Site" = ABBA.C3, "ABBA.C4 = Null" = ABBA.C4)
ABBA.C <- aictab(cand.set = Models.ABBA.C)
print(ABBA.C)
write.csv(ABBA.C, "output/AIC_2Step/ABBA_C.csv")
# save the summary tables of the models 
summary.ABBA.C <-map_df(Models.ABBA.C, broom::tidy, .id="model")
write_csv(summary.ABBA.C, path = "output/Summary_2Step/summary.ABBA.C.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.C1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.C.Global <- glm(C ~ EVI * GDD * NDMI * Site, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Set interaction limit to 
ABBA.C.mech <- dredge(ABBA.C.Global, evaluate = TRUE, rank = "AICc", m.lim = c(0,3))
print(ABBA.C.mech)
summary(get.models(ABBA.C.mech, 1)[[1]])
