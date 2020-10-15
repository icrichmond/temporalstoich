# Author: Isabella Richmond
# Last edited: October 14, 2020

# This code was for the creation and evaluation of my temporal stoichiometric ratio models. 
# The response variables are C:N, C:P, N:P of Balsam Fir, Red Maple, White Birch, and Lowbush Blueberry
# Models evaluate the response of stoichiometry in four boreal plant species with year and site 
# If year is found to be in the top model when compared using AICc, another model is conducted 
# where mechanisms are investigated (productivity, moisture, weather)

#### Data Preparation ####
# load packages
easypackages::libraries("MASS", "sjPlot", "purrr", "patchwork", "broom", "ggcorrplot", "ggplot2","dplyr", "tibble", "readr", "plyr", "ggpol", "ggpubr", "MuMIn", "AICcmodavg", "texreg", "kimisc", "psych", "DescTools")

# import datasets
stoich <- read_csv("input/Stoich_2016_2017.csv")
gdd <- read_csv("input/GDD_2016_2017_R.csv")
evi <- read_csv("input/EVI_2016_2017_R.csv")
ndmi <- read_csv("input/NDMI_2016_2017_R.csv")
# subset by year so that joining is possible 
stoich2016 <- subset(stoich, Year==2016)
stoich2017 <- subset(stoich, Year==2017)
gdd2016 <- subset(gdd, Year==2016)
gdd2017 <- subset(gdd, Year==2017)
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
  left_join(gdd2016, by=c("PlotName","Species")) %>%
  inner_join(evi2016,by="PlotName") %>%
  inner_join(ndmi2016, by="PlotName")
stoich2017 <- stoich2017 %>%
  left_join(gdd2017, by=c("PlotName","Species")) %>%
  inner_join(evi2017, by="PlotName")%>%
  inner_join(ndmi2017,by="PlotName")
# bind the 2016 and 2017 dataset back together 
stoich <- rbind(stoich2016,stoich2017)

# convert Year variable to factor (listed as integer)
stoich <- dplyr::rename(stoich, Year = Year.x)
stoich$Year <- as.factor(stoich$Year)
str(stoich)
stoich <- drop_na(stoich, GDD)

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

# test for correlation between covariates
# if r > 0.70, variables are highly correlated and should not be in the same models 
# compute a correlation matrix 
# isolate the explanatory variables 
abbacorrdata <- tibble(ABBA$GDD, ABBA$NDMI,ABBA$EVI)
abbacorr <- (cor(abbacorrdata))
ggcorrplot(abbacorr, hc.order = TRUE, lab = TRUE)
ggsave("graphics/Models/Correlations/ABBAcorr.jpg")

acrucorrdata <- tibble(ACRU$GDD, ACRU$NDMI,ACRU$EVI)
acrucorr <- (cor(acrucorrdata))
ggcorrplot(acrucorr, hc.order = TRUE, lab = TRUE)
ggsave("graphics/Models/Correlations/ACRUcorr.jpg")

bepacorrdata <- tibble(BEPA$GDD, BEPA$NDMI,BEPA$EVI)
bepacorr <- (cor(bepacorrdata))
ggcorrplot(bepacorr, hc.order = TRUE, lab = TRUE)
ggsave("graphics/Models/Correlations/BEPAcorr.jpg")

vaancorrdata <- tibble(VAAN$GDD, VAAN$NDMI,VAAN$EVI)
vaancorr <- (cor(vaancorrdata))
ggcorrplot(vaancorr, hc.order = TRUE, lab = TRUE)
ggsave("graphics/Models/Correlations/VAANcorr.jpg")
# no variables are highly correlated, all correlation values < 0.40

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
# C:N, or C:P, and N:P as a function of covariates for each species
# evaluate with only Site and Year first, then if Year is in top model continue with 
# EVI, NDMI, GDD, etc. 

# ABBA
# Carbon:Nitrogen
ABBA.CNRatio1 <- glm(CNRatio ~ Year*Site, data = ABBA)
ABBA.CNRatio2 <- glm(CNRatio ~ Year, data = ABBA)
ABBA.CNRatio3 <- glm(CNRatio ~ Site, data = ABBA)
ABBA.CNRatio4 <- glm(CNRatio ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.CNRatiomodels <- list(ABBA.CNRatio1, ABBA.CNRatio2, ABBA.CNRatio3, ABBA.CNRatio4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.CNRatio.residplots <- imap(ABBA.CNRatiomodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/Models/ModelDiagnostics/ABBA_CNRatio.pdf")
ABBA.CNRatio.residplots
dev.off()
# models meet assumptions - diagnostic plots look good
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.CNRatio <- list("ABBA.CNRatio1 = Year*Site" = ABBA.CNRatio1, "ABBA.CNRatio2 = Year" = ABBA.CNRatio2, "ABBA.CNRatio3 = Site" = ABBA.CNRatio3, "ABBA.CNRatio4 = Null" = ABBA.CNRatio4)
ABBA.CNRatio <- aictab(cand.set = Models.ABBA.CNRatio)
print(ABBA.CNRatio)
write.csv(ABBA.CNRatio, "output/AIC/ABBA_CN.csv")
# save the summary tables of the models 
summary.ABBA.CNRatio <-map_df(Models.ABBA.CNRatio, broom::tidy, .id="model")
write_csv(summary.ABBA.CNRatio, path = "output/Summary/ABBA_CN.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.CNRatio1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.CNRatio.Global <- glm(CNRatio ~ EVI + GDD + NDMI + EVI*GDD + EVI*NDMI + GDD*NDMI, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.CNRatio.mech <- dredge(ABBA.CNRatio.Global, evaluate = TRUE, rank = "AICc")
# check the residuals of the models to ensure that glm was correct choice 
ABBA.CNRatio.mechmodels <- get.models(ABBA.CNRatio.mech,subset=NA)
ABBA.CNRatio.mech.residplots <- imap(ABBA.CNRatio.mechmodels, resid_plots) 
pdf("graphics/Models/ModelDiagnostics/ABBA_CN_mech.pdf")
ABBA.CNRatio.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.CNRatio.mech)
# save the AIC table
write_csv(ABBA.CNRatio.mech, "output/AIC/ABBA_CN_mech.csv")
# visualize the AIC table 
pdf("graphics/Models/AIC/ABBA_CN.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.CNRatio.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.CNRatio.mechtop <- (get.models(ABBA.CNRatio.mech, 1)[[1]])
ABBA.CNRatio.mechtop <- tidy(ABBA.CNRatio.mechtop)
write_csv(ABBA.CNRatio.mechtop, "output/Summary/ABBA_CN_mech.csv")
PseudoR2(ABBA.CNRatio.mechtop, which = "Nagelkerke")
# investigate for pretending variables as per Leroux 2019
# use AIC table to identify potential pretending variabels
# for any suspected pretending variables, look at the confidence intervals of each 
# model and parameter estimate. If they cross zero and fulfill all other criteria, 
# they are probably a pretending variable
for (i in ABBA.CNRatio.mechmodels) { 
  print(tidy(i, conf.int=TRUE))
}
# EVI*GDD and GDD*NDMI seem to be pretending variables in the top models, remove and rerun
ABBA.CNRatio.Global.pretend <- glm(CNRatio ~ EVI + GDD + NDMI + EVI*NDMI, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.CNRatio.mech.pretend <- dredge(ABBA.CNRatio.Global.pretend, evaluate = TRUE, rank = "AICc")
# check the residuals of the models to ensure that glm was correct choice 
ABBA.CNRatio.mechmodels.pretend <- get.models(ABBA.CNRatio.mech.pretend,subset=NA)
ABBA.CNRatio.mech.residplots.pretend <- imap(ABBA.CNRatio.mechmodels.pretend, resid_plots) 
pdf("graphics/Models/ModelDiagnostics/ABBA_CN_mech_pretend.pdf")
ABBA.CNRatio.mech.residplots.pretend
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.CNRatio.mech.pretend)
# save the AIC table
write_csv(ABBA.CNRatio.mech.pretend, "output/AIC/ABBA_CN_mech_pretend.csv")
# visualize the AIC table 
pdf("graphics/Models/AIC/ABBA_CN_pretend.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.CNRatio.mech.pretend)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.CNRatio.mechtop.pretend <- (get.models(ABBA.CNRatio.mech.pretend, 1)[[1]])
ABBA.CNRatio.mechtop.pretend <- tidy(ABBA.CNRatio.mechtop.pretend, conf.int=TRUE)
write_csv(ABBA.CNRatio.mechtop.pretend, "output/Summary/ABBA_CN_mech_pretend.csv")
PseudoR2(ABBA.CNRatio.mechtop.pretend, which = "Nagelkerke")

# Carbon:Phosphorus
ABBA.CPRatio1 <- glm(CPRatio ~ Year*Site, data = ABBA)
ABBA.CPRatio2 <- glm(CPRatio ~ Year, data = ABBA)
ABBA.CPRatio3 <- glm(CPRatio ~ Site, data = ABBA)
ABBA.CPRatio4 <- glm(CPRatio ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.CPRatiomodels <- list(ABBA.CPRatio1, ABBA.CPRatio2, ABBA.CPRatio3, ABBA.CPRatio4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.CPRatio.residplots <- imap(ABBA.CPRatiomodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/Models/ModelDiagnostics/ABBA_CP.pdf")
ABBA.CPRatio.residplots
dev.off()
# models pass assumptions, diagnostic plots look good 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.CPRatio <- list("ABBA.CPRatio1 = Year*Site" = ABBA.CPRatio1, "ABBA.CPRatio2 = Year" = ABBA.CPRatio2, "ABBA.CPRatio3 = Site" = ABBA.CPRatio3, "ABBA.CPRatio4 = Null" = ABBA.CPRatio4)
ABBA.CPRatio <- aictab(cand.set = Models.ABBA.CPRatio)
print(ABBA.CPRatio)
write.csv(ABBA.CPRatio, "output/AIC/ABBA_CP.csv")
# save the summary tables of the models 
summary.ABBA.CPRatio <-map_df(Models.ABBA.CPRatio, broom::tidy, .id="model")
write_csv(summary.ABBA.CPRatio, path = "output/Summary/ABBA_CP.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.CPRatio1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.CPRatio.Global <- glm(CPRatio ~EVI + GDD + NDMI + EVI*GDD + EVI*NDMI + GDD*NDMI, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.CPRatio.mech <- dredge(ABBA.CPRatio.Global, evaluate = TRUE, rank = "AICc")
# check the residuals of the models to ensure that glm was correct choice 
ABBA.CPRatio.mechmodels <- get.models(ABBA.CPRatio.mech,subset=NA)
ABBA.CPRatio.mech.residplots <- imap(ABBA.CPRatio.mechmodels, resid_plots) 
pdf("graphics/Models/ModelDiagnostics/ABBA_CP_mech.pdf")
ABBA.CPRatio.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.CPRatio.mech)
# save the AIC table
write_csv(ABBA.CPRatio.mech, "output/AIC/ABBA_CP_mech.csv")
# visualize the AIC table 
pdf("graphics/Models/AIC/ABBA_CP.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.CPRatio.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.CPRatio.mechtop <- (get.models(ABBA.CPRatio.mech, 1)[[1]])
ABBA.CPRatio.mechtop <- tidy(ABBA.CPRatio.mechtop)
write_csv(ABBA.CPRatio.mechtop, "output/Summary/ABBA_CP_mech.csv")
PseudoR2(ABBA.CPRatio.mechtop, which = "Nagelkerke")
# investigate for pretending variables as per Leroux 2019
# use AIC table to identify potential pretending variabels
# for any suspected pretending variables, look at the confidence intervals of each 
# model and parameter estimate. If they cross zero and fulfill all other criteria, 
# they are probably a pretending variable
for (i in ABBA.CPRatio.mechmodels) { 
  print(tidy(i, conf.int=TRUE))
}
# EVI*GDD seems to be a pretending variables in the top models, remove and rerun
ABBA.CPRatio.Global.pretend <- glm(CPRatio ~ EVI + GDD + NDMI + EVI*NDMI + GDD*NDMI, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.CPRatio.mech.pretend <- dredge(ABBA.CPRatio.Global.pretend, evaluate = TRUE, rank = "AICc")
# check the residuals of the models to ensure that glm was correct choice 
ABBA.CPRatio.mechmodels.pretend <- get.models(ABBA.CPRatio.mech.pretend,subset=NA)
ABBA.CPRatio.mech.residplots.pretend <- imap(ABBA.CPRatio.mechmodels.pretend, resid_plots) 
pdf("graphics/Models/ModelDiagnostics_GzLM/ABBA_CP_mech_pretend.pdf")
ABBA.CPRatio.mech.residplots.pretend
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.CPRatio.mech.pretend)
# save the AIC table
write_csv(ABBA.CPRatio.mech.pretend, "output/AIC/ABBA_CP_mech_pretend.csv")
# visualize the AIC table 
pdf("graphics/Models/AIC/ABBA_CP_pretend.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.CPRatio.mech.pretend)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.CPRatio.mechtop.pretend <- (get.models(ABBA.CPRatio.mech.pretend, 1)[[1]])
ABBA.CPRatio.mechtop.pretend <- tidy(ABBA.CPRatio.mechtop.pretend, conf.int=TRUE)
write_csv(ABBA.CPRatio.mechtop.pretend, "output/Summary/ABBA_CP_mech_pretend.csv")
PseudoR2(ABBA.CPRatio.mechtop.pretend, which = "Nagelkerke")

# Nitrogen:Phosphorus
ABBA.NPRatio1 <- glm(NPRatio ~ Year*Site, data = ABBA)
ABBA.NPRatio2 <- glm(NPRatio ~ Year, data = ABBA)
ABBA.NPRatio3 <- glm(NPRatio ~ Site, data = ABBA)
ABBA.NPRatio4 <- glm(NPRatio ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.NPRatiomodels <- list(ABBA.NPRatio1, ABBA.NPRatio2, ABBA.NPRatio3, ABBA.NPRatio4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.NPRatio.residplots <- imap(ABBA.NPRatiomodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/Models/ModelDiagnostics/ABBA_NP.pdf")
ABBA.NPRatio.residplots
dev.off()
# if models pass assumptions, proceed. If not, use different error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.NPRatio <- list("ABBA.NPRatio1 = Year*Site" = ABBA.NPRatio1, "ABBA.NPRatio2 = Year" = ABBA.NPRatio2, "ABBA.NPRatio3 = Site" = ABBA.NPRatio3, "ABBA.NPRatio4 = Null" = ABBA.NPRatio4)
ABBA.NPRatio <- aictab(cand.set = Models.ABBA.NPRatio)
print(ABBA.NPRatio)
write.csv(ABBA.NPRatio, "output/AIC/ABBA_NP.csv")
# save the summary tables of the models 
summary.ABBA.NPRatio <-map_df(Models.ABBA.NPRatio, broom::tidy, .id="model")
write_csv(summary.ABBA.NPRatio, path = "output/Summary/ABBA_NP.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.NPRatio1, which = "Nagelkerke")
# site was the top model - because year is not a top variable

# ACRU
# Carbon:Nitrogen
ACRU.CNRatio1 <- glm(CNRatio ~ Year*Site, data = ACRU)
ACRU.CNRatio2 <- glm(CNRatio ~ Year, data = ACRU)
ACRU.CNRatio3 <- glm(CNRatio ~ Site, data = ACRU)
ACRU.CNRatio4 <- glm(CNRatio ~ 1, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.CNRatiomodels <- list(ACRU.CNRatio1, ACRU.CNRatio2, ACRU.CNRatio3, ACRU.CNRatio4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.CNRatio.residplots <- imap(ACRU.CNRatiomodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/Models/ModelDiagnostics/ACRU_CN.pdf")
ACRU.CNRatio.residplots
dev.off()
# diagnostic plots look good
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.CNRatio <- list("ACRU.CNRatio1 = Year*Site" = ACRU.CNRatio1, "ACRU.CNRatio2 = Year" = ACRU.CNRatio2, "ACRU.CNRatio3 = Site" = ACRU.CNRatio3, "ACRU.CNRatio4 = Null" = ACRU.CNRatio4)
ACRU.CNRatio <- aictab(cand.set = Models.ACRU.CNRatio)
print(ACRU.CNRatio)
write.csv(ACRU.CNRatio, "output/AIC/ACRU_CN.csv")
# save the summary tables of the models 
summary.ACRU.CNRatio <-map_df(Models.ACRU.CNRatio, broom::tidy, .id="model")
write_csv(summary.ACRU.CNRatio, path = "output/Summary/ACRU_CN.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.CNRatio1, which = "Nagelkerke")
# Null model is the top model, stop here.

# Carbon:Phosphorus
ACRU.CPRatio1 <- glm(CPRatio ~ Year*Site, data = ACRU)
ACRU.CPRatio2 <- glm(CPRatio ~ Year, data = ACRU)
ACRU.CPRatio3 <- glm(CPRatio ~ Site, data = ACRU)
ACRU.CPRatio4 <- glm(CPRatio ~ 1, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.CPRatiomodels <- list(ACRU.CPRatio1, ACRU.CPRatio2, ACRU.CPRatio3, ACRU.CPRatio4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.CPRatio.residplots <- imap(ACRU.CPRatiomodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/Models/ModelDiagnostics/ACRU_CP.pdf")
ACRU.CPRatio.residplots
dev.off()
# model diagnostics look good, assumptions are met. 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.CPRatio <- list("ACRU.CPRatio1 = Year*Site" = ACRU.CPRatio1, "ACRU.CPRatio2 = Year" = ACRU.CPRatio2, "ACRU.CPRatio3 = Site" = ACRU.CPRatio3, "ACRU.CPRatio4 = Null" = ACRU.CPRatio4)
ACRU.CPRatio <- aictab(cand.set = Models.ACRU.CPRatio)
print(ACRU.CPRatio)
write.csv(ACRU.CPRatio, "output/AIC/ACRU_CP.csv")
# save the summary tables of the models 
summary.ACRU.CPRatio <-map_df(Models.ACRU.CPRatio, broom::tidy, .id="model")
write_csv(summary.ACRU.CPRatio, path = "output/Summary/ACRU_CP.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.CPRatio1, which = "Nagelkerke")
# Null is top model, stop here.

# Nitrogen:Phosphorus
ACRU.NPRatio1 <- glm(NPRatio ~ Year*Site, data = ACRU)
ACRU.NPRatio2 <- glm(NPRatio ~ Year, data = ACRU)
ACRU.NPRatio3 <- glm(NPRatio ~ Site, data = ACRU)
ACRU.NPRatio4 <- glm(NPRatio ~ 1, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.NPRatiomodels <- list(ACRU.NPRatio1, ACRU.NPRatio2, ACRU.NPRatio3, ACRU.NPRatio4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.NPRatio.residplots <- imap(ACRU.NPRatiomodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/Models/ModelDiagnostics/ACRU_NP.pdf")
ACRU.NPRatio.residplots
dev.off()
# model diagnostics look good, assumptions are met
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.NPRatio <- list("ACRU.NPRatio1 = Year*Site" = ACRU.NPRatio1, "ACRU.NPRatio2 = Year" = ACRU.NPRatio2, "ACRU.NPRatio3 = Site" = ACRU.NPRatio3, "ACRU.NPRatio4 = Null" = ACRU.NPRatio4)
ACRU.NPRatio <- aictab(cand.set = Models.ACRU.NPRatio)
print(ACRU.NPRatio)
write.csv(ACRU.NPRatio, "output/AIC/ACRU_NP.csv")
# save the summary tables of the models 
summary.ACRU.NPRatio <-map_df(Models.ACRU.NPRatio, broom::tidy, .id="model")
write_csv(summary.ACRU.NPRatio, path = "output/Summary/ACRU_NP.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.NPRatio1, which = "Nagelkerke")
# Null is the top model, stop here.

# BEPA
# Carbon:Nitrogen
BEPA.CNRatio1 <- glm(CNRatio ~ Year*Site, data = BEPA)
BEPA.CNRatio2 <- glm(CNRatio ~ Year, data = BEPA)
BEPA.CNRatio3 <- glm(CNRatio ~ Site, data = BEPA)
BEPA.CNRatio4 <- glm(CNRatio ~ 1, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.CNRatiomodels <- list(BEPA.CNRatio1, BEPA.CNRatio2, BEPA.CNRatio3, BEPA.CNRatio4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.CNRatio.residplots <- imap(BEPA.CNRatiomodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/Models/ModelDiagnostics/BEPA_CN.pdf")
BEPA.CNRatio.residplots
dev.off()
# diagnostic plots are ok, could try another error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.CNRatio <- list("BEPA.CNRatio1 = Year*Site" = BEPA.CNRatio1, "BEPA.CNRatio2 = Year" = BEPA.CNRatio2, "BEPA.CNRatio3 = Site" = BEPA.CNRatio3, "BEPA.CNRatio4 = Null" = BEPA.CNRatio4)
BEPA.CNRatio <- aictab(cand.set = Models.BEPA.CNRatio)
print(BEPA.CNRatio)
write.csv(BEPA.CNRatio, "output/AIC/BEPA_CN.csv")
# save the summary tables of the models 
summary.BEPA.CNRatio <-map_df(Models.BEPA.CNRatio, broom::tidy, .id="model")
write_csv(summary.BEPA.CNRatio, path = "output/Summary/BEPA_CN.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.CNRatio1, which = "Nagelkerke")
# Null is top model, stop here. Could potentially be due to bad fit - try different error structure just to be sure?

# Carbon:Phosphorus
BEPA.CPRatio1 <- glm(CPRatio ~ Year*Site, data = BEPA)
BEPA.CPRatio2 <- glm(CPRatio ~ Year, data = BEPA)
BEPA.CPRatio3 <- glm(CPRatio ~ Site, data = BEPA)
BEPA.CPRatio4 <- glm(CPRatio ~ 1, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.CPRatiomodels <- list(BEPA.CPRatio1, BEPA.CPRatio2, BEPA.CPRatio3, BEPA.CPRatio4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.CPRatio.residplots <- imap(BEPA.CPRatiomodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/Models/ModelDiagnostics/BEPA_CP.pdf")
BEPA.CPRatio.residplots
dev.off()
# diagnostic plots are ok, could try another error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.CPRatio <- list("BEPA.CPRatio1 = Year*Site" = BEPA.CPRatio1, "BEPA.CPRatio2 = Year" = BEPA.CPRatio2, "BEPA.CPRatio3 = Site" = BEPA.CPRatio3, "BEPA.CPRatio4 = Null" = BEPA.CPRatio4)
BEPA.CPRatio <- aictab(cand.set = Models.BEPA.CPRatio)
print(BEPA.CPRatio)
write.csv(BEPA.CPRatio, "output/AIC_2Step/BEPA_CPRatio.csv")
# save the summary tables of the models 
summary.BEPA.CPRatio <-map_df(Models.BEPA.CPRatio, broom::tidy, .id="model")
write_csv(summary.BEPA.CPRatio, path = "output/Summary/BEPA_CP.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.CPRatio1, which = "Nagelkerke")
# Null is top model, stop here. Could be due to bad fit, try another error structure

# Nitrogen:Phosphorus
BEPA.NPRatio1 <- glm(NPRatio ~ Year*Site, data = BEPA)
BEPA.NPRatio2 <- glm(NPRatio ~ Year, data = BEPA)
BEPA.NPRatio3 <- glm(NPRatio ~ Site, data = BEPA)
BEPA.NPRatio4 <- glm(NPRatio ~ 1, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.NPRatiomodels <- list(BEPA.NPRatio1, BEPA.NPRatio2, BEPA.NPRatio3, BEPA.NPRatio4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.NPRatio.residplots <- imap(BEPA.NPRatiomodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/Models/ModelDiagnostics/BEPA_NP.pdf")
BEPA.NPRatio.residplots
dev.off()
# diagnostic plots look good, assumptions are met 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.NPRatio <- list("BEPA.NPRatio1 = Year*Site" = BEPA.NPRatio1, "BEPA.NPRatio2 = Year" = BEPA.NPRatio2, "BEPA.NPRatio3 = Site" = BEPA.NPRatio3, "BEPA.NPRatio4 = Null" = BEPA.NPRatio4)
BEPA.NPRatio <- aictab(cand.set = Models.BEPA.NPRatio)
print(BEPA.NPRatio)
write.csv(BEPA.NPRatio, "output/AIC/BEPA_NP.csv")
# save the summary tables of the models 
summary.BEPA.NPRatio <-map_df(Models.BEPA.NPRatio, broom::tidy, .id="model")
write_csv(summary.BEPA.NPRatio, path = "output/Summary/BEPA_NP.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.NPRatio1, which = "Nagelkerke")
# Null was the top model, stop here

# VAAN
# Carbon:Nitrogen
VAAN.CNRatio1 <- glm(CNRatio ~ Year*Site, data = VAAN)
VAAN.CNRatio2 <- glm(CNRatio ~ Year, data = VAAN)
VAAN.CNRatio3 <- glm(CNRatio ~ Site, data = VAAN)
VAAN.CNRatio4 <- glm(CNRatio ~ 1, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.CNRatiomodels <- list(VAAN.CNRatio1, VAAN.CNRatio2, VAAN.CNRatio3, VAAN.CNRatio4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.CNRatio.residplots <- imap(VAAN.CNRatiomodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/Models/ModelDiagnostics/VAAN_CN.pdf")
VAAN.CNRatio.residplots
dev.off()
# diagnostic plots look good, assumptions are met
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.CNRatio <- list("VAAN.CNRatio1 = Year*Site" = VAAN.CNRatio1, "VAAN.CNRatio2 = Year" = VAAN.CNRatio2, "VAAN.CNRatio3 = Site" = VAAN.CNRatio3, "VAAN.CNRatio4 = Null" = VAAN.CNRatio4)
VAAN.CNRatio <- aictab(cand.set = Models.VAAN.CNRatio)
print(VAAN.CNRatio)
write.csv(VAAN.CNRatio, "output/AIC/VAAN_CN.csv")
# save the summary tables of the models 
summary.VAAN.CNRatio <-map_df(Models.VAAN.CNRatio, broom::tidy, .id="model")
write_csv(summary.VAAN.CNRatio, path = "output/Summary/VAAN_CN.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.CNRatio1, which = "Nagelkerke")
# top model has only Site, stop here.

# Carbon:Phosphorus
VAAN.CPRatio1 <- glm(CPRatio ~ Year*Site, data = VAAN)
VAAN.CPRatio2 <- glm(CPRatio ~ Year, data = VAAN)
VAAN.CPRatio3 <- glm(CPRatio ~ Site, data = VAAN)
VAAN.CPRatio4 <- glm(CPRatio ~ 1, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.CPRatiomodels <- list(VAAN.CPRatio1, VAAN.CPRatio2, VAAN.CPRatio3, VAAN.CPRatio4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.CPRatio.residplots <- imap(VAAN.CPRatiomodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/Models/ModelDiagnostics/VAAN_CP.pdf")
VAAN.CPRatio.residplots
dev.off()
# diagnostic plots look good, assumptions are met 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.CPRatio <- list("VAAN.CPRatio1 = Year*Site" = VAAN.CPRatio1, "VAAN.CPRatio2 = Year" = VAAN.CPRatio2, "VAAN.CPRatio3 = Site" = VAAN.CPRatio3, "VAAN.CPRatio4 = Null" = VAAN.CPRatio4)
VAAN.CPRatio <- aictab(cand.set = Models.VAAN.CPRatio)
print(VAAN.CPRatio)
write.csv(VAAN.CPRatio, "output/AIC/VAAN_CP.csv")
# save the summary tables of the models 
summary.VAAN.CPRatio <-map_df(Models.VAAN.CPRatio, broom::tidy, .id="model")
write_csv(summary.VAAN.CPRatio, path = "output/Summary/VAAN_CP.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.CPRatio1, which = "Nagelkerke")
# top model has only Site, stop here.

# Nitrogen:Phosphorus
VAAN.NPRatio1 <- glm(NPRatio ~ Year*Site, data = VAAN)
VAAN.NPRatio2 <- glm(NPRatio ~ Year, data = VAAN)
VAAN.NPRatio3 <- glm(NPRatio ~ Site, data = VAAN)
VAAN.NPRatio4 <- glm(NPRatio ~ 1, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.NPRatiomodels <- list(VAAN.NPRatio1, VAAN.NPRatio2, VAAN.NPRatio3, VAAN.NPRatio4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.NPRatio.residplots <- imap(VAAN.NPRatiomodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/Models/ModelDiagnostics/VAAN_NP.pdf")
VAAN.NPRatio.residplots
dev.off()
# diagnostic plots are okay, assumptions are met. Could use different error structure to check. 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.NPRatio <- list("VAAN.NPRatio1 = Year*Site" = VAAN.NPRatio1, "VAAN.NPRatio2 = Year" = VAAN.NPRatio2, "VAAN.NPRatio3 = Site" = VAAN.NPRatio3, "VAAN.NPRatio4 = Null" = VAAN.NPRatio4)
VAAN.NPRatio <- aictab(cand.set = Models.VAAN.NPRatio)
print(VAAN.NPRatio)
write.csv(VAAN.NPRatio, "output/AIC/VAAN_NP.csv")
# save the summary tables of the models 
summary.VAAN.NPRatio <-map_df(Models.VAAN.NPRatio, broom::tidy, .id="model")
write_csv(summary.VAAN.NPRatio, path = "output/Summary/VAAN_NP.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.NPRatio1, which = "Nagelkerke")
# top model has only Site, stop here.