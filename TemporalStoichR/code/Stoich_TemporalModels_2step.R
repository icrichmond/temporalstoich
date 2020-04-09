# Author: Isabella Richmond
# Last edited: April 3, 2020

# This code was for the creation and evaluation of my temporal stoichiometry models. A lot of 
# code was provided by Travis Heckford (twitter.com/travheckford)
# Models evaluate the response of stoichiometry in four boreal plant species with year and site 
# If year is found to be in the top model when compared using AICc, another model is conducted 
# where mechanisms are investigated (productivity, moisture, weather)

#### Data Preparation ####
# load packages
install.packages("easypackages")
library(easypackages)
install_packages("sjPlot", "MuMIn", "purrr", "ggcorrplot", "purrr", "broom", "patchwork")
libraries("MASS", "sjPlot", "purrr", "patchwork", "broom", "ggcorrplot", "ggplot2","dplyr", "tibble", "readr", "plyr", "ggpol", "ggpubr", "MuMIn", "AICcmodavg", "texreg", "kimisc", "psych", "DescTools")

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

# test for correlation between covariates
# if r > 0.70, variables are highly correlated and should not be in the same models 
# compute a correlation matrix 
# isolate the explanatory variables 
abbacorrdata <- tibble(ABBA$GDD, ABBA$NDMI,ABBA$EVI)
abbacorr <- (cor(abbacorrdata))
ggcorrplot(abbacorr, hc.order = TRUE, lab = TRUE)
ggsave("graphics/StoichModels_2Step/Correlations/ABBAcorr.jpg")

acrucorrdata <- tibble(ACRU$GDD, ACRU$NDMI,ACRU$EVI)
acrucorr <- (cor(acrucorrdata))
ggcorrplot(acrucorr, hc.order = TRUE, lab = TRUE)
ggsave("graphics/StoichModels_2Step/Correlations/ACRUcorr.jpg")

bepacorrdata <- tibble(BEPA$GDD, BEPA$NDMI,BEPA$EVI)
bepacorr <- (cor(bepacorrdata))
ggcorrplot(bepacorr, hc.order = TRUE, lab = TRUE)
ggsave("graphics/StoichModels_2Step/Correlations/BEPAcorr.jpg")

vaancorrdata <- tibble(VAAN$GDD, VAAN$NDMI,VAAN$EVI)
vaancorr <- (cor(vaancorrdata))
ggcorrplot(vaancorr, hc.order = TRUE, lab = TRUE)
ggsave("graphics/StoichModels_2Step/Correlations/VAANcorr.jpg")
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
# assumptions are ok, could try another error structure to check
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
ABBA.C.Global <- glm(C ~ EVI * GDD * NDMI, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.C.mech <- dredge(ABBA.C.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI))
# check the residuals of the models to ensure that glm was correct choice 
ABBA.C.mechmodels <- get.models(ABBA.C.mech,subset=NA)
ABBA.C.mech.residplots <- imap(ABBA.C.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_C_mech_glm.pdf")
ABBA.C.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.C.mech)
# save the AIC table
write_csv(ABBA.C.mech, "output/AIC_2Step/ABBA_C_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ABBA.C.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.C.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.C.mechtop <- (get.models(ABBA.C.mech, 1)[[1]])
ABBA.C.mechtop
ABBA.C.mechtop <- tidy(ABBA.C.mechtop)
write_csv(ABBA.C.mechtop, "output/Summary_2Step/summary.ABBA.C.mech.csv")

# % Nitrogen
ABBA.N1 <- glm(N ~ Year*Site, data = ABBA)
ABBA.N2 <- glm(N ~ Year, data = ABBA)
ABBA.N3 <- glm(N ~ Site, data = ABBA)
ABBA.N4 <- glm(N ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Nmodels <- list(ABBA.N1, ABBA.N2, ABBA.N3, ABBA.N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.N.residplots <- imap(ABBA.Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_N_glm.pdf")
ABBA.N.residplots
dev.off()
# assumptions are ok, could use another error structure to check
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.N <- list("ABBA.N1 = Year*Site" = ABBA.N1, "ABBA.N2 = Year" = ABBA.N2, "ABBA.N3 = Site" = ABBA.N3, "ABBA.N4 = Null" = ABBA.N4)
ABBA.N <- aictab(cand.set = Models.ABBA.N)
print(ABBA.N)
write.csv(ABBA.N, "output/AIC_2Step/ABBA_N.csv")
# save the summary tables of the models 
summary.ABBA.N <-map_df(Models.ABBA.N, broom::tidy, .id="model")
write_csv(summary.ABBA.N, path = "output/Summary_2Step/summary.ABBA.N.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.N1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.N.Global <- glm(N ~ EVI * GDD * NDMI, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.N.mech <- dredge(ABBA.N.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI))
# check the residuals of the models to ensure that glm was correct choice 
ABBA.N.mechmodels <- get.models(ABBA.N.mech,subset=NA)
ABBA.N.mech.residplots <- imap(ABBA.N.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_N_mech_glm.pdf")
ABBA.N.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.N.mech)
# save the AIC table
write_csv(ABBA.N.mech, "output/AIC_2Step/ABBA_N_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ABBA.N.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.N.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.N.mechtop <- (get.models(ABBA.N.mech, 1)[[1]])
ABBA.N.mechtop <- tidy(ABBA.N.mechtop)
ABBA.N.mechtop
write_csv(ABBA.N.mechtop, "output/Summary_2Step/summary.ABBA.N.mech.csv")
# % Phosphorus
ABBA.P1 <- glm(P ~ Year*Site, data = ABBA)
ABBA.P2 <- glm(P ~ Year, data = ABBA)
ABBA.P3 <- glm(P ~ Site, data = ABBA)
ABBA.P4 <- glm(P ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Pmodels <- list(ABBA.P1, ABBA.P2, ABBA.P3, ABBA.P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.P.residplots <- imap(ABBA.Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_P_glm.pdf")
ABBA.P.residplots
dev.off()
# assumptions are ok, would use another error structure to check
# create an AIPc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.P <- list("ABBA.P1 = Year*Site" = ABBA.P1, "ABBA.P2 = Year" = ABBA.P2, "ABBA.P3 = Site" = ABBA.P3, "ABBA.P4 = Null" = ABBA.P4)
ABBA.P <- aictab(cand.set = Models.ABBA.P)
print(ABBA.P)
write.csv(ABBA.P, "output/AIC_2Step/ABBA_P.csv")
# save the summary tables of the models 
summary.ABBA.P <-map_df(Models.ABBA.P, broom::tidy, .id="model")
write_csv(summary.ABBA.P, path = "output/Summary_2Step/summary.ABBA.P.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.P1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.P.Global <- glm(P ~ EVI * GDD * NDMI, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.P.mech <- dredge(ABBA.P.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI))
# check the residuals of the models to ensure that glm was correct choice 
ABBA.P.mechmodels <- get.models(ABBA.P.mech,subset=NA)
ABBA.P.mech.residplots <- imap(ABBA.P.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_P_mech_glm.pdf")
ABBA.P.mech.residplots
dev.off()
# if assumptions are met, proceed with AIP table and analysis
# look at the AIP table
print(ABBA.P.mech)
# save the AIC table
write_csv(ABBA.P.mech, "output/AIC_2Step/ABBA_P_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ABBA.P.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.P.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.P.mechtop <- (get.models(ABBA.P.mech, 1)[[1]])
ABBA.P.mechtop <- tidy(ABBA.P.mechtop)
ABBA.P.mechtop
write_csv(ABBA.P.mechtop, "output/Summary_2Step/summary.ABBA.P.mech.csv")

# Carbon (g)
ABBA.Qty_C1 <- glm(Qty_C ~ Year*Site, data = ABBA)
ABBA.Qty_C2 <- glm(Qty_C ~ Year, data = ABBA)
ABBA.Qty_C3 <- glm(Qty_C ~ Site, data = ABBA)
ABBA.Qty_C4 <- glm(Qty_C ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Qty_Cmodels <- list(ABBA.Qty_C1, ABBA.Qty_C2, ABBA.Qty_C3, ABBA.Qty_C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.Qty_C.residplots <- imap(ABBA.Qty_Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_Qty_C_glm.pdf")
ABBA.Qty_C.residplots
dev.off()
# models do not meet assumptions - really bad fit. Will have to try a different error structure.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.Qty_C <- list("ABBA.Qty_C1 = Year*Site" = ABBA.Qty_C1, "ABBA.Qty_C2 = Year" = ABBA.Qty_C2, "ABBA.Qty_C3 = Site" = ABBA.Qty_C3, "ABBA.Qty_C4 = Null" = ABBA.Qty_C4)
ABBA.Qty_C <- aictab(cand.set = Models.ABBA.Qty_C)
print(ABBA.Qty_C)
write.csv(ABBA.Qty_C, "output/AIC_2Step/ABBA_Qty_C.csv")
# save the summary tables of the models 
summary.ABBA.Qty_C <-map_df(Models.ABBA.Qty_C, broom::tidy, .id="model")
write_csv(summary.ABBA.Qty_C, path = "output/Summary_2Step/summary.ABBA.Qty_C.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.Qty_C1, which = "Nagelkerke")
# top model was Null - stop here. Probably due to poor fit. 

# Nitrogen (g)
ABBA.Qty_N1 <- glm(Qty_N ~ Year*Site, data = ABBA)
ABBA.Qty_N2 <- glm(Qty_N ~ Year, data = ABBA)
ABBA.Qty_N3 <- glm(Qty_N ~ Site, data = ABBA)
ABBA.Qty_N4 <- glm(Qty_N ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Qty_Nmodels <- list(ABBA.Qty_N1, ABBA.Qty_N2, ABBA.Qty_N3, ABBA.Qty_N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.Qty_N.residplots <- imap(ABBA.Qty_Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_Qty_N_glm.pdf")
ABBA.Qty_N.residplots
dev.off()
# models do not meet assumptions - really bad fit. Will have to try a different error structure.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.Qty_N <- list("ABBA.Qty_N1 = Year*Site" = ABBA.Qty_N1, "ABBA.Qty_N2 = Year" = ABBA.Qty_N2, "ABBA.Qty_N3 = Site" = ABBA.Qty_N3, "ABBA.Qty_N4 = Null" = ABBA.Qty_N4)
ABBA.Qty_N <- aictab(cand.set = Models.ABBA.Qty_N)
print(ABBA.Qty_N)
write.csv(ABBA.Qty_N, "output/AIC_2Step/ABBA_Qty_N.csv")
# save the summary tables of the models 
summary.ABBA.Qty_N <-map_df(Models.ABBA.Qty_N, broom::tidy, .id="model")
write_csv(summary.ABBA.Qty_N, path = "output/Summary_2Step/summary.ABBA.Qty_N.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.Qty_N1, which = "Nagelkerke")
# top model was Null - stop here. Probably due to poor fit. 

# Phosphorus (g)
ABBA.Qty_P1 <- glm(Qty_P ~ Year*Site, data = ABBA)
ABBA.Qty_P2 <- glm(Qty_P ~ Year, data = ABBA)
ABBA.Qty_P3 <- glm(Qty_P ~ Site, data = ABBA)
ABBA.Qty_P4 <- glm(Qty_P ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Qty_Pmodels <- list(ABBA.Qty_P1, ABBA.Qty_P2, ABBA.Qty_P3, ABBA.Qty_P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.Qty_P.residplots <- imap(ABBA.Qty_Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_Qty_P_glm.pdf")
ABBA.Qty_P.residplots
dev.off()
# models do not meet assumptions - really bad fit. Will have to try a different error structure.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.Qty_P <- list("ABBA.Qty_P1 = Year*Site" = ABBA.Qty_P1, "ABBA.Qty_P2 = Year" = ABBA.Qty_P2, "ABBA.Qty_P3 = Site" = ABBA.Qty_P3, "ABBA.Qty_P4 = Null" = ABBA.Qty_P4)
ABBA.Qty_P <- aictab(cand.set = Models.ABBA.Qty_P)
print(ABBA.Qty_P)
write.csv(ABBA.Qty_P, "output/AIC_2Step/ABBA_Qty_P.csv")
# save the summary tables of the models 
summary.ABBA.Qty_P <-map_df(Models.ABBA.Qty_P, broom::tidy, .id="model")
write_csv(summary.ABBA.Qty_P, path = "output/Summary_2Step/summary.ABBA.Qty_P.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.Qty_P1, which = "Nagelkerke")
# top model was Null - stop here. Probably due to poor fit. 

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
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_CNRatio_glm.pdf")
ABBA.CNRatio.residplots
dev.off()
# models meet assumptions - diagnostic plots look good
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.CNRatio <- list("ABBA.CNRatio1 = Year*Site" = ABBA.CNRatio1, "ABBA.CNRatio2 = Year" = ABBA.CNRatio2, "ABBA.CNRatio3 = Site" = ABBA.CNRatio3, "ABBA.CNRatio4 = Null" = ABBA.CNRatio4)
ABBA.CNRatio <- aictab(cand.set = Models.ABBA.CNRatio)
print(ABBA.CNRatio)
write.csv(ABBA.CNRatio, "output/AIC_2Step/ABBA_CNRatio.csv")
# save the summary tables of the models 
summary.ABBA.CNRatio <-map_df(Models.ABBA.CNRatio, broom::tidy, .id="model")
write_csv(summary.ABBA.CNRatio, path = "output/Summary_2Step/summary.ABBA.CNRatio.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.CNRatio1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.CNRatio.Global <- glm(CNRatio ~ EVI * GDD * NDMI, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.CNRatio.mech <- dredge(ABBA.CNRatio.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI))
# check the residuals of the models to ensure that glm was correct choice 
ABBA.CNRatio.mechmodels <- get.models(ABBA.CNRatio.mech,subset=NA)
ABBA.CNRatio.mech.residplots <- imap(ABBA.CNRatio.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_CNRatio_mech_glm.pdf")
ABBA.CNRatio.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.CNRatio.mech)
# save the AIC table
write_csv(ABBA.CNRatio.mech, "output/AIC_2Step/ABBA_CNRatio_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ABBA.CNRatio.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.CNRatio.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.CNRatio.mechtop <- (get.models(ABBA.CNRatio.mech, 1)[[1]])
ABBA.CNRatio.mechtop <- tidy(ABBA.CNRatio.mechtop)
write_csv(ABBA.CNRatio.mechtop, "output/Summary_2Step/summary.ABBA.CNRatio.mech.csv")
PseudoR2(ABBA.CNRatio.mechtop, which = "Nagelkerke")

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
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_CPRatio_glm.pdf")
ABBA.CPRatio.residplots
dev.off()
# models pass assumptions, diagnostic plots look good 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.CPRatio <- list("ABBA.CPRatio1 = Year*Site" = ABBA.CPRatio1, "ABBA.CPRatio2 = Year" = ABBA.CPRatio2, "ABBA.CPRatio3 = Site" = ABBA.CPRatio3, "ABBA.CPRatio4 = Null" = ABBA.CPRatio4)
ABBA.CPRatio <- aictab(cand.set = Models.ABBA.CPRatio)
print(ABBA.CPRatio)
write.csv(ABBA.CPRatio, "output/AIC_2Step/ABBA_CPRatio.csv")
# save the summary tables of the models 
summary.ABBA.CPRatio <-map_df(Models.ABBA.CPRatio, broom::tidy, .id="model")
write_csv(summary.ABBA.CPRatio, path = "output/Summary_2Step/summary.ABBA.CPRatio.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.CPRatio1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.CPRatio.Global <- glm(CPRatio ~ EVI * GDD * NDMI, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.CPRatio.mech <- dredge(ABBA.CPRatio.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI))
# check the residuals of the models to ensure that glm was correct choice 
ABBA.CPRatio.mechmodels <- get.models(ABBA.CPRatio.mech,subset=NA)
ABBA.CPRatio.mech.residplots <- imap(ABBA.CPRatio.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_CPRatio_mech_glm.pdf")
ABBA.CPRatio.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.CPRatio.mech)
# save the AIC table
write_csv(ABBA.CPRatio.mech, "output/AIC_2Step/ABBA_CPRatio_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ABBA.CPRatio.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.CPRatio.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.CPRatio.mechtop <- (get.models(ABBA.CPRatio.mech, 1)[[1]])
ABBA.CPRatio.mechtop <- tidy(ABBA.CPRatio.mechtop)
write_csv(ABBA.CPRatio.mechtop, "output/Summary_2Step/summary.ABBA.CPRatio.mech.csv")
PseudoR2(ABBA.CPRatio.mechtop, which = "Nagelkerke")

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
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_NPRatio_glm.pdf")
ABBA.NPRatio.residplots
dev.off()
# if models pass assumptions, proceed. If not, use different error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.NPRatio <- list("ABBA.NPRatio1 = Year*Site" = ABBA.NPRatio1, "ABBA.NPRatio2 = Year" = ABBA.NPRatio2, "ABBA.NPRatio3 = Site" = ABBA.NPRatio3, "ABBA.NPRatio4 = Null" = ABBA.NPRatio4)
ABBA.NPRatio <- aictab(cand.set = Models.ABBA.NPRatio)
print(ABBA.NPRatio)
write.csv(ABBA.NPRatio, "output/AIC_2Step/ABBA_NPRatio.csv")
# save the summary tables of the models 
summary.ABBA.NPRatio <-map_df(Models.ABBA.NPRatio, broom::tidy, .id="model")
write_csv(summary.ABBA.NPRatio, path = "output/Summary_2Step/summary.ABBA.NPRatio.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.NPRatio1, which = "Nagelkerke")
# site was the top model - because year is not a top variable

# ACRU
# % Carbon
ACRU.C1 <- glm(C ~ Year*Site, data = ACRU)
ACRU.C2 <- glm(C ~ Year, data = ACRU)
ACRU.C3 <- glm(C ~ Site, data = ACRU)
ACRU.C4 <- glm(C ~ 1, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Cmodels <- list(ACRU.C1, ACRU.C2, ACRU.C3, ACRU.C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.C.residplots <- imap(ACRU.Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_C_glm.pdf")
ACRU.C.residplots
dev.off()
# Model assumptions are OK, will try another error structure to see if it improves. 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.C <- list("ACRU.C1 = Year*Site" = ACRU.C1, "ACRU.C2 = Year" = ACRU.C2, "ACRU.C3 = Site" = ACRU.C3, "ACRU.C4 = Null" = ACRU.C4)
ACRU.C <- aictab(cand.set = Models.ACRU.C)
print(ACRU.C)
write.csv(ACRU.C, "output/AIC_2Step/ACRU_C.csv")
# save the summary tables of the models 
summary.ACRU.C <-map_df(Models.ACRU.C, broom::tidy, .id="model")
write_csv(summary.ACRU.C, path = "output/Summary_2Step/summary.ACRU.C.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.C1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ACRU.C.Global <- glm(C ~ EVI * GDD * NDMI, data = ACRU)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ACRU.C.mech <- dredge(ACRU.C.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI))
# check the residuals of the models to ensure that glm was correct choice 
ACRU.C.mechmodels <- get.models(ACRU.C.mech,subset=NA)
ACRU.C.mech.residplots <- imap(ACRU.C.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_C_mech_glm.pdf")
ACRU.C.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ACRU.C.mech)
# save the AIC table
write_csv(ACRU.C.mech, "output/AIC_2Step/ACRU_C_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ACRU.C.pdf")
par(mar=c(4,5,9,4))
plot(ACRU.C.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ACRU.C.mechtop <- (get.models(ACRU.C.mech, 1)[[1]])
ACRU.C.mechtop <- tidy(ACRU.C.mechtop)
ACRU.C.mechtop
write_csv(ACRU.C.mechtop, "output/Summary_2Step/summary.ACRU.C.mech.csv")

# % Nitrogen
ACRU.N1 <- glm(N ~ Year*Site, data = ACRU)
ACRU.N2 <- glm(N ~ Year, data = ACRU)
ACRU.N3 <- glm(N ~ Site, data = ACRU)
ACRU.N4 <- glm(N ~ 1, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Nmodels <- list(ACRU.N1, ACRU.N2, ACRU.N3, ACRU.N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.N.residplots <- imap(ACRU.Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_N_glm.pdf")
ACRU.N.residplots
dev.off()
# model assumptions look good
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.N <- list("ACRU.N1 = Year*Site" = ACRU.N1, "ACRU.N2 = Year" = ACRU.N2, "ACRU.N3 = Site" = ACRU.N3, "ACRU.N4 = Null" = ACRU.N4)
ACRU.N <- aictab(cand.set = Models.ACRU.N)
print(ACRU.N)
write.csv(ACRU.N, "output/AIC_2Step/ACRU_N.csv")
# save the summary tables of the models 
summary.ACRU.N <-map_df(Models.ACRU.N, broom::tidy, .id="model")
write_csv(summary.ACRU.N, path = "output/Summary_2Step/summary.ACRU.N.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.N1, which = "Nagelkerke")
# this model has Year in the top model but is within 2 delta AIC with the null so stop here

# % Phosphorus
ACRU.P1 <- glm(P ~ Year*Site, data = ACRU)
ACRU.P2 <- glm(P ~ Year, data = ACRU)
ACRU.P3 <- glm(P ~ Site, data = ACRU)
ACRU.P4 <- glm(P ~ 1, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Pmodels <- list(ACRU.P1, ACRU.P2, ACRU.P3, ACRU.P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.P.residplots <- imap(ACRU.Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_P_glm.pdf")
ACRU.P.residplots
dev.off()
# diagnostic plots don't look great, will try another error structure to see if it helps 
# create an AIPc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.P <- list("ACRU.P1 = Year*Site" = ACRU.P1, "ACRU.P2 = Year" = ACRU.P2, "ACRU.P3 = Site" = ACRU.P3, "ACRU.P4 = Null" = ACRU.P4)
ACRU.P <- aictab(cand.set = Models.ACRU.P)
print(ACRU.P)
write.csv(ACRU.P, "output/AIC_2Step/ACRU_P.csv")
# save the summary tables of the models 
summary.ACRU.P <-map_df(Models.ACRU.P, broom::tidy, .id="model")
write_csv(summary.ACRU.P, path = "output/Summary_2Step/summary.ACRU.P.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.P1, which = "Nagelkerke")
# Null is top model - stop here.

# Carbon (g)
ACRU.Qty_C1 <- glm(Qty_C ~ Year*Site, data = ACRU)
ACRU.Qty_C2 <- glm(Qty_C ~ Year, data = ACRU)
ACRU.Qty_C3 <- glm(Qty_C ~ Site, data = ACRU)
ACRU.Qty_C4 <- glm(Qty_C ~ 1, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Qty_Cmodels <- list(ACRU.Qty_C1, ACRU.Qty_C2, ACRU.Qty_C3, ACRU.Qty_C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.Qty_C.residplots <- imap(ACRU.Qty_Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_Qty_C_glm.pdf")
ACRU.Qty_C.residplots
dev.off()
# assumptions not met, use different error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.Qty_C <- list("ACRU.Qty_C1 = Year*Site" = ACRU.Qty_C1, "ACRU.Qty_C2 = Year" = ACRU.Qty_C2, "ACRU.Qty_C3 = Site" = ACRU.Qty_C3, "ACRU.Qty_C4 = Null" = ACRU.Qty_C4)
ACRU.Qty_C <- aictab(cand.set = Models.ACRU.Qty_C)
print(ACRU.Qty_C)
write.csv(ACRU.Qty_C, "output/AIC_2Step/ACRU_Qty_C.csv")
# save the summary tables of the models 
summary.ACRU.Qty_C <-map_df(Models.ACRU.Qty_C, broom::tidy, .id="model")
write_csv(summary.ACRU.Qty_C, path = "output/Summary_2Step/summary.ACRU.Qty_C.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.Qty_C1, which = "Nagelkerke")
# top model is Null - stop here. Probably due to bad fit.

# Nitrogen (g)
ACRU.Qty_N1 <- glm(Qty_N ~ Year*Site, data = ACRU)
ACRU.Qty_N2 <- glm(Qty_N ~ Year, data = ACRU)
ACRU.Qty_N3 <- glm(Qty_N ~ Site, data = ACRU)
ACRU.Qty_N4 <- glm(Qty_N ~ 1, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Qty_Nmodels <- list(ACRU.Qty_N1, ACRU.Qty_N2, ACRU.Qty_N3, ACRU.Qty_N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.Qty_N.residplots <- imap(ACRU.Qty_Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_Qty_N_glm.pdf")
ACRU.Qty_N.residplots
dev.off()
# models don't pass assumptions, use another error structure
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.Qty_N <- list("ACRU.Qty_N1 = Year*Site" = ACRU.Qty_N1, "ACRU.Qty_N2 = Year" = ACRU.Qty_N2, "ACRU.Qty_N3 = Site" = ACRU.Qty_N3, "ACRU.Qty_N4 = Null" = ACRU.Qty_N4)
ACRU.Qty_N <- aictab(cand.set = Models.ACRU.Qty_N)
print(ACRU.Qty_N)
write.csv(ACRU.Qty_N, "output/AIC_2Step/ACRU_Qty_N.csv")
# save the summary tables of the models 
summary.ACRU.Qty_N <-map_df(Models.ACRU.Qty_N, broom::tidy, .id="model")
write_csv(summary.ACRU.Qty_N, path = "output/Summary_2Step/summary.ACRU.Qty_N.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.Qty_N1, which = "Nagelkerke")
# Null is top model, stop here. Probably due to bad fit.

# Phosphorus (g)
ACRU.Qty_P1 <- glm(Qty_P ~ Year*Site, data = ACRU)
ACRU.Qty_P2 <- glm(Qty_P ~ Year, data = ACRU)
ACRU.Qty_P3 <- glm(Qty_P ~ Site, data = ACRU)
ACRU.Qty_P4 <- glm(Qty_P ~ 1, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Qty_Pmodels <- list(ACRU.Qty_P1, ACRU.Qty_P2, ACRU.Qty_P3, ACRU.Qty_P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.Qty_P.residplots <- imap(ACRU.Qty_Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_Qty_P_glm.pdf")
ACRU.Qty_P.residplots
dev.off()
# if models pass assumptions, proceed. If not, use different error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.Qty_P <- list("ACRU.Qty_P1 = Year*Site" = ACRU.Qty_P1, "ACRU.Qty_P2 = Year" = ACRU.Qty_P2, "ACRU.Qty_P3 = Site" = ACRU.Qty_P3, "ACRU.Qty_P4 = Null" = ACRU.Qty_P4)
ACRU.Qty_P <- aictab(cand.set = Models.ACRU.Qty_P)
print(ACRU.Qty_P)
write.csv(ACRU.Qty_P, "output/AIC_2Step/ACRU_Qty_P.csv")
# save the summary tables of the models 
summary.ACRU.Qty_P <-map_df(Models.ACRU.Qty_P, broom::tidy, .id="model")
write_csv(summary.ACRU.Qty_P, path = "output/Summary_2Step/summary.ACRU.Qty_P.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.Qty_P1, which = "Nagelkerke")
# Null is the top model, stop here. Probably due to bad fit. 

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
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_CNRatio_glm.pdf")
ACRU.CNRatio.residplots
dev.off()
# diagnostic plots look good
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.CNRatio <- list("ACRU.CNRatio1 = Year*Site" = ACRU.CNRatio1, "ACRU.CNRatio2 = Year" = ACRU.CNRatio2, "ACRU.CNRatio3 = Site" = ACRU.CNRatio3, "ACRU.CNRatio4 = Null" = ACRU.CNRatio4)
ACRU.CNRatio <- aictab(cand.set = Models.ACRU.CNRatio)
print(ACRU.CNRatio)
write.csv(ACRU.CNRatio, "output/AIC_2Step/ACRU_CNRatio.csv")
# save the summary tables of the models 
summary.ACRU.CNRatio <-map_df(Models.ACRU.CNRatio, broom::tidy, .id="model")
write_csv(summary.ACRU.CNRatio, path = "output/Summary_2Step/summary.ACRU.CNRatio.csv")
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
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_CPRatio_glm.pdf")
ACRU.CPRatio.residplots
dev.off()
# model diagnostics look good, assumptions are met. 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.CPRatio <- list("ACRU.CPRatio1 = Year*Site" = ACRU.CPRatio1, "ACRU.CPRatio2 = Year" = ACRU.CPRatio2, "ACRU.CPRatio3 = Site" = ACRU.CPRatio3, "ACRU.CPRatio4 = Null" = ACRU.CPRatio4)
ACRU.CPRatio <- aictab(cand.set = Models.ACRU.CPRatio)
print(ACRU.CPRatio)
write.csv(ACRU.CPRatio, "output/AIC_2Step/ACRU_CPRatio.csv")
# save the summary tables of the models 
summary.ACRU.CPRatio <-map_df(Models.ACRU.CPRatio, broom::tidy, .id="model")
write_csv(summary.ACRU.CPRatio, path = "output/Summary_2Step/summary.ACRU.CPRatio.csv")
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
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_NPRatio_glm.pdf")
ACRU.NPRatio.residplots
dev.off()
# model diagnostics look good, assumptions are met
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.NPRatio <- list("ACRU.NPRatio1 = Year*Site" = ACRU.NPRatio1, "ACRU.NPRatio2 = Year" = ACRU.NPRatio2, "ACRU.NPRatio3 = Site" = ACRU.NPRatio3, "ACRU.NPRatio4 = Null" = ACRU.NPRatio4)
ACRU.NPRatio <- aictab(cand.set = Models.ACRU.NPRatio)
print(ACRU.NPRatio)
write.csv(ACRU.NPRatio, "output/AIC_2Step/ACRU_NPRatio.csv")
# save the summary tables of the models 
summary.ACRU.NPRatio <-map_df(Models.ACRU.NPRatio, broom::tidy, .id="model")
write_csv(summary.ACRU.NPRatio, path = "output/Summary_2Step/summary.ACRU.NPRatio.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.NPRatio1, which = "Nagelkerke")
# Null is the top model, stop here.

# BEPA
# % Carbon
BEPA.C1 <- glm(C ~ Year*Site, data = BEPA)
BEPA.C2 <- glm(C ~ Year, data = BEPA)
BEPA.C3 <- glm(C ~ Site, data = BEPA)
BEPA.C4 <- glm(C ~ 1, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Cmodels <- list(BEPA.C1, BEPA.C2, BEPA.C3, BEPA.C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.C.residplots <- imap(BEPA.Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_C_glm.pdf")
BEPA.C.residplots
dev.off()
# diagnostic plots look good, assumptions are met 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.C <- list("BEPA.C1 = Year*Site" = BEPA.C1, "BEPA.C2 = Year" = BEPA.C2, "BEPA.C3 = Site" = BEPA.C3, "BEPA.C4 = Null" = BEPA.C4)
BEPA.C <- aictab(cand.set = Models.BEPA.C)
print(BEPA.C)
write.csv(BEPA.C, "output/AIC_2Step/BEPA_C.csv")
# save the summary tables of the models 
summary.BEPA.C <-map_df(Models.BEPA.C, broom::tidy, .id="model")
write_csv(summary.BEPA.C, path = "output/Summary_2Step/summary.BEPA.C.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.C1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
BEPA.C.Global <- glm(C ~ EVI * GDD * NDMI, data = BEPA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
BEPA.C.mech <- dredge(BEPA.C.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI))
# check the residuals of the models to ensure that glm was correct choice 
BEPA.C.mechmodels <- get.models(BEPA.C.mech,subset=NA)
BEPA.C.mech.residplots <- imap(BEPA.C.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_C_mech_glm.pdf")
BEPA.C.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(BEPA.C.mech)
# save the AIC table
write_csv(BEPA.C.mech, "output/AIC_2Step/BEPA_C_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/BEPA.C.pdf")
par(mar=c(4,5,9,4))
plot(BEPA.C.mech)
dev.off()
# get the summary of the top model and save it to a .csv
BEPA.C.mechtop <- (get.models(BEPA.C.mech, 1)[[1]])
BEPA.C.mechtop <- tidy(BEPA.C.mechtop)
write_csv(BEPA.C.mechtop, "output/Summary_2Step/summary.BEPA.C.mech.csv")

# % Nitrogen
BEPA.N1 <- glm(N ~ Year*Site, data = BEPA)
BEPA.N2 <- glm(N ~ Year, data = BEPA)
BEPA.N3 <- glm(N ~ Site, data = BEPA)
BEPA.N4 <- glm(N ~ 1, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Nmodels <- list(BEPA.N1, BEPA.N2, BEPA.N3, BEPA.N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.N.residplots <- imap(BEPA.Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_N_glm.pdf")
BEPA.N.residplots
dev.off()
# diagnostic plots look good, assumptions are met
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.N <- list("BEPA.N1 = Year*Site" = BEPA.N1, "BEPA.N2 = Year" = BEPA.N2, "BEPA.N3 = Site" = BEPA.N3, "BEPA.N4 = Null" = BEPA.N4)
BEPA.N <- aictab(cand.set = Models.BEPA.N)
print(BEPA.N)
write.csv(BEPA.N, "output/AIC_2Step/BEPA_N.csv")
# save the summary tables of the models 
summary.BEPA.N <-map_df(Models.BEPA.N, broom::tidy, .id="model")
write_csv(summary.BEPA.N, path = "output/Summary_2Step/summary.BEPA.N.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.N1, which = "Nagelkerke")
# top model has only site, second is null, stop here.

# % Phosphorus
BEPA.P1 <- glm(P ~ Year*Site, data = BEPA)
BEPA.P2 <- glm(P ~ Year, data = BEPA)
BEPA.P3 <- glm(P ~ Site, data = BEPA)
BEPA.P4 <- glm(P ~ 1, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Pmodels <- list(BEPA.P1, BEPA.P2, BEPA.P3, BEPA.P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.P.residplots <- imap(BEPA.Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_P_glm.pdf")
BEPA.P.residplots
dev.off()
# diagnostic plots look good, assumptions are met 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.P <- list("BEPA.P1 = Year*Site" = BEPA.P1, "BEPA.P2 = Year" = BEPA.P2, "BEPA.P3 = Site" = BEPA.P3, "BEPA.P4 = Null" = BEPA.P4)
BEPA.P <- aictab(cand.set = Models.BEPA.P)
print(BEPA.P)
write.csv(BEPA.P, "output/AIC_2Step/BEPA_P.csv")
# save the summary tables of the models 
summary.BEPA.P <-map_df(Models.BEPA.P, broom::tidy, .id="model")
write_csv(summary.BEPA.P, path = "output/Summary_2Step/summary.BEPA.P.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.P1, which = "Nagelkerke")
# top model is Site only, stop here 

# Carbon (g)
BEPA.Qty_C1 <- glm(Qty_C ~ Year*Site, data = BEPA)
BEPA.Qty_C2 <- glm(Qty_C ~ Year, data = BEPA)
BEPA.Qty_C3 <- glm(Qty_C ~ Site, data = BEPA)
BEPA.Qty_C4 <- glm(Qty_C ~ 1, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Qty_Cmodels <- list(BEPA.Qty_C1, BEPA.Qty_C2, BEPA.Qty_C3, BEPA.Qty_C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.Qty_C.residplots <- imap(BEPA.Qty_Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_Qty_C_glm.pdf")
BEPA.Qty_C.residplots
dev.off()
# diagnostic plots do not meet assumptions, choose different error structure
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.Qty_C <- list("BEPA.Qty_C1 = Year*Site" = BEPA.Qty_C1, "BEPA.Qty_C2 = Year" = BEPA.Qty_C2, "BEPA.Qty_C3 = Site" = BEPA.Qty_C3, "BEPA.Qty_C4 = Null" = BEPA.Qty_C4)
BEPA.Qty_C <- aictab(cand.set = Models.BEPA.Qty_C)
print(BEPA.Qty_C)
write.csv(BEPA.Qty_C, "output/AIC_2Step/BEPA_Qty_C.csv")
# save the summary tables of the models 
summary.BEPA.Qty_C <-map_df(Models.BEPA.Qty_C, broom::tidy, .id="model")
write_csv(summary.BEPA.Qty_C, path = "output/Summary_2Step/summary.BEPA.Qty_C.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.Qty_C1, which = "Nagelkerke")
# top model is null, stop here. Probably due to bad fit.

# Nitrogen (g)
BEPA.Qty_N1 <- glm(Qty_N ~ Year*Site, data = BEPA)
BEPA.Qty_N2 <- glm(Qty_N ~ Year, data = BEPA)
BEPA.Qty_N3 <- glm(Qty_N ~ Site, data = BEPA)
BEPA.Qty_N4 <- glm(Qty_N ~ 1, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Qty_Nmodels <- list(BEPA.Qty_N1, BEPA.Qty_N2, BEPA.Qty_N3, BEPA.Qty_N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.Qty_N.residplots <- imap(BEPA.Qty_Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_Qty_N_glm.pdf")
BEPA.Qty_N.residplots
dev.off()
# models do not meet assumptions, choose another error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.Qty_N <- list("BEPA.Qty_N1 = Year*Site" = BEPA.Qty_N1, "BEPA.Qty_N2 = Year" = BEPA.Qty_N2, "BEPA.Qty_N3 = Site" = BEPA.Qty_N3, "BEPA.Qty_N4 = Null" = BEPA.Qty_N4)
BEPA.Qty_N <- aictab(cand.set = Models.BEPA.Qty_N)
print(BEPA.Qty_N)
write.csv(BEPA.Qty_N, "output/AIC_2Step/BEPA_Qty_N.csv")
# save the summary tables of the models 
summary.BEPA.Qty_N <-map_df(Models.BEPA.Qty_N, broom::tidy, .id="model")
write_csv(summary.BEPA.Qty_N, path = "output/Summary_2Step/summary.BEPA.Qty_N.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.Qty_N1, which = "Nagelkerke")
# Null is top model, stop here. Probably due to bad fit.

# Phosphorus (g)
BEPA.Qty_P1 <- glm(Qty_P ~ Year*Site, data = BEPA)
BEPA.Qty_P2 <- glm(Qty_P ~ Year, data = BEPA)
BEPA.Qty_P3 <- glm(Qty_P ~ Site, data = BEPA)
BEPA.Qty_P4 <- glm(Qty_P ~ 1, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Qty_Pmodels <- list(BEPA.Qty_P1, BEPA.Qty_P2, BEPA.Qty_P3, BEPA.Qty_P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.Qty_P.residplots <- imap(BEPA.Qty_Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_Qty_P_glm.pdf")
BEPA.Qty_P.residplots
dev.off()
# models do not meet assumptions, use different error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.Qty_P <- list("BEPA.Qty_P1 = Year*Site" = BEPA.Qty_P1, "BEPA.Qty_P2 = Year" = BEPA.Qty_P2, "BEPA.Qty_P3 = Site" = BEPA.Qty_P3, "BEPA.Qty_P4 = Null" = BEPA.Qty_P4)
BEPA.Qty_P <- aictab(cand.set = Models.BEPA.Qty_P)
print(BEPA.Qty_P)
write.csv(BEPA.Qty_P, "output/AIC_2Step/BEPA_Qty_P.csv")
# save the summary tables of the models 
summary.BEPA.Qty_P <-map_df(Models.BEPA.Qty_P, broom::tidy, .id="model")
write_csv(summary.BEPA.Qty_P, path = "output/Summary_2Step/summary.BEPA.Qty_P.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.Qty_P1, which = "Nagelkerke")
# Null is top model, stop here. Probably due to bad fit.

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
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_CNRatio_glm.pdf")
BEPA.CNRatio.residplots
dev.off()
# diagnostic plots are ok, could try another error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.CNRatio <- list("BEPA.CNRatio1 = Year*Site" = BEPA.CNRatio1, "BEPA.CNRatio2 = Year" = BEPA.CNRatio2, "BEPA.CNRatio3 = Site" = BEPA.CNRatio3, "BEPA.CNRatio4 = Null" = BEPA.CNRatio4)
BEPA.CNRatio <- aictab(cand.set = Models.BEPA.CNRatio)
print(BEPA.CNRatio)
write.csv(BEPA.CNRatio, "output/AIC_2Step/BEPA_CNRatio.csv")
# save the summary tables of the models 
summary.BEPA.CNRatio <-map_df(Models.BEPA.CNRatio, broom::tidy, .id="model")
write_csv(summary.BEPA.CNRatio, path = "output/Summary_2Step/summary.BEPA.CNRatio.csv")
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
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_CPRatio_glm.pdf")
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
write_csv(summary.BEPA.CPRatio, path = "output/Summary_2Step/summary.BEPA.CPRatio.csv")
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
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_NPRatio_glm.pdf")
BEPA.NPRatio.residplots
dev.off()
# diagnostic plots look good, assumptions are met 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.NPRatio <- list("BEPA.NPRatio1 = Year*Site" = BEPA.NPRatio1, "BEPA.NPRatio2 = Year" = BEPA.NPRatio2, "BEPA.NPRatio3 = Site" = BEPA.NPRatio3, "BEPA.NPRatio4 = Null" = BEPA.NPRatio4)
BEPA.NPRatio <- aictab(cand.set = Models.BEPA.NPRatio)
print(BEPA.NPRatio)
write.csv(BEPA.NPRatio, "output/AIC_2Step/BEPA_NPRatio.csv")
# save the summary tables of the models 
summary.BEPA.NPRatio <-map_df(Models.BEPA.NPRatio, broom::tidy, .id="model")
write_csv(summary.BEPA.NPRatio, path = "output/Summary_2Step/summary.BEPA.NPRatio.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.NPRatio1, which = "Nagelkerke")
# Null was the top model, stop here

# VAAN
# % Carbon
VAAN.C1 <- glm(C ~ Year*Site, data = VAAN)
VAAN.C2 <- glm(C ~ Year, data = VAAN)
VAAN.C3 <- glm(C ~ Site, data = VAAN)
VAAN.C4 <- glm(C ~ 1, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Cmodels <- list(VAAN.C1, VAAN.C2, VAAN.C3, VAAN.C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.C.residplots <- imap(VAAN.Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_C_glm.pdf")
VAAN.C.residplots
dev.off()
# diagnostic plots are okay, could use a different error structure to check
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.C <- list("VAAN.C1 = Year*Site" = VAAN.C1, "VAAN.C2 = Year" = VAAN.C2, "VAAN.C3 = Site" = VAAN.C3, "VAAN.C4 = Null" = VAAN.C4)
VAAN.C <- aictab(cand.set = Models.VAAN.C)
print(VAAN.C)
write.csv(VAAN.C, "output/AIC_2Step/VAAN_C.csv")
# save the summary tables of the models 
summary.VAAN.C <-map_df(Models.VAAN.C, broom::tidy, .id="model")
write_csv(summary.VAAN.C, path = "output/Summary_2Step/summary.VAAN.C.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.C1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
VAAN.C.Global <- glm(C ~ EVI * GDD * NDMI, data = VAAN)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
VAAN.C.mech <- dredge(VAAN.C.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI))
# check the residuals of the models to ensure that glm was correct choice 
VAAN.C.mechmodels <- get.models(VAAN.C.mech,subset=NA)
VAAN.C.mech.residplots <- imap(VAAN.C.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_C_mech_glm.pdf")
VAAN.C.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(VAAN.C.mech)
# save the AIC table
write_csv(VAAN.C.mech, "output/AIC_2Step/VAAN_C_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/VAAN.C.pdf")
par(mar=c(4,5,9,4))
plot(VAAN.C.mech)
dev.off()
# get the summary of the top model and save it to a .csv
VAAN.C.mechtop <- (get.models(VAAN.C.mech, 1)[[1]])
VAAN.C.mechtop <- tidy(VAAN.C.mechtop)
write_csv(VAAN.C.mechtop, "output/Summary_2Step/summary.VAAN.C.mech.csv")

# % Nitrogen
VAAN.N1 <- glm(N ~ Year*Site, data = VAAN)
VAAN.N2 <- glm(N ~ Year, data = VAAN)
VAAN.N3 <- glm(N ~ Site, data = VAAN)
VAAN.N4 <- glm(N ~ 1, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Nmodels <- list(VAAN.N1, VAAN.N2, VAAN.N3, VAAN.N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.N.residplots <- imap(VAAN.Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_N_glm.pdf")
VAAN.N.residplots
dev.off()
# diagnostic plots are okay, assumptions are met
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.N <- list("VAAN.N1 = Year*Site" = VAAN.N1, "VAAN.N2 = Year" = VAAN.N2, "VAAN.N3 = Site" = VAAN.N3, "VAAN.N4 = Null" = VAAN.N4)
VAAN.N <- aictab(cand.set = Models.VAAN.N)
print(VAAN.N)
write.csv(VAAN.N, "output/AIC_2Step/VAAN_N.csv")
# save the summary tables of the models 
summary.VAAN.N <-map_df(Models.VAAN.N, broom::tidy, .id="model")
write_csv(summary.VAAN.N, path = "output/Summary_2Step/summary.VAAN.N.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.N1, which = "Nagelkerke")
# Null model is top model, stop here

# % Phosphorus
VAAN.P1 <- glm(P ~ Year*Site, data = VAAN)
VAAN.P2 <- glm(P ~ Year, data = VAAN)
VAAN.P3 <- glm(P ~ Site, data = VAAN)
VAAN.P4 <- glm(P ~ 1, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Pmodels <- list(VAAN.P1, VAAN.P2, VAAN.P3, VAAN.P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.P.residplots <- imap(VAAN.Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_P_glm.pdf")
VAAN.P.residplots
dev.off()
# diagnostic plots are okay, assumptions are met 
# create an AIPc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.P <- list("VAAN.P1 = Year*Site" = VAAN.P1, "VAAN.P2 = Year" = VAAN.P2, "VAAN.P3 = Site" = VAAN.P3, "VAAN.P4 = Null" = VAAN.P4)
VAAN.P <- aictab(cand.set = Models.VAAN.P)
print(VAAN.P)
write.csv(VAAN.P, "output/AIC_2Step/VAAN_P.csv")
# save the summary tables of the models 
summary.VAAN.P <-map_df(Models.VAAN.P, broom::tidy, .id="model")
write_csv(summary.VAAN.P, path = "output/Summary_2Step/summary.VAAN.P.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.P1, which = "Nagelkerke")
# top model is only Site, stop here

# Carbon (g)
VAAN.Qty_C1 <- glm(Qty_C ~ Year*Site, data = VAAN)
VAAN.Qty_C2 <- glm(Qty_C ~ Year, data = VAAN)
VAAN.Qty_C3 <- glm(Qty_C ~ Site, data = VAAN)
VAAN.Qty_C4 <- glm(Qty_C ~ 1, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Qty_Cmodels <- list(VAAN.Qty_C1, VAAN.Qty_C2, VAAN.Qty_C3, VAAN.Qty_C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.Qty_C.residplots <- imap(VAAN.Qty_Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_Qty_C_glm.pdf")
VAAN.Qty_C.residplots
dev.off()
# assumptions are not met, need to choose different error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.Qty_C <- list("VAAN.Qty_C1 = Year*Site" = VAAN.Qty_C1, "VAAN.Qty_C2 = Year" = VAAN.Qty_C2, "VAAN.Qty_C3 = Site" = VAAN.Qty_C3, "VAAN.Qty_C4 = Null" = VAAN.Qty_C4)
VAAN.Qty_C <- aictab(cand.set = Models.VAAN.Qty_C)
print(VAAN.Qty_C)
write.csv(VAAN.Qty_C, "output/AIC_2Step/VAAN_Qty_C.csv")
# save the summary tables of the models 
summary.VAAN.Qty_C <-map_df(Models.VAAN.Qty_C, broom::tidy, .id="model")
write_csv(summary.VAAN.Qty_C, path = "output/Summary_2Step/summary.VAAN.Qty_C.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.Qty_C1, which = "Nagelkerke")
# top model is just Site, stop here.Could be do to bad fit.

# Nitrogen (g)
VAAN.Qty_N1 <- glm(Qty_N ~ Year*Site, data = VAAN)
VAAN.Qty_N2 <- glm(Qty_N ~ Year, data = VAAN)
VAAN.Qty_N3 <- glm(Qty_N ~ Site, data = VAAN)
VAAN.Qty_N4 <- glm(Qty_N ~ 1, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Qty_Nmodels <- list(VAAN.Qty_N1, VAAN.Qty_N2, VAAN.Qty_N3, VAAN.Qty_N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.Qty_N.residplots <- imap(VAAN.Qty_Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_Qty_N_glm.pdf")
VAAN.Qty_N.residplots
dev.off()
# assumptions not met, choose a different error structure 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.Qty_N <- list("VAAN.Qty_N1 = Year*Site" = VAAN.Qty_N1, "VAAN.Qty_N2 = Year" = VAAN.Qty_N2, "VAAN.Qty_N3 = Site" = VAAN.Qty_N3, "VAAN.Qty_N4 = Null" = VAAN.Qty_N4)
VAAN.Qty_N <- aictab(cand.set = Models.VAAN.Qty_N)
print(VAAN.Qty_N)
write.csv(VAAN.Qty_N, "output/AIC_2Step/VAAN_Qty_N.csv")
# save the summary tables of the models 
summary.VAAN.Qty_N <-map_df(Models.VAAN.Qty_N, broom::tidy, .id="model")
write_csv(summary.VAAN.Qty_N, path = "output/Summary_2Step/summary.VAAN.Qty_N.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.Qty_N1, which = "Nagelkerke")
# top model is only Site, stop here. Could be due to bad fit.

# Phosphorus (g)
VAAN.Qty_P1 <- glm(Qty_P ~ Year*Site, data = VAAN)
VAAN.Qty_P2 <- glm(Qty_P ~ Year, data = VAAN)
VAAN.Qty_P3 <- glm(Qty_P ~ Site, data = VAAN)
VAAN.Qty_P4 <- glm(Qty_P ~ 1, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Qty_Pmodels <- list(VAAN.Qty_P1, VAAN.Qty_P2, VAAN.Qty_P3, VAAN.Qty_P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.Qty_P.residplots <- imap(VAAN.Qty_Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_Qty_P_glm.pdf")
VAAN.Qty_P.residplots
dev.off()
# assumptions not met, choose a different error structure
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.Qty_P <- list("VAAN.Qty_P1 = Year*Site" = VAAN.Qty_P1, "VAAN.Qty_P2 = Year" = VAAN.Qty_P2, "VAAN.Qty_P3 = Site" = VAAN.Qty_P3, "VAAN.Qty_P4 = Null" = VAAN.Qty_P4)
VAAN.Qty_P <- aictab(cand.set = Models.VAAN.Qty_P)
print(VAAN.Qty_P)
write.csv(VAAN.Qty_P, "output/AIC_2Step/VAAN_Qty_P.csv")
# save the summary tables of the models 
summary.VAAN.Qty_P <-map_df(Models.VAAN.Qty_P, broom::tidy, .id="model")
write_csv(summary.VAAN.Qty_P, path = "output/Summary_2Step/summary.VAAN.Qty_P.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.Qty_P1, which = "Nagelkerke")
# top model has only Site, stop here. Probably due to bad fit.

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
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_CNRatio_glm.pdf")
VAAN.CNRatio.residplots
dev.off()
# diagnostic plots look good, assumptions are met
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.CNRatio <- list("VAAN.CNRatio1 = Year*Site" = VAAN.CNRatio1, "VAAN.CNRatio2 = Year" = VAAN.CNRatio2, "VAAN.CNRatio3 = Site" = VAAN.CNRatio3, "VAAN.CNRatio4 = Null" = VAAN.CNRatio4)
VAAN.CNRatio <- aictab(cand.set = Models.VAAN.CNRatio)
print(VAAN.CNRatio)
write.csv(VAAN.CNRatio, "output/AIC_2Step/VAAN_CNRatio.csv")
# save the summary tables of the models 
summary.VAAN.CNRatio <-map_df(Models.VAAN.CNRatio, broom::tidy, .id="model")
write_csv(summary.VAAN.CNRatio, path = "output/Summary_2Step/summary.VAAN.CNRatio.csv")
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
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_CPRatio_glm.pdf")
VAAN.CPRatio.residplots
dev.off()
# diagnostic plots look good, assumptions are met 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.CPRatio <- list("VAAN.CPRatio1 = Year*Site" = VAAN.CPRatio1, "VAAN.CPRatio2 = Year" = VAAN.CPRatio2, "VAAN.CPRatio3 = Site" = VAAN.CPRatio3, "VAAN.CPRatio4 = Null" = VAAN.CPRatio4)
VAAN.CPRatio <- aictab(cand.set = Models.VAAN.CPRatio)
print(VAAN.CPRatio)
write.csv(VAAN.CPRatio, "output/AIC_2Step/VAAN_CPRatio.csv")
# save the summary tables of the models 
summary.VAAN.CPRatio <-map_df(Models.VAAN.CPRatio, broom::tidy, .id="model")
write_csv(summary.VAAN.CPRatio, path = "output/Summary_2Step/summary.VAAN.CPRatio.csv")
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
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_NPRatio_glm.pdf")
VAAN.NPRatio.residplots
dev.off()
# diagnostic plots are okay, assumptions are met. Could use different error structure to check. 
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.NPRatio <- list("VAAN.NPRatio1 = Year*Site" = VAAN.NPRatio1, "VAAN.NPRatio2 = Year" = VAAN.NPRatio2, "VAAN.NPRatio3 = Site" = VAAN.NPRatio3, "VAAN.NPRatio4 = Null" = VAAN.NPRatio4)
VAAN.NPRatio <- aictab(cand.set = Models.VAAN.NPRatio)
print(VAAN.NPRatio)
write.csv(VAAN.NPRatio, "output/AIC_2Step/VAAN_NPRatio.csv")
# save the summary tables of the models 
summary.VAAN.NPRatio <-map_df(Models.VAAN.NPRatio, broom::tidy, .id="model")
write_csv(summary.VAAN.NPRatio, path = "output/Summary_2Step/summary.VAAN.NPRatio.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.NPRatio1, which = "Nagelkerke")
# top model has only Site, stop here.