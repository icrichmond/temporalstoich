# Author: Isabella Richmond
# Last edited: April 1, 2020

# This code was for the creation and evaluation of my temporal stoichiometry models. A lot of 
# code was provided by Travis Heckford (twitter.com/travheckford)
# These are the generalized linear models for the models that did not meet assumptions of the GLM
# performed in a separate script - Stoich_TemporalModels_2step.R in the code folder 

# Models evaluate the response of stoichiometry in four boreal plant species with year and site 
# If year is found to be in the top model when compared using AICc, another model is conducted 
# where mechanisms are investigated (productivity, site, moisture, weather)


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

### Generalized Linear Models ###
# move from general linear models to generalized linear models for the quantity models as they
# violated assumptions
# start by transforming g to mg to reduce zero inflation and see if it helps
# set tibble width to Inf so that we can see the new columns that we create in our tibble dataframe
options(tibble.print_max = 10, tibble.width= Inf)
# coerce stoich dataframe into a tibble and then add columns for quantities transformed into mg 
stoich <- stoich %>% as_tibble() %>% mutate(
  Qty_C_mg = Qty_C*1000,
  Qty_N_mg = Qty_N*1000,
  Qty_P_mg = Qty_P*1000
  
)
# now subset by species so that we can test some models
ABBA <- filter(stoich, Species=="ABBA") 
ACRU <- filter(stoich, Species=="ACRU")
BEPA <- filter(stoich, Species=="BEPA")
VAAN <- filter(stoich, Species=="VAAN")

# let's run some GLMs and see if the assumptions get any better
# will just to a test run with ABBA to see if the transformation works 
# Carbon (g)
ABBA.Qty_C_mg1 <- glm(Qty_C_mg ~ Year*Site, data = ABBA)
ABBA.Qty_C_mg2 <- glm(Qty_C_mg ~ Year, data = ABBA)
ABBA.Qty_C_mg3 <- glm(Qty_C_mg ~ Site, data = ABBA)
ABBA.Qty_C_mg4 <- glm(Qty_C_mg ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Qty_C_mgmodels <- list(ABBA.Qty_C_mg1, ABBA.Qty_C_mg2, ABBA.Qty_C_mg3, ABBA.Qty_C_mg4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.Qty_C_mg.residplots <- imap(ABBA.Qty_C_mgmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/ABBA_Qty_C_mg.pdf")
ABBA.Qty_C_mg.residplots
dev.off()
# models do not meet assumptions - really bad fit. Will have to try a different error structure.


# Nitrogen (g)
ABBA.Qty_N_mg1 <- glm(Qty_N_mg ~ Year*Site, data = ABBA)
ABBA.Qty_N_mg2 <- glm(Qty_N_mg ~ Year, data = ABBA)
ABBA.Qty_N_mg3 <- glm(Qty_N_mg ~ Site, data = ABBA)
ABBA.Qty_N_mg4 <- glm(Qty_N_mg ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Qty_N_mgmodels <- list(ABBA.Qty_N_mg1, ABBA.Qty_N_mg2, ABBA.Qty_N_mg3, ABBA.Qty_N_mg4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.Qty_N_mg.residplots <- imap(ABBA.Qty_N_mgmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/ABBA_Qty_N_mg.pdf")
ABBA.Qty_N_mg.residplots
dev.off()
# models do not meet assumptions - really bad fit. Will have to try a different error structure.

# Phosphorus (g)
ABBA.Qty_P_mg1 <- glm(Qty_P_mg ~ Year*Site, data = ABBA)
ABBA.Qty_P_mg2 <- glm(Qty_P_mg ~ Year, data = ABBA)
ABBA.Qty_P_mg3 <- glm(Qty_P_mg ~ Site, data = ABBA)
ABBA.Qty_P_mg4 <- glm(Qty_P_mg ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Qty_P_mgmodels <- list(ABBA.Qty_P_mg1, ABBA.Qty_P_mg2, ABBA.Qty_P_mg3, ABBA.Qty_P_mg4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.Qty_P_mg.residplots <- imap(ABBA.Qty_P_mgmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/ABBA_Qty_P_mg.pdf")
ABBA.Qty_P_mg.residplots
dev.off()
# models do not meet assumptions - really bad fit. Will have to try a different error structure.

# transformation to mg did not work to get the models to meet GLM assumptions 
# next attempt is to try a gamma error distribution 
# ABBA 
# Carbon (g)
ABBA.Qty_C1 <- glm(Qty_C ~ Year*Site, family = Gamma, data = ABBA)
ABBA.Qty_C2 <- glm(Qty_C ~ Year, family = Gamma, data = ABBA)
ABBA.Qty_C3 <- glm(Qty_C ~ Site, family = Gamma, data = ABBA)
ABBA.Qty_C4 <- glm(Qty_C ~ 1, family = Gamma, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Qty_Cmodels <- list(ABBA.Qty_C1, ABBA.Qty_C2, ABBA.Qty_C3, ABBA.Qty_C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.Qty_C.residplots <- imap(ABBA.Qty_Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/ABBA_Qty_C_gamma.pdf")
ABBA.Qty_C.residplots
dev.off()
# assumptions look A LOT better. Can proceed.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.Qty_C <- list("ABBA.Qty_C1 = Year*Site" = ABBA.Qty_C1, "ABBA.Qty_C2 = Year" = ABBA.Qty_C2, "ABBA.Qty_C3 = Site" = ABBA.Qty_C3, "ABBA.Qty_C4 = Null" = ABBA.Qty_C4)
ABBA.Qty_C <- aictab(cand.set = Models.ABBA.Qty_C)
print(ABBA.Qty_C)
write.csv(ABBA.Qty_C, "output/AIC_2Step/ABBA_Qty_C_gamma.csv")
# save the summary tables of the models 
summary.ABBA.Qty_C <-map_df(Models.ABBA.Qty_C, broom::tidy, .id="model")
write_csv(summary.ABBA.Qty_C, path = "output/Summary_2Step/summary.ABBA.Qty_C_gamma.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.Qty_C1, which = "Nagelkerke")
# top model was Year*Site, continue on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.Qty_C.Global <- glm(Qty_C ~ EVI * GDD * NDMI * Site, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.Qty_C.Global <- glm(Qty_C ~ EVI*GDD*NDMI*Site, data = ABBA)
ABBA.Qty_C.mech <- dredge(ABBA.Qty_C.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI | EVI*GDD*Site | EVI*NDMI*Site | GDD*NDMI*Site))
# check the residuals of the models to ensure that glm was correct choice 
ABBA.Qty_C.mechmodels <- get.models(ABBA.Qty_C.mech,subset=NA)
ABBA.Qty_C.mech.residplots <- imap(ABBA.Qty_C.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_QtyC_mech_gamma.pdf")
ABBA.Qty_C.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.Qty_C.mech)
# save the AIC table
write_csv(ABBA.Qty_C.mech, "output/AIC_2Step/ABBA_QtyC_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ABBA.QtyC.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.Qty_C.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.Qty_C.mechtop <- (get.models(ABBA.Qty_C.mech, 1)[[1]])
ABBA.Qty_C.mechtop <- tidy(ABBA.Qty_C.mechtop)
write_csv(ABBA.Qty_C.mechtop, "output/Summary_2Step/summary.ABBA.Qty_C.mech.csv")


# Nitrogen (g)
ABBA.Qty_N1 <- glm(Qty_N ~ Year*Site, family = Gamma, data = ABBA)
ABBA.Qty_N2 <- glm(Qty_N ~ Year, family = Gamma,  data = ABBA)
ABBA.Qty_N3 <- glm(Qty_N ~ Site, family = Gamma, data = ABBA)
ABBA.Qty_N4 <- glm(Qty_N ~ 1, family = Gamma, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Qty_Nmodels <- list(ABBA.Qty_N1, ABBA.Qty_N2, ABBA.Qty_N3, ABBA.Qty_N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.Qty_N.residplots <- imap(ABBA.Qty_Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/ABBA_Qty_N_gamma.pdf")
ABBA.Qty_N.residplots
dev.off()
# assumptions look A LOT better. Can proceed.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.Qty_N <- list("ABBA.Qty_N1 = Year*Site" = ABBA.Qty_N1, "ABBA.Qty_N2 = Year" = ABBA.Qty_N2, "ABBA.Qty_N3 = Site" = ABBA.Qty_N3, "ABBA.Qty_N4 = Null" = ABBA.Qty_N4)
ABBA.Qty_N <- aictab(cand.set = Models.ABBA.Qty_N)
print(ABBA.Qty_N)
write.csv(ABBA.Qty_N, "output/AIC_2Step/ABBA_Qty_N_gamma.csv")
# save the summary tables of the models 
summary.ABBA.Qty_N <-map_df(Models.ABBA.Qty_N, broom::tidy, .id="model")
write_csv(summary.ABBA.Qty_N, path = "output/Summary_2Step/summary.ABBA.Qty_N_gamma.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.Qty_N1, which = "Nagelkerke")
# top model was Year*Site, continue on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.Qty_N.Global <- glm(Qty_N ~ EVI * GDD * NDMI * Site, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.Qty_N.Global <- glm(Qty_N ~ EVI*GDD*NDMI*Site, data = ABBA)
ABBA.Qty_N.mech <- dredge(ABBA.Qty_N.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI && GDD && NDMI | EVI && GDD && Site | EVI && NDMI && Site | GDD && NDMI && Site))
# check the residuals of the models to ensure that glm was correct choice 
ABBA.Qty_N.mechmodels <- get.models(ABBA.Qty_N.mech,subset=NA)
ABBA.Qty_N.mech.residplots <- imap(ABBA.Qty_N.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_QtyN_mech_gamma.pdf")
ABBA.Qty_N.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.Qty_N.mech)
# save the AIC table
write_csv(ABBA.Qty_N.mech, "output/AIC_2Step/ABBA_QtyN_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ABBA.QtyN.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.Qty_N.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.Qty_N.mechtop <- (get.models(ABBA.Qty_N.mech, 1)[[1]])
ABBA.Qty_N.mechtop <- tidy(ABBA.Qty_N.mechtop)
write_csv(ABBA.Qty_N.mechtop, "output/Summary_2Step/summary.ABBA.Qty_N.mech.csv")

# Phosphorus (g)
ABBA.Qty_P1 <- glm(Qty_P ~ Year*Site, family = Gamma, data = ABBA)
ABBA.Qty_P2 <- glm(Qty_P ~ Year, family = Gamma, data = ABBA)
ABBA.Qty_P3 <- glm(Qty_P ~ Site, family = Gamma, data = ABBA)
ABBA.Qty_P4 <- glm(Qty_P ~ 1, family = Gamma, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Qty_Pmodels <- list(ABBA.Qty_P1, ABBA.Qty_P2, ABBA.Qty_P3, ABBA.Qty_P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.Qty_P.residplots <- imap(ABBA.Qty_Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/ABBA_Qty_P_gamma.pdf")
ABBA.Qty_P.residplots
dev.off()
# models do not meet assumptions - really bad fit. Will have to try a different error structure.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.Qty_P <- list("ABBA.Qty_P1 = Year*Site" = ABBA.Qty_P1, "ABBA.Qty_P2 = Year" = ABBA.Qty_P2, "ABBA.Qty_P3 = Site" = ABBA.Qty_P3, "ABBA.Qty_P4 = Null" = ABBA.Qty_P4)
ABBA.Qty_P <- aictab(cand.set = Models.ABBA.Qty_P)
print(ABBA.Qty_P)
write.csv(ABBA.Qty_P, "output/AIC_2Step/ABBA_Qty_P_gamma.csv")
# save the summary tables of the models 
summary.ABBA.Qty_P <-map_df(Models.ABBA.Qty_P, broom::tidy, .id="model")
write_csv(summary.ABBA.Qty_P, path = "output/Summary_2Step/summary.ABBA.Qty_P_gamma.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.Qty_P1, which = "Nagelkerke")
# top model was Year*Site, continue on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.Qty_P.Global <- glm(Qty_P ~ EVI * GDD * NDMI * Site, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.Qty_P.Global <- glm(Qty_P ~ EVI*GDD*NDMI*Site, data = ABBA)
ABBA.Qty_P.mech <- dredge(ABBA.Qty_P.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI && GDD && NDMI | EVI && GDD && Site | EVI && NDMI && Site | GDD && NDMI && Site))
# check the residuals of the models to ensure that glm was correct choice 
ABBA.Qty_P.mechmodels <- get.models(ABBA.Qty_P.mech,subset=NA)
ABBA.Qty_P.mech.residplots <- imap(ABBA.Qty_P.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_QtyP_mech_gamma.pdf")
ABBA.Qty_P.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.Qty_P.mech)
# save the AIC table
write_csv(ABBA.Qty_P.mech, "output/AIC_2Step/ABBA_QtyP_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ABBA.QtyP.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.Qty_P.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.Qty_P.mechtop <- (get.models(ABBA.Qty_P.mech, 1)[[1]])
ABBA.Qty_P.mechtop <- tidy(ABBA.Qty_P.mechtop)
write_csv(ABBA.Qty_P.mechtop, "output/Summary_2Step/summary.ABBA.Qty_P.mech.csv")

#ACRU
# Carbon (g)
ACRU.Qty_C1 <- glm(Qty_C ~ Year*Site, family = Gamma, data = ACRU)
ACRU.Qty_C2 <- glm(Qty_C ~ Year, family = Gamma, data = ACRU)
ACRU.Qty_C3 <- glm(Qty_C ~ Site, family = Gamma, data = ACRU)
ACRU.Qty_C4 <- glm(Qty_C ~ 1, family = Gamma, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Qty_Cmodels <- list(ACRU.Qty_C1, ACRU.Qty_C2, ACRU.Qty_C3, ACRU.Qty_C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.Qty_C.residplots <- imap(ACRU.Qty_Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/ACRU_Qty_C_gamma.pdf")
ACRU.Qty_C.residplots
dev.off()
# assumptions look A LOT better. Can proceed.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.Qty_C <- list("ACRU.Qty_C1 = Year*Site" = ACRU.Qty_C1, "ACRU.Qty_C2 = Year" = ACRU.Qty_C2, "ACRU.Qty_C3 = Site" = ACRU.Qty_C3, "ACRU.Qty_C4 = Null" = ACRU.Qty_C4)
ACRU.Qty_C <- aictab(cand.set = Models.ACRU.Qty_C)
print(ACRU.Qty_C)
write.csv(ACRU.Qty_C, "output/AIC_2Step/ACRU_Qty_C_gamma.csv")
# save the summary tables of the models 
summary.ACRU.Qty_C <-map_df(Models.ACRU.Qty_C, broom::tidy, .id="model")
write_csv(summary.ACRU.Qty_C, path = "output/Summary_2Step/summary.ACRU.Qty_C_gamma.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.Qty_C1, which = "Nagelkerke")
# top model was Null - stop here.  

# Nitrogen (g)
ACRU.Qty_N1 <- glm(Qty_N ~ Year*Site, family = Gamma, data = ACRU)
ACRU.Qty_N2 <- glm(Qty_N ~ Year, family = Gamma,  data = ACRU)
ACRU.Qty_N3 <- glm(Qty_N ~ Site, family = Gamma, data = ACRU)
ACRU.Qty_N4 <- glm(Qty_N ~ 1, family = Gamma, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Qty_Nmodels <- list(ACRU.Qty_N1, ACRU.Qty_N2, ACRU.Qty_N3, ACRU.Qty_N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.Qty_N.residplots <- imap(ACRU.Qty_Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/ACRU_Qty_N_gamma.pdf")
ACRU.Qty_N.residplots
dev.off()
# assumptions look A LOT better. Can proceed.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.Qty_N <- list("ACRU.Qty_N1 = Year*Site" = ACRU.Qty_N1, "ACRU.Qty_N2 = Year" = ACRU.Qty_N2, "ACRU.Qty_N3 = Site" = ACRU.Qty_N3, "ACRU.Qty_N4 = Null" = ACRU.Qty_N4)
ACRU.Qty_N <- aictab(cand.set = Models.ACRU.Qty_N)
print(ACRU.Qty_N)
write.csv(ACRU.Qty_N, "output/AIC_2Step/ACRU_Qty_N_gamma.csv")
# save the summary tables of the models 
summary.ACRU.Qty_N <-map_df(Models.ACRU.Qty_N, broom::tidy, .id="model")
write_csv(summary.ACRU.Qty_N, path = "output/Summary_2Step/summary.ACRU.Qty_N_gamma.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.Qty_N1, which = "Nagelkerke")
# top model was Null - stop here. 

# Phosphorus (g)
ACRU.Qty_P1 <- glm(Qty_P ~ Year*Site, family = Gamma, data = ACRU)
ACRU.Qty_P2 <- glm(Qty_P ~ Year, family = Gamma, data = ACRU)
ACRU.Qty_P3 <- glm(Qty_P ~ Site, family = Gamma, data = ACRU)
ACRU.Qty_P4 <- glm(Qty_P ~ 1, family = Gamma, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Qty_Pmodels <- list(ACRU.Qty_P1, ACRU.Qty_P2, ACRU.Qty_P3, ACRU.Qty_P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.Qty_P.residplots <- imap(ACRU.Qty_Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/ACRU_Qty_P_gamma.pdf")
ACRU.Qty_P.residplots
dev.off()
# models do not meet assumptions - really bad fit. Will have to try a different error structure.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.Qty_P <- list("ACRU.Qty_P1 = Year*Site" = ACRU.Qty_P1, "ACRU.Qty_P2 = Year" = ACRU.Qty_P2, "ACRU.Qty_P3 = Site" = ACRU.Qty_P3, "ACRU.Qty_P4 = Null" = ACRU.Qty_P4)
ACRU.Qty_P <- aictab(cand.set = Models.ACRU.Qty_P)
print(ACRU.Qty_P)
write.csv(ACRU.Qty_P, "output/AIC_2Step/ACRU_Qty_P_gamma.csv")
# save the summary tables of the models 
summary.ACRU.Qty_P <-map_df(Models.ACRU.Qty_P, broom::tidy, .id="model")
write_csv(summary.ACRU.Qty_P, path = "output/Summary_2Step/summary.ACRU.Qty_P_gamma.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.Qty_P1, which = "Nagelkerke")
# top model was Null - stop here. 

# BEPA
# Carbon (g)
BEPA.Qty_C1 <- glm(Qty_C ~ Year*Site, family = Gamma, data = BEPA)
BEPA.Qty_C2 <- glm(Qty_C ~ Year, family = Gamma, data = BEPA)
BEPA.Qty_C3 <- glm(Qty_C ~ Site, family = Gamma, data = BEPA)
BEPA.Qty_C4 <- glm(Qty_C ~ 1, family = Gamma, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Qty_Cmodels <- list(BEPA.Qty_C1, BEPA.Qty_C2, BEPA.Qty_C3, BEPA.Qty_C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.Qty_C.residplots <- imap(BEPA.Qty_Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/BEPA_Qty_C_gamma.pdf")
BEPA.Qty_C.residplots
dev.off()
# assumptions look A LOT better. Can proceed.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.Qty_C <- list("BEPA.Qty_C1 = Year*Site" = BEPA.Qty_C1, "BEPA.Qty_C2 = Year" = BEPA.Qty_C2, "BEPA.Qty_C3 = Site" = BEPA.Qty_C3, "BEPA.Qty_C4 = Null" = BEPA.Qty_C4)
BEPA.Qty_C <- aictab(cand.set = Models.BEPA.Qty_C)
print(BEPA.Qty_C)
write.csv(BEPA.Qty_C, "output/AIC_2Step/BEPA_Qty_C_gamma.csv")
# save the summary tables of the models 
summary.BEPA.Qty_C <-map_df(Models.BEPA.Qty_C, broom::tidy, .id="model")
write_csv(summary.BEPA.Qty_C, path = "output/Summary_2Step/summary.BEPA.Qty_C_gamma.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.Qty_C1, which = "Nagelkerke")
# top model was Null - stop here.  

# Nitrogen (g)
BEPA.Qty_N1 <- glm(Qty_N ~ Year*Site, family = Gamma, data = BEPA)
BEPA.Qty_N2 <- glm(Qty_N ~ Year, family = Gamma,  data = BEPA)
BEPA.Qty_N3 <- glm(Qty_N ~ Site, family = Gamma, data = BEPA)
BEPA.Qty_N4 <- glm(Qty_N ~ 1, family = Gamma, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Qty_Nmodels <- list(BEPA.Qty_N1, BEPA.Qty_N2, BEPA.Qty_N3, BEPA.Qty_N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.Qty_N.residplots <- imap(BEPA.Qty_Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/BEPA_Qty_N_gamma.pdf")
BEPA.Qty_N.residplots
dev.off()
# assumptions look A LOT better. Can proceed.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.Qty_N <- list("BEPA.Qty_N1 = Year*Site" = BEPA.Qty_N1, "BEPA.Qty_N2 = Year" = BEPA.Qty_N2, "BEPA.Qty_N3 = Site" = BEPA.Qty_N3, "BEPA.Qty_N4 = Null" = BEPA.Qty_N4)
BEPA.Qty_N <- aictab(cand.set = Models.BEPA.Qty_N)
print(BEPA.Qty_N)
write.csv(BEPA.Qty_N, "output/AIC_2Step/BEPA_Qty_N_gamma.csv")
# save the summary tables of the models 
summary.BEPA.Qty_N <-map_df(Models.BEPA.Qty_N, broom::tidy, .id="model")
write_csv(summary.BEPA.Qty_N, path = "output/Summary_2Step/summary.BEPA.Qty_N_gamma.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.Qty_N1, which = "Nagelkerke")
# top model was Null - stop here.  

# Phosphorus (g)
BEPA.Qty_P1 <- glm(Qty_P ~ Year*Site, family = Gamma, data = BEPA)
BEPA.Qty_P2 <- glm(Qty_P ~ Year, family = Gamma, data = BEPA)
BEPA.Qty_P3 <- glm(Qty_P ~ Site, family = Gamma, data = BEPA)
BEPA.Qty_P4 <- glm(Qty_P ~ 1, family = Gamma, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Qty_Pmodels <- list(BEPA.Qty_P1, BEPA.Qty_P2, BEPA.Qty_P3, BEPA.Qty_P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.Qty_P.residplots <- imap(BEPA.Qty_Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/BEPA_Qty_P_gamma.pdf")
BEPA.Qty_P.residplots
dev.off()
# models do not meet assumptions - really bad fit. Will have to try a different error structure.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.Qty_P <- list("BEPA.Qty_P1 = Year*Site" = BEPA.Qty_P1, "BEPA.Qty_P2 = Year" = BEPA.Qty_P2, "BEPA.Qty_P3 = Site" = BEPA.Qty_P3, "BEPA.Qty_P4 = Null" = BEPA.Qty_P4)
BEPA.Qty_P <- aictab(cand.set = Models.BEPA.Qty_P)
print(BEPA.Qty_P)
write.csv(BEPA.Qty_P, "output/AIC_2Step/BEPA_Qty_P_gamma.csv")
# save the summary tables of the models 
summary.BEPA.Qty_P <-map_df(Models.BEPA.Qty_P, broom::tidy, .id="model")
write_csv(summary.BEPA.Qty_P, path = "output/Summary_2Step/summary.BEPA.Qty_P_gamma.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.Qty_P1, which = "Nagelkerke")
# top model was Null - stop here. 

#VAAN
# Carbon (g)
VAAN.Qty_C1 <- glm(Qty_C ~ Year*Site, family = Gamma, data = VAAN)
VAAN.Qty_C2 <- glm(Qty_C ~ Year, family = Gamma, data = VAAN)
VAAN.Qty_C3 <- glm(Qty_C ~ Site, family = Gamma, data = VAAN)
VAAN.Qty_C4 <- glm(Qty_C ~ 1, family = Gamma, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Qty_Cmodels <- list(VAAN.Qty_C1, VAAN.Qty_C2, VAAN.Qty_C3, VAAN.Qty_C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.Qty_C.residplots <- imap(VAAN.Qty_Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/VAAN_Qty_C_gamma.pdf")
VAAN.Qty_C.residplots
dev.off()
# assumptions look A LOT better. Can proceed.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.Qty_C <- list("VAAN.Qty_C1 = Year*Site" = VAAN.Qty_C1, "VAAN.Qty_C2 = Year" = VAAN.Qty_C2, "VAAN.Qty_C3 = Site" = VAAN.Qty_C3, "VAAN.Qty_C4 = Null" = VAAN.Qty_C4)
VAAN.Qty_C <- aictab(cand.set = Models.VAAN.Qty_C)
print(VAAN.Qty_C)
write.csv(VAAN.Qty_C, "output/AIC_2Step/VAAN_Qty_C_gamma.csv")
# save the summary tables of the models 
summary.VAAN.Qty_C <-map_df(Models.VAAN.Qty_C, broom::tidy, .id="model")
write_csv(summary.VAAN.Qty_C, path = "output/Summary_2Step/summary.VAAN.Qty_C_gamma.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.Qty_C1, which = "Nagelkerke")
# top model was Site - stop here.  

# Nitrogen (g)
VAAN.Qty_N1 <- glm(Qty_N ~ Year*Site, family = Gamma, data = VAAN)
VAAN.Qty_N2 <- glm(Qty_N ~ Year, family = Gamma,  data = VAAN)
VAAN.Qty_N3 <- glm(Qty_N ~ Site, family = Gamma, data = VAAN)
VAAN.Qty_N4 <- glm(Qty_N ~ 1, family = Gamma, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Qty_Nmodels <- list(VAAN.Qty_N1, VAAN.Qty_N2, VAAN.Qty_N3, VAAN.Qty_N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.Qty_N.residplots <- imap(VAAN.Qty_Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/VAAN_Qty_N_gamma.pdf")
VAAN.Qty_N.residplots
dev.off()
# assumptions look A LOT better. Can proceed.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.Qty_N <- list("VAAN.Qty_N1 = Year*Site" = VAAN.Qty_N1, "VAAN.Qty_N2 = Year" = VAAN.Qty_N2, "VAAN.Qty_N3 = Site" = VAAN.Qty_N3, "VAAN.Qty_N4 = Null" = VAAN.Qty_N4)
VAAN.Qty_N <- aictab(cand.set = Models.VAAN.Qty_N)
print(VAAN.Qty_N)
write.csv(VAAN.Qty_N, "output/AIC_2Step/VAAN_Qty_N_gamma.csv")
# save the summary tables of the models 
summary.VAAN.Qty_N <-map_df(Models.VAAN.Qty_N, broom::tidy, .id="model")
write_csv(summary.VAAN.Qty_N, path = "output/Summary_2Step/summary.VAAN.Qty_N_gamma.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.Qty_N1, which = "Nagelkerke")
# top model was Site - stop here. 

# Phosphorus (g)
VAAN.Qty_P1 <- glm(Qty_P ~ Year*Site, family = Gamma, data = VAAN)
VAAN.Qty_P2 <- glm(Qty_P ~ Year, family = Gamma, data = VAAN)
VAAN.Qty_P3 <- glm(Qty_P ~ Site, family = Gamma, data = VAAN)
VAAN.Qty_P4 <- glm(Qty_P ~ 1, family = Gamma, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Qty_Pmodels <- list(VAAN.Qty_P1, VAAN.Qty_P2, VAAN.Qty_P3, VAAN.Qty_P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.Qty_P.residplots <- imap(VAAN.Qty_Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics_GzLM/VAAN_Qty_P_gamma.pdf")
VAAN.Qty_P.residplots
dev.off()
# models do not meet assumptions - really bad fit. Will have to try a different error structure.
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.Qty_P <- list("VAAN.Qty_P1 = Year*Site" = VAAN.Qty_P1, "VAAN.Qty_P2 = Year" = VAAN.Qty_P2, "VAAN.Qty_P3 = Site" = VAAN.Qty_P3, "VAAN.Qty_P4 = Null" = VAAN.Qty_P4)
VAAN.Qty_P <- aictab(cand.set = Models.VAAN.Qty_P)
print(VAAN.Qty_P)
write.csv(VAAN.Qty_P, "output/AIC_2Step/VAAN_Qty_P_gamma.csv")
# save the summary tables of the models 
summary.VAAN.Qty_P <-map_df(Models.VAAN.Qty_P, broom::tidy, .id="model")
write_csv(summary.VAAN.Qty_P, path = "output/Summary_2Step/summary.VAAN.Qty_P_gamma.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.Qty_P1, which = "Nagelkerke")
# top model was Site - stop here