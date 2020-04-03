# Author: Isabella Richmond
# Last edited: April 3, 2020

# This code was for the creation and evaluation of my temporal stoichiometry models. A lot of 
# code was provided by Travis Heckford (twitter.com/travheckford)
# Models evaluate the response of stoichiometry in four boreal plant species with year and site 
# If year is found to be in the top model when compared using AICc, another model is conducted 
# where mechanisms are investigated (productivity, site, moisture, weather)
# These models are specifically for % data that has been restandardized to reduce any size effect
# of carbon vs nitrogen and phosphorus 

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

#### Create Functions #### 
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

#### Standardize Percent Data ####
# we are going to standardize the percent data and rerun models to make sure that we are not 
# getting an effect with %C just because the values are higher
# get the mean and sd for each % C, N, P for ABBA, ACRU, BEPA, & VAAN
abba.mean.C <- mean(ABBA$C)
abba.std.C <- sd(ABBA$C)
abba.mean.N <- mean(ABBA$N)
abba.std.N <- sd(ABBA$N)
abba.mean.P <- mean(ABBA$P)
abba.std.P <- sd(ABBA$P)

acru.mean.C <- mean(ACRU$C)
acru.std.C <- sd(ACRU$C)
acru.mean.N <- mean(ACRU$N)
acru.std.N <- sd(ACRU$N)
acru.mean.P <- mean(ACRU$P)
acru.std.P <- sd(ACRU$P)

bepa.mean.C <- mean(BEPA$C)
bepa.std.C <- sd(BEPA$C)
bepa.mean.N <- mean(BEPA$N)
bepa.std.N <- sd(BEPA$N)
bepa.mean.P <- mean(BEPA$P)
bepa.std.P <- sd(BEPA$P)

vaan.mean.C <- mean(VAAN$C)
vaan.std.C <- sd(VAAN$C)
vaan.mean.N <- mean(VAAN$N)
vaan.std.N <- sd(VAAN$N)
vaan.mean.P <- mean(VAAN$P)
vaan.std.P <- sd(VAAN$P)
# then, we standardize the values extracted from each home range against the mean 
# and SD values just calculated. 
options(tibble.print_max = 10, tibble.width= Inf)
ABBA <- ABBA %>% as_tibble() %>% mutate(
  C_std = ((C-abba.mean.C)/abba.std.C),
  N_std = ((N-abba.mean.N)/abba.std.N),
  P_std = ((P-abba.mean.P)/abba.std.P)
)
ACRU <- ACRU %>% as_tibble() %>% mutate(
  C_std = ((C-acru.mean.C)/acru.std.C),
  N_std = ((N-acru.mean.N)/acru.std.N),
  P_std = ((P-acru.mean.P)/acru.std.P)
)
BEPA <- BEPA %>% as_tibble() %>% mutate(
  C_std = ((C-bepa.mean.C)/bepa.std.C),
  N_std = ((N-bepa.mean.N)/bepa.std.N),
  P_std = ((P-bepa.mean.P)/bepa.std.P)
)
VAAN <- VAAN %>% as_tibble() %>% mutate(
  C_std = ((C-vaan.mean.C)/vaan.std.C),
  N_std = ((N-vaan.mean.N)/vaan.std.N),
  P_std = ((P-vaan.mean.P)/vaan.std.P)
)
# now run the models with the standardized data 

#### Models ####
# models are run individually for each species
# using AICc to evaluate standardized % models variables and combinations of models 
# evaluate with only Site and Year first, then if Year is in top model continue with 
# EVI, NDMI, GDD, etc. 

# ABBA
# % Carbon
ABBA.C1 <- glm(C_std ~ Year*Site, data = ABBA)
ABBA.C2 <- glm(C_std ~ Year, data = ABBA)
ABBA.C3 <- glm(C_std ~ Site, data = ABBA)
ABBA.C4 <- glm(C_std ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Cmodels <- list(ABBA.C1, ABBA.C2, ABBA.C3, ABBA.C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.C.residplots <- imap(ABBA.Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_C_std_glm.pdf")
ABBA.C.residplots
dev.off()
# assumptions are ok, could try another error structure to check
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.C <- list("ABBA.C1 = Year*Site" = ABBA.C1, "ABBA.C2 = Year" = ABBA.C2, "ABBA.C3 = Site" = ABBA.C3, "ABBA.C4 = Null" = ABBA.C4)
ABBA.C <- aictab(cand.set = Models.ABBA.C)
print(ABBA.C)
write.csv(ABBA.C, "output/AIC_2Step/ABBA_C_std.csv")
# save the summary tables of the models 
summary.ABBA.C <-map_df(Models.ABBA.C, broom::tidy, .id="model")
write_csv(summary.ABBA.C, path = "output/Summary_2Step/summary.ABBA.C.std.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.C1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.C.Global <- glm(C_std ~ EVI * GDD * NDMI * Site, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.C.Global <- glm(C_std ~ EVI*GDD*NDMI*Site, data = ABBA)
ABBA.C.mech <- dredge(ABBA.C.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI | EVI*GDD*Site | EVI*NDMI*Site | GDD*NDMI*Site | EVI*GDD*NDMI*Site))
# check the residuals of the models to ensure that glm was correct choice 
ABBA.C.mechmodels <- get.models(ABBA.C.mech,subset=NA)
ABBA.C.mech.residplots <- imap(ABBA.C.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_C_std_mech_glm.pdf")
ABBA.C.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.C.mech)
# save the AIC table
write_csv(ABBA.C.mech, "output/AIC_2Step/ABBA_C_std_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ABBA.C.std.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.C.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.C.mechtop <- (get.models(ABBA.C.mech, 1)[[1]])
ABBA.C.mechtop
GABBA.C.mechtop <- tidy(ABBA.C.mechtop)
write_csv(ABBA.C.mechtop, "output/Summary_2Step/summary.ABBA.C.std.mech.csv")

# % Nitrogen
ABBA.N1 <- glm(N_std ~ Year*Site, data = ABBA)
ABBA.N2 <- glm(N_std ~ Year, data = ABBA)
ABBA.N3 <- glm(N_std ~ Site, data = ABBA)
ABBA.N4 <- glm(N_std ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Nmodels <- list(ABBA.N1, ABBA.N2, ABBA.N3, ABBA.N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.N.residplots <- imap(ABBA.Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_N_std_glm.pdf")
ABBA.N.residplots
dev.off()
# assumptions are ok, could use another error structure to check
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.N <- list("ABBA.N1 = Year*Site" = ABBA.N1, "ABBA.N2 = Year" = ABBA.N2, "ABBA.N3 = Site" = ABBA.N3, "ABBA.N4 = Null" = ABBA.N4)
ABBA.N <- aictab(cand.set = Models.ABBA.N)
print(ABBA.N)
write.csv(ABBA.N, "output/AIC_2Step/ABBA_N_std.csv")
# save the summary tables of the models 
summary.ABBA.N <-map_df(Models.ABBA.N, broom::tidy, .id="model")
write_csv(summary.ABBA.N, path = "output/Summary_2Step/summary.ABBA.N.std.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.N1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.N.Global <- glm(N_std ~ EVI * GDD * NDMI * Site, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.N.mech <- dredge(ABBA.N.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI | EVI*GDD*Site | EVI*NDMI*Site | GDD*NDMI*Site | EVI*GDD*NDMI*Site))
# check the residuals of the models to ensure that glm was correct choice 
ABBA.N.mechmodels <- get.models(ABBA.N.mech,subset=NA)
ABBA.N.mech.residplots <- imap(ABBA.N.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_N_std_mech_glm.pdf")
ABBA.N.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ABBA.N.mech)
# save the AIC table
write_csv(ABBA.N.mech, "output/AIC_2Step/ABBA_N_std_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ABBA.N.std.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.N.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.N.mechtop <- (get.models(ABBA.N.mech, 1)[[1]])
ABBA.N.mechtop <- tidy(ABBA.N.mechtop)
ABBA.N.mechtop
write_csv(ABBA.N.mechtop, "output/Summary_2Step/summary.ABBA.N.std.mech.csv")

# % Phosphorus
ABBA.P1 <- glm(P_std ~ Year*Site, data = ABBA)
ABBA.P2 <- glm(P_std ~ Year, data = ABBA)
ABBA.P3 <- glm(P_std ~ Site, data = ABBA)
ABBA.P4 <- glm(P_std ~ 1, data = ABBA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ABBA.Pmodels <- list(ABBA.P1, ABBA.P2, ABBA.P3, ABBA.P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ABBA.P.residplots <- imap(ABBA.Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_P_std_glm.pdf")
ABBA.P.residplots
dev.off()
# assumptions are ok, would use another error structure to check
# create an AIPc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ABBA.P <- list("ABBA.P1 = Year*Site" = ABBA.P1, "ABBA.P2 = Year" = ABBA.P2, "ABBA.P3 = Site" = ABBA.P3, "ABBA.P4 = Null" = ABBA.P4)
ABBA.P <- aictab(cand.set = Models.ABBA.P)
print(ABBA.P)
write.csv(ABBA.P, "output/AIC_2Step/ABBA_P_std.csv")
# save the summary tables of the models 
summary.ABBA.P <-map_df(Models.ABBA.P, broom::tidy, .id="model")
write_csv(summary.ABBA.P, path = "output/Summary_2Step/summary.ABBA.P.std.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ABBA.P1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ABBA.P.Global <- glm(P_std ~ EVI * GDD * NDMI * Site, data = ABBA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ABBA.P.mech <- dredge(ABBA.P.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI | EVI*GDD*Site | EVI*NDMI*Site | GDD*NDMI*Site | EVI*GDD*NDMI*Site))
# check the residuals of the models to ensure that glm was correct choice 
ABBA.P.mechmodels <- get.models(ABBA.P.mech,subset=NA)
ABBA.P.mech.residplots <- imap(ABBA.P.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ABBA_P_std_mech_glm.pdf")
ABBA.P.mech.residplots
dev.off()
# if assumptions are met, proceed with AIP table and analysis
# look at the AIP table
print(ABBA.P.mech)
# save the AIC table
write_csv(ABBA.P.mech, "output/AIC_2Step/ABBA_P_std_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ABBA.P.std.pdf")
par(mar=c(4,5,9,4))
plot(ABBA.P.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ABBA.P.mechtop <- (get.models(ABBA.P.mech, 1)[[1]])
ABBA.P.mechtop <- tidy(ABBA.P.mechtop)
ABBA.P.mechtop
write_csv(ABBA.P.mechtop, "output/Summary_2Step/summary.ABBA.P.std.mech.csv")

# ACRU
# % Carbon
ACRU.C1 <- glm(C_std ~ Year*Site, data = ACRU)
ACRU.C2 <- glm(C_std ~ Year, data = ACRU)
ACRU.C3 <- glm(C_std ~ Site, data = ACRU)
ACRU.C4 <- glm(C_std ~ 1, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Cmodels <- list(ACRU.C1, ACRU.C2, ACRU.C3, ACRU.C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.C.residplots <- imap(ACRU.Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_C_std_glm.pdf")
ACRU.C.residplots
dev.off()
# assumptions are ok, could try another error structure to check
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.C <- list("ACRU.C1 = Year*Site" = ACRU.C1, "ACRU.C2 = Year" = ACRU.C2, "ACRU.C3 = Site" = ACRU.C3, "ACRU.C4 = Null" = ACRU.C4)
ACRU.C <- aictab(cand.set = Models.ACRU.C)
print(ACRU.C)
write.csv(ACRU.C, "output/AIC_2Step/ACRU_C_std.csv")
# save the summary tables of the models 
summary.ACRU.C <-map_df(Models.ACRU.C, broom::tidy, .id="model")
write_csv(summary.ACRU.C, path = "output/Summary_2Step/summary.ACRU.C.std.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.C1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
ACRU.C.Global <- glm(C_std ~ EVI * GDD * NDMI * Site, data = ACRU)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
ACRU.C.Global <- glm(C_std ~ EVI*GDD*NDMI*Site, data = ACRU)
ACRU.C.mech <- dredge(ACRU.C.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI | EVI*GDD*Site | EVI*NDMI*Site | GDD*NDMI*Site | EVI*GDD*NDMI*Site))
# check the residuals of the models to ensure that glm was correct choice 
ACRU.C.mechmodels <- get.models(ACRU.C.mech,subset=NA)
ACRU.C.mech.residplots <- imap(ACRU.C.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_C_std_mech_glm.pdf")
ACRU.C.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(ACRU.C.mech)
# save the AIC table
write_csv(ACRU.C.mech, "output/AIC_2Step/ACRU_C_std_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/ACRU.C.std.pdf")
par(mar=c(4,5,9,4))
plot(ACRU.C.mech)
dev.off()
# get the summary of the top model and save it to a .csv
ACRU.C.mechtop <- (get.models(ACRU.C.mech, 1)[[1]])
ACRU.C.mechtop
ACRU.C.mechtop <- tidy(ACRU.C.mechtop)
write_csv(ACRU.C.mechtop, "output/Summary_2Step/summary.ACRU.C.std.mech.csv")

# % Nitrogen
ACRU.N1 <- glm(N_std ~ Year*Site, data = ACRU)
ACRU.N2 <- glm(N_std ~ Year, data = ACRU)
ACRU.N3 <- glm(N_std ~ Site, data = ACRU)
ACRU.N4 <- glm(N_std ~ 1, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Nmodels <- list(ACRU.N1, ACRU.N2, ACRU.N3, ACRU.N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.N.residplots <- imap(ACRU.Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_N_std_glm.pdf")
ACRU.N.residplots
dev.off()
# assumptions are ok, could use another error structure to check
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.N <- list("ACRU.N1 = Year*Site" = ACRU.N1, "ACRU.N2 = Year" = ACRU.N2, "ACRU.N3 = Site" = ACRU.N3, "ACRU.N4 = Null" = ACRU.N4)
ACRU.N <- aictab(cand.set = Models.ACRU.N)
print(ACRU.N)
write.csv(ACRU.N, "output/AIC_2Step/ACRU_N_std.csv")
# save the summary tables of the models 
summary.ACRU.N <-map_df(Models.ACRU.N, broom::tidy, .id="model")
write_csv(summary.ACRU.N, path = "output/Summary_2Step/summary.ACRU.N.std.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.N1, which = "Nagelkerke")
# this model has Year as top model but Null is within 2 delta AICc, stop here. 

# % Phosphorus
ACRU.P1 <- glm(P_std ~ Year*Site, data = ACRU)
ACRU.P2 <- glm(P_std ~ Year, data = ACRU)
ACRU.P3 <- glm(P_std ~ Site, data = ACRU)
ACRU.P4 <- glm(P_std ~ 1, data = ACRU)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
ACRU.Pmodels <- list(ACRU.P1, ACRU.P2, ACRU.P3, ACRU.P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
ACRU.P.residplots <- imap(ACRU.Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/ACRU_P_std_glm.pdf")
ACRU.P.residplots
dev.off()
# assumptions are ok, would use another error structure to check
# create an AIPc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.ACRU.P <- list("ACRU.P1 = Year*Site" = ACRU.P1, "ACRU.P2 = Year" = ACRU.P2, "ACRU.P3 = Site" = ACRU.P3, "ACRU.P4 = Null" = ACRU.P4)
ACRU.P <- aictab(cand.set = Models.ACRU.P)
print(ACRU.P)
write.csv(ACRU.P, "output/AIC_2Step/ACRU_P_std.csv")
# save the summary tables of the models 
summary.ACRU.P <-map_df(Models.ACRU.P, broom::tidy, .id="model")
write_csv(summary.ACRU.P, path = "output/Summary_2Step/summary.ACRU.P.std.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(ACRU.P1, which = "Nagelkerke")
# Null is top model, stop here

# BEPA
# % Carbon
BEPA.C1 <- glm(C_std ~ Year*Site, data = BEPA)
BEPA.C2 <- glm(C_std ~ Year, data = BEPA)
BEPA.C3 <- glm(C_std ~ Site, data = BEPA)
BEPA.C4 <- glm(C_std ~ 1, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Cmodels <- list(BEPA.C1, BEPA.C2, BEPA.C3, BEPA.C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.C.residplots <- imap(BEPA.Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_C_std_glm.pdf")
BEPA.C.residplots
dev.off()
# assumptions are ok, could try another error structure to check
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.C <- list("BEPA.C1 = Year*Site" = BEPA.C1, "BEPA.C2 = Year" = BEPA.C2, "BEPA.C3 = Site" = BEPA.C3, "BEPA.C4 = Null" = BEPA.C4)
BEPA.C <- aictab(cand.set = Models.BEPA.C)
print(BEPA.C)
write.csv(BEPA.C, "output/AIC_2Step/BEPA_C_std.csv")
# save the summary tables of the models 
summary.BEPA.C <-map_df(Models.BEPA.C, broom::tidy, .id="model")
write_csv(summary.BEPA.C, path = "output/Summary_2Step/summary.BEPA.C.std.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.C1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
BEPA.C.Global <- glm(C_std ~ EVI * GDD * NDMI * Site, data = BEPA)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
BEPA.C.Global <- glm(C_std ~ EVI*GDD*NDMI*Site, data = BEPA)
BEPA.C.mech <- dredge(BEPA.C.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI | EVI*GDD*Site | EVI*NDMI*Site | GDD*NDMI*Site | EVI*GDD*NDMI*Site))
# check the residuals of the models to ensure that glm was correct choice 
BEPA.C.mechmodels <- get.models(BEPA.C.mech,subset=NA)
BEPA.C.mech.residplots <- imap(BEPA.C.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_C_std_mech_glm.pdf")
BEPA.C.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(BEPA.C.mech)
# save the AIC table
write_csv(BEPA.C.mech, "output/AIC_2Step/BEPA_C_std_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/BEPA.C.std.pdf")
par(mar=c(4,5,9,4))
plot(BEPA.C.mech)
dev.off()
# get the summary of the top model and save it to a .csv
BEPA.C.mechtop <- (get.models(BEPA.C.mech, 1)[[1]])
BEPA.C.mechtop
BEPA.C.mechtop <- tidy(BEPA.C.mechtop)
write_csv(BEPA.C.mechtop, "output/Summary_2Step/summary.BEPA.C.std.mech.csv")
# % Nitrogen
BEPA.N1 <- glm(N_std ~ Year*Site, data = BEPA)
BEPA.N2 <- glm(N_std ~ Year, data = BEPA)
BEPA.N3 <- glm(N_std ~ Site, data = BEPA)
BEPA.N4 <- glm(N_std ~ 1, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Nmodels <- list(BEPA.N1, BEPA.N2, BEPA.N3, BEPA.N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.N.residplots <- imap(BEPA.Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_N_std_glm.pdf")
BEPA.N.residplots
dev.off()
# assumptions are ok, could use another error structure to check
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.N <- list("BEPA.N1 = Year*Site" = BEPA.N1, "BEPA.N2 = Year" = BEPA.N2, "BEPA.N3 = Site" = BEPA.N3, "BEPA.N4 = Null" = BEPA.N4)
BEPA.N <- aictab(cand.set = Models.BEPA.N)
print(BEPA.N)
write.csv(BEPA.N, "output/AIC_2Step/BEPA_N_std.csv")
# save the summary tables of the models 
summary.BEPA.N <-map_df(Models.BEPA.N, broom::tidy, .id="model")
write_csv(summary.BEPA.N, path = "output/Summary_2Step/summary.BEPA.N.std.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.N1, which = "Nagelkerke")
# Site was top model with Null within 2 delta AICc
# % Phosphorus
BEPA.P1 <- glm(P_std ~ Year*Site, data = BEPA)
BEPA.P2 <- glm(P_std ~ Year, data = BEPA)
BEPA.P3 <- glm(P_std ~ Site, data = BEPA)
BEPA.P4 <- glm(P_std ~ 1, data = BEPA)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
BEPA.Pmodels <- list(BEPA.P1, BEPA.P2, BEPA.P3, BEPA.P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
BEPA.P.residplots <- imap(BEPA.Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/BEPA_P_std_glm.pdf")
BEPA.P.residplots
dev.off()
# assumptions are ok, would use another error structure to check
# create an AIPc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.BEPA.P <- list("BEPA.P1 = Year*Site" = BEPA.P1, "BEPA.P2 = Year" = BEPA.P2, "BEPA.P3 = Site" = BEPA.P3, "BEPA.P4 = Null" = BEPA.P4)
BEPA.P <- aictab(cand.set = Models.BEPA.P)
print(BEPA.P)
write.csv(BEPA.P, "output/AIC_2Step/BEPA_P_std.csv")
# save the summary tables of the models 
summary.BEPA.P <-map_df(Models.BEPA.P, broom::tidy, .id="model")
write_csv(summary.BEPA.P, path = "output/Summary_2Step/summary.BEPA.P.std.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(BEPA.P1, which = "Nagelkerke")
# Site is the top model, stop here.

# VAAN
# % Carbon
VAAN.C1 <- glm(C_std ~ Year*Site, data = VAAN)
VAAN.C2 <- glm(C_std ~ Year, data = VAAN)
VAAN.C3 <- glm(C_std ~ Site, data = VAAN)
VAAN.C4 <- glm(C_std ~ 1, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Cmodels <- list(VAAN.C1, VAAN.C2, VAAN.C3, VAAN.C4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.C.residplots <- imap(VAAN.Cmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_C_std_glm.pdf")
VAAN.C.residplots
dev.off()
# assumptions are ok, could try another error structure to check
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.C <- list("VAAN.C1 = Year*Site" = VAAN.C1, "VAAN.C2 = Year" = VAAN.C2, "VAAN.C3 = Site" = VAAN.C3, "VAAN.C4 = Null" = VAAN.C4)
VAAN.C <- aictab(cand.set = Models.VAAN.C)
print(VAAN.C)
write.csv(VAAN.C, "output/AIC_2Step/VAAN_C_std.csv")
# save the summary tables of the models 
summary.VAAN.C <-map_df(Models.VAAN.C, broom::tidy, .id="model")
write_csv(summary.VAAN.C, path = "output/Summary_2Step/summary.VAAN.C.std.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.C1, which = "Nagelkerke")
# this model has Year in the top model, move on to testing the mechanisms
# use dredge package and keep interaction terms to max 2 
# build global model with all mechanisms and interactions 
VAAN.C.Global <- glm(C_std ~ EVI * GDD * NDMI * Site, data = VAAN)
# set options, dredge requires this 
options(na.action = "na.fail")
# create AICc table ranking models with dredge. Subset the models to remove three-way 
# interaction terms 
VAAN.C.Global <- glm(C_std ~ EVI*GDD*NDMI*Site, data = VAAN)
VAAN.C.mech <- dredge(VAAN.C.Global, evaluate = TRUE, rank = "AICc", subset = !(EVI*GDD*NDMI | EVI*GDD*Site | EVI*NDMI*Site | GDD*NDMI*Site | EVI*GDD*NDMI*Site))
# check the residuals of the models to ensure that glm was correct choice 
VAAN.C.mechmodels <- get.models(VAAN.C.mech,subset=NA)
VAAN.C.mech.residplots <- imap(VAAN.C.mechmodels, resid_plots) 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_C_std_mech_glm.pdf")
VAAN.C.mech.residplots
dev.off()
# if assumptions are met, proceed with AIC table and analysis
# look at the AIC table
print(VAAN.C.mech)
# save the AIC table
write_csv(VAAN.C.mech, "output/AIC_2Step/VAAN_C_std_Mech.csv")
# visualize the AIC table 
pdf("graphics/StoichModels_2Step/AIC/VAAN.C.std.pdf")
par(mar=c(4,5,9,4))
plot(VAAN.C.mech)
dev.off()
# get the summary of the top model and save it to a .csv
VAAN.C.mechtop <- (get.models(VAAN.C.mech, 1)[[1]])
VAAN.C.mechtop
VAAN.C.mechtop <- tidy(VAAN.C.mechtop)
write_csv(VAAN.C.mechtop, "output/Summary_2Step/summary.VAAN.C.std.mech.csv")
# % Nitrogen
VAAN.N1 <- glm(N_std ~ Year*Site, data = VAAN)
VAAN.N2 <- glm(N_std ~ Year, data = VAAN)
VAAN.N3 <- glm(N_std ~ Site, data = VAAN)
VAAN.N4 <- glm(N_std ~ 1, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Nmodels <- list(VAAN.N1, VAAN.N2, VAAN.N3, VAAN.N4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.N.residplots <- imap(VAAN.Nmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_N_std_glm.pdf")
VAAN.N.residplots
dev.off()
# assumptions are ok, could use another error structure to check
# create an AICc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.N <- list("VAAN.N1 = Year*Site" = VAAN.N1, "VAAN.N2 = Year" = VAAN.N2, "VAAN.N3 = Site" = VAAN.N3, "VAAN.N4 = Null" = VAAN.N4)
VAAN.N <- aictab(cand.set = Models.VAAN.N)
print(VAAN.N)
write.csv(VAAN.N, "output/AIC_2Step/VAAN_N_std.csv")
# save the summary tables of the models 
summary.VAAN.N <-map_df(Models.VAAN.N, broom::tidy, .id="model")
write_csv(summary.VAAN.N, path = "output/Summary_2Step/summary.VAAN.N.std.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.N1, which = "Nagelkerke")
# Null is top model, stop here.
# % Phosphorus
VAAN.P1 <- glm(P_std ~ Year*Site, data = VAAN)
VAAN.P2 <- glm(P_std ~ Year, data = VAAN)
VAAN.P3 <- glm(P_std ~ Site, data = VAAN)
VAAN.P4 <- glm(P_std ~ 1, data = VAAN)
# check model diagnostics to make sure models are not violating any assumptions 
# create list of models 
VAAN.Pmodels <- list(VAAN.P1, VAAN.P2, VAAN.P3, VAAN.P4)
# use imap to loop through list of models using function at start of script and 
# create diagnostic figures 
VAAN.P.residplots <- imap(VAAN.Pmodels, resid_plots) 
# save all diagnostic plots to a pdf 
pdf("graphics/StoichModels_2Step/ModelDiagnostics/VAAN_P_std_glm.pdf")
VAAN.P.residplots
dev.off()
# assumptions are ok, would use another error structure to check
# create an AIPc table to show the "best model" to use as a prediction of spatial stoichiometry
Models.VAAN.P <- list("VAAN.P1 = Year*Site" = VAAN.P1, "VAAN.P2 = Year" = VAAN.P2, "VAAN.P3 = Site" = VAAN.P3, "VAAN.P4 = Null" = VAAN.P4)
VAAN.P <- aictab(cand.set = Models.VAAN.P)
print(VAAN.P)
write.csv(VAAN.P, "output/AIC_2Step/VAAN_P_std.csv")
# save the summary tables of the models 
summary.VAAN.P <-map_df(Models.VAAN.P, broom::tidy, .id="model")
write_csv(summary.VAAN.P, path = "output/Summary_2Step/summary.VAAN.P.std.csv")
# calculate pseudo R^2 - just another check of significance determination
PseudoR2(VAAN.P1, which = "Nagelkerke")
# Site is top model, stop here 

# After running standardized models, results are the same as with the non-standardized data. 
# %C has Year in top model for all species, ABBA has Year in %C, N and P 
