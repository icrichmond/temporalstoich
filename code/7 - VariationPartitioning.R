
#### Data Preparation ####
# load packages
easypackages::packages("vegan", "tidyverse", "patchwork")

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


#### Variation Partitioning #### 
# Overall 

stoich.C <- varpart(stoich$C, ~ Year, ~Species, ~Site, data = stoich)
stoich.N <- varpart(stoich$N, ~ Year, ~Species, ~Site, data = stoich)
stoich.P <- varpart(stoich$P, ~ Year, ~Species, ~Site, data = stoich)
stoich.CN <- varpart(stoich$CNRatio, ~ Year, ~Species, ~Site, data = stoich)
stoich.CP <- varpart(stoich$CPRatio, ~ Year, ~Species, ~Site, data = stoich)
stoich.NP <- varpart(stoich$NPRatio, ~ Year, ~Species, ~Site, data = stoich)

par(mfrow=c(2,3), mar=c(0.25,0.25,0.25,0.25))
c <- plot(stoich.C, bg = c("#a39193", "#aa6f73", "#eea990"), Xnames = c("Year", "Species", "Site"), cutoff = -Inf)
n <- plot(stoich.N, bg = c("#a39193", "#aa6f73", "#eea990"), Xnames = c("Year", "Species", "Site"), cutoff = -Inf)
p <- plot(stoich.P, bg = c("#a39193", "#aa6f73", "#eea990"), Xnames = c("Year", "Species", "Site"), cutoff = -Inf)
cn <- plot(stoich.CN, bg = c("#a39193", "#aa6f73", "#eea990"), Xnames = c("Year", "Species", "Site"), cutoff = -Inf)
cp <- plot(stoich.CP, bg = c("#a39193", "#aa6f73", "#eea990"), Xnames = c("Year", "Species", "Site"), cutoff = -Inf)
np <- plot(stoich.NP, bg = c("#a39193", "#aa6f73", "#eea990"), Xnames = c("Year", "Species", "Site"), cutoff = -Inf)
