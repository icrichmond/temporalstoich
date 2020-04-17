# Author: Isabella Richmond
# Last edited: April 2, 2020
# Reference codes: Stoich_TemporalModels_2step.R and Stoich_GzLMTemporalModels_2step.R 

# This code was for the visualization of our data and temporal stoichiometry models.
# A large portion of boxplot code provided by Juliana Balluffi-Fry and Matteo Rizzuto
# Run codes in source scripts to see models
# Models evaluate the response of stoichiometry in four boreal plant species with year and site 
# If year is found to be in the top model when compared using AICc, another model is conducted 
# where mechanisms are investigated (productivity, site, moisture, weather)

#### Data Preparation #### 
# load packages
install.packages("easypackages")
library(easypackages)
install_packages("purrr", "ggcorrplot", "broom", "patchwork")
libraries("reshape2", "purrr", "patchwork", "broom", "ggcorrplot", "ggplot2","dplyr", "tibble", "readr", "plyr", "ggpol", "ggpubr")
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
# subset by species
# ABBA = Abies balsamea, balsam fir 
# ACRU = Acer rubrum, red maple 
# BEPA = Betula papyrifera, white birch 
# VAAN = Vaccinium angustifolium, lowland blueberry
ABBA <- subset(stoich, Species == "ABBA")
ACRU <- subset(stoich, Species == "ACRU")
BEPA <- subset(stoich, Species == "BEPA")
VAAN <- subset(stoich, Species == "VAAN")

#### Visualize the models ####
# going to make boxplots for variables across years for each species
# use color-hex.com to choose colour palettes and Color Oracle app
# to ensure that they are color-blind friendly 
# ABBA
abba.c.box <- ggboxplot(ABBA, x = "Year", y = "C", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"),
                        add = "jitter", xlab = "Year", ylab = "% Carbon")
abba.n.box <- ggboxplot(ABBA, x = "Year", y = "N", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter", xlab = "Year", ylab = "% Nitrogen")
abba.p.box <- ggboxplot(ABBA, x = "Year", y = "P", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter", xlab = "Year", ylab = "% Phosphorus")
abba.qty.c.box <- ggboxplot(ABBA, x = "Year", y = "Qty_C", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter", xlab = "Year", ylab = "Carbon (g)")
abba.qty.n.box <- ggboxplot(ABBA, x = "Year", y = "Qty_N", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                            add = "jitter", xlab = "Year", ylab = "Nitrogen (g)")
abba.qty.p.box <- ggboxplot(ABBA, x = "Year", y = "Qty_P", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                            add = "jitter", xlab = "Year", ylab = "Phosphorus (g)")
abba.cn.box <- ggboxplot(ABBA, x = "Year", y = "CNRatio", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                            add = "jitter", xlab = "Year", ylab = "Carbon:Nitrogen")
abba.cp.box <- ggboxplot(ABBA, x = "Year", y = "CPRatio", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                         add = "jitter", xlab = "Year", ylab = "Carbon:Phosphorus")
abba.np.box <- ggboxplot(ABBA, x = "Year", y = "NPRatio", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                         add = "jitter", xlab = "Year", ylab = "Nitrogen:Phosphorus")
#ACRU
acru.c.box <- ggboxplot(ACRU, x = "Year", y = "C", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter",  xlab = "Year", ylab = "% Carbon")
acru.n.box <- ggboxplot(ACRU, x = "Year", y = "N", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter", xlab = "Year", ylab = "% Nitrogen")
acru.p.box <- ggboxplot(ACRU, x = "Year", y = "P", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter", xlab = "Year", ylab = "% Phosphorus")
acru.qty.c.box <- ggboxplot(ACRU, x = "Year", y = "Qty_C", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                            add = "jitter", xlab = "Year", ylab = "Carbon (g)")
acru.qty.n.box <- ggboxplot(ACRU, x = "Year", y = "Qty_N", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                            add = "jitter",  xlab = "Year", ylab = "Nitrogen (g)")
acru.qty.p.box <- ggboxplot(ACRU, x = "Year", y = "Qty_P", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                            add = "jitter", xlab = "Year", ylab = "Phosphorus (g)")
acru.cn.box <- ggboxplot(ACRU, x = "Year", y = "CNRatio", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                         add = "jitter",  xlab = "Year", ylab = "Carbon:Nitrogen")
acru.cp.box <- ggboxplot(ACRU, x = "Year", y = "CPRatio", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                         add = "jitter",xlab = "Year", ylab = "Carbon:Phosphorus")
acru.np.box <- ggboxplot(ACRU, x = "Year", y = "NPRatio", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                         add = "jitter", xlab = "Year", ylab = "Nitrogen:Phosphorus")

#BEPA
bepa.c.box <- ggboxplot(BEPA, x = "Year", y = "C", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter", xlab = "Year", ylab = "% Carbon")
bepa.n.box <- ggboxplot(BEPA, x = "Year", y = "N", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter",  xlab = "Year", ylab = "% Nitrogen")
bepa.p.box <- ggboxplot(BEPA, x = "Year", y = "P", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter",  xlab = "Year", ylab = "% Phosphorus")
bepa.qty.c.box <- ggboxplot(BEPA, x = "Year", y = "Qty_C", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                            add = "jitter", xlab = "Year", ylab = "Carbon (g)")
bepa.qty.n.box <- ggboxplot(BEPA, x = "Year", y = "Qty_N", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                            add = "jitter",  xlab = "Year", ylab = "Nitrogen (g)")
bepa.qty.p.box <- ggboxplot(BEPA, x = "Year", y = "Qty_P", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                            add = "jitter",  xlab = "Year", ylab = "Phosphorus (g)")
bepa.cn.box <- ggboxplot(BEPA, x = "Year", y = "CNRatio", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                         add = "jitter",  xlab = "Year", ylab = "Carbon:Nitrogen")
bepa.cp.box <- ggboxplot(BEPA, x = "Year", y = "CPRatio", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                         add = "jitter",  xlab = "Year", ylab = "Carbon:Phosphorus")
bepa.np.box <- ggboxplot(BEPA, x = "Year", y = "NPRatio", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                         add = "jitter", xlab = "Year", ylab = "Nitrogen:Phosphorus")
#VAAN
vaan.c.box <- ggboxplot(VAAN, x = "Year", y = "C", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter",  xlab = "Year", ylab = "% Carbon")
vaan.n.box <- ggboxplot(VAAN, x = "Year", y = "N", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter",  xlab = "Year", ylab = "% Nitrogen")
vaan.p.box <- ggboxplot(VAAN, x = "Year", y = "P", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter",  xlab = "Year", ylab = "% Phosphorus")
vaan.qty.c.box <- ggboxplot(VAAN, x = "Year", y = "Qty_C", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                            add = "jitter", xlab = "Year", ylab = "Carbon (g)")
vaan.qty.n.box <- ggboxplot(VAAN, x = "Year", y = "Qty_N", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                            add = "jitter", xlab = "Year", ylab = "Nitrogen (g)")
vaan.qty.p.box <- ggboxplot(VAAN, x = "Year", y = "Qty_P", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                            add = "jitter",  xlab = "Year", ylab = "Phosphorus (g)")
vaan.cn.box <- ggboxplot(VAAN, x = "Year", y = "CNRatio", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                         add = "jitter",  xlab = "Year", ylab = "Carbon:Nitrogen")
vaan.cp.box <- ggboxplot(VAAN, x = "Year", y = "CPRatio", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                         add = "jitter", xlab = "Year", ylab = "Carbon:Phosphorus")
vaan.np.box <- ggboxplot(VAAN, x = "Year", y = "NPRatio", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                         add = "jitter", xlab = "Year", ylab = "Nitrogen:Phosphorus")

# also need boxplots of explanatory variables for the second models (EVI, NDMI, GDD) 
# across years. Group the data points by site.
evi.box <- ggboxplot(stoich, x = "Year", y = "EVI", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter",  xlab = "Year", ylab = "EVI")
gdd.box <- ggboxplot(VAAN, x = "Year", y = "GDD", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter",  xlab = "Year", ylab = "GDD")
ndmi.box <- ggboxplot(VAAN, x = "Year", y = "NDMI", color = "Site", palette = c("#66545e", "#a39193", "#aa6f73", "#eea990"), 
                        add = "jitter",  xlab = "Year", ylab = "NDMI")

# now save boxplots in required combinations for my slides using patchwork
# ABBA
png("graphics/StoichModels_2Step/Boxplots/ABBApercentbox.png", width = 900, height = 700)
(abba.c.box | abba.n.box | abba.p.box) + 
  plot_annotation(title = "Balsam Fir")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/ABBAqtybox.png", width = 900, height = 700)
(abba.qty.c.box | abba.qty.n.box | abba.qty.p.box) + 
  plot_annotation(title = "Balsam Fir")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/ABBAratiobox.png", width = 900, height = 700)
(abba.cn.box | abba.cp.box | abba.np.box) + 
  plot_annotation(title = "Balsam Fir")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/ABBAfull.png", width = 900, height = 700)
(abba.c.box / abba.qty.c.box / abba.cn.box | abba.n.box / abba.qty.n.box / abba.cp.box | abba.p.box/abba.qty.p.box/abba.np.box
  ) + 
  plot_annotation(title = "Balsam Fir")
dev.off()

# ACRU
png("graphics/StoichModels_2Step/Boxplots/ACRUpercentbox.png", width = 900, height = 700)
(acru.c.box | acru.n.box | acru.p.box) + 
  plot_annotation(title = "Red Maple")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/ACRUqtybox.png", width = 900, height = 700)
(acru.qty.c.box | acru.qty.n.box | acru.qty.p.box) + 
  plot_annotation(title = "Red Maple")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/ACRUratiobox.png", width = 900, height = 700)
(acru.cn.box | acru.cp.box | acru.np.box) + 
  plot_annotation(title = "Red Maple")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/ACRUfull.png", width = 900, height = 700)
(acru.c.box / acru.qty.c.box / acru.cn.box | acru.n.box / acru.qty.n.box / acru.cp.box | acru.p.box/acru.qty.p.box/acru.np.box
) + 
  plot_annotation(title = "Red Maple")
dev.off()

# BEPA
png("graphics/StoichModels_2Step/Boxplots/BEPApercentbox.png", width = 900, height = 700)
(bepa.c.box | bepa.n.box | bepa.p.box) + 
  plot_annotation(title = "White Birch")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/BEPAqtybox.png", width = 900, height = 700)
(bepa.qty.c.box | bepa.qty.n.box | bepa.qty.p.box) + 
  plot_annotation(title = "White Birch")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/BEPAratiobox.png", width = 900, height = 700)
(bepa.cn.box | bepa.cp.box | bepa.np.box) + 
  plot_annotation(title = "White Birch")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/BEPAfull.png", width = 900, height = 700)
(bepa.c.box / bepa.qty.c.box / bepa.cn.box | bepa.n.box / bepa.qty.n.box / bepa.cp.box | bepa.p.box/bepa.qty.p.box/bepa.np.box
) + 
  plot_annotation(title = "White Birch")
dev.off()

# VAAN
png("graphics/StoichModels_2Step/Boxplots/VAANpercentbox.png", width = 900, height = 700)
(vaan.c.box | vaan.n.box | vaan.p.box) + 
  plot_annotation(title = "Lowland Blueberry")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/VAANqtybox.png", width = 900, height = 700)
(vaan.qty.c.box | vaan.qty.n.box | vaan.qty.p.box) + 
  plot_annotation(title = "Lowland Blueberry")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/VAANratiobox.png", width = 900, height = 700)
(vaan.cn.box | vaan.cp.box | vaan.np.box) + 
  plot_annotation(title = "Lowland Blueberry")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/VAANfull.png", width = 900, height = 700)
(vaan.c.box / vaan.qty.c.box / vaan.cn.box | vaan.n.box / vaan.qty.n.box / vaan.cp.box | vaan.p.box/vaan.qty.p.box/vaan.np.box
) + 
  plot_annotation(title = "Lowland Blueberry")
dev.off()

# % Data
png("graphics/StoichModels_2Step/Boxplots/PercentC.png", width = 900, height = 700)
(abba.c.box / bepa.c.box |acru.c.box / vaan.c.box
) + 
  plot_annotation(title = "A = ABBA, B = BEPA, C = ACRU, D = VAAN", tag_levels = "A")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/PercentN.png", width = 900, height = 700)
(abba.n.box / bepa.n.box |acru.n.box / vaan.n.box
) + 
  plot_annotation(title = "A = ABBA, B = BEPA, C = ACRU, D = VAAN", tag_levels = "A")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/PercentP.png", width = 900, height = 700)
(abba.p.box / bepa.p.box |acru.p.box / vaan.p.box
) + 
  plot_annotation(title = "A = ABBA, B = BEPA, C = ACRU, D = VAAN", tag_levels = "A")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/PercentFull.png", width = 1000, height = 800)
((abba.c.box | acru.c.box | bepa.c.box | vaan.c.box) / (abba.n.box | acru.n.box | bepa.n.box | vaan.n.box) / (abba.p.box | acru.p.box | bepa.p.box | vaan.p.box) 
  + plot_annotation(tag_levels = "A") 
  + plot_annotation(title =   "Balsam Fir                                               Red Maple                                           White Birch                                         Lowland Blueberry")
  + plot_annotation(theme = theme(plot.title = element_text(size = 16))) 
  )
  dev.off()


# Qty Data
png("graphics/StoichModels_2Step/Boxplots/QtyC.png", width = 900, height = 700)
(abba.qty.c.box / bepa.qty.c.box |acru.qty.c.box / vaan.qty.c.box
) + 
  plot_annotation(title = "A = ABBA, B = BEPA, C = ACRU, D = VAAN", tag_levels = "A")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/QtyN.png", width = 900, height = 700)
(abba.n.box / bepa.n.box |acru.n.box / vaan.n.box
) + 
  plot_annotation(title = "A = ABBA, B = BEPA, C = ACRU, D = VAAN", tag_levels = "A")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/QtyP.png", width = 900, height = 700)
(abba.qty.p.box / bepa.qty.p.box |acru.qty.p.box / vaan.qty.p.box
) + 
  plot_annotation(title = "A = ABBA, B = BEPA, C = ACRU, D = VAAN", tag_levels = "A")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/QtyFull.png", width = 1000, height = 800)
((abba.qty.c.box | acru.qty.c.box | bepa.qty.c.box | vaan.qty.c.box) / (abba.qty.n.box | acru.qty.n.box | bepa.qty.n.box | vaan.qty.n.box) / (abba.qty.p.box | acru.qty.p.box | bepa.qty.p.box | vaan.qty.p.box) 
  + plot_annotation(tag_levels = "A") 
  + plot_annotation(title =   "Balsam Fir                                               Red Maple                                           White Birch                                         Lowland Blueberry")
  + plot_annotation(theme = theme(plot.title = element_text(size = 16))) 
)
dev.off()


# Ratio Data
png("graphics/StoichModels_2Step/Boxplots/CN.png", width = 900, height = 700)
(abba.cn.box / bepa.cn.box |acru.cn.box / vaan.cn.box
) + 
  plot_annotation(title = "A = ABBA, B = BEPA, C = ACRU, D = VAAN", tag_levels = "A")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/CP.png", width = 900, height = 700)
(abba.cp.box / bepa.cp.box |acru.cp.box / vaan.cp.box
) + 
  plot_annotation(title = "A = ABBA, B = BEPA, C = ACRU, D = VAAN", tag_levels = "A")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/NP.png", width = 900, height = 700)
(abba.np.box / bepa.np.box |acru.np.box / vaan.np.box
) + 
  plot_annotation(title = "A = ABBA, B = BEPA, C = ACRU, D = VAAN", tag_levels = "A")
dev.off()

png("graphics/StoichModels_2Step/Boxplots/RatioFull.png", width = 1000, height = 800)
((abba.cn.box | acru.cn.box | bepa.cn.box | vaan.cn.box) / (abba.cp.box | acru.cp.box | bepa.cp.box | vaan.cp.box) / (abba.np.box | acru.np.box | bepa.np.box | vaan.np.box) 
  + plot_annotation(tag_levels = "A") 
  + plot_annotation(title =   "Balsam Fir                                               Red Maple                                           White Birch                                         Lowland Blueberry")
  + plot_annotation(theme = theme(plot.title = element_text(size = 16))) 
)
dev.off()

# explanatory variables
png("graphics/StoichModels_2Step/Boxplots/EVI_GDD_NDMI.png", width = 900, height = 500)
(evi.box | gdd.box | ndmi.box) 
dev.off()

# Make a figure showing the relationship between % C and GDD for the MS 
# Basic scatter plot
meltStoich <- melt(
  stoich,
  id.vars = c('GDD', 'C', 'Site', 'Year'),
  measure.vars = c('Species')
)

png("graphics/StoichModels_2Step/Boxplots/GDD_C_Species.png", width = 1000, height = 800)
ggplot(meltStoich) +
  geom_point(aes(GDD,C, shape = Year, color = Site), size = 2.5) +
  geom_smooth(aes(GDD,C), color = "#373730", method = lm) + 
  scale_color_manual(values=c("#66545e", "#a39193", "#aa6f73", "#eea990"))+
  facet_wrap( ~ value, labeller = labeller(value = 
                                             c("ABBA" = "Balsam Fir",
                                               "ACRU" = "Red Maple",
                                               "BEPA" = "White Birch",
                                               "VAAN" = "Lowland Blueberry"))) 
dev.off()