# Author: Travis Heckford & Isabella Richmond
# Date of last change: October 14, 2020

# Processing of Landsat (EVI) from Landsat imagery for 2017 data 
# 2016 EVI data was processed in exactly the same way by Travis Heckford
# both 2016 and 2017 data used for my temporal stoich models 


# If steps where done in ArcGIS, the details of those steps are described in this code

# Install/load the following packages
install.packages("easypackages")
library(easypackages)
packages("colorspace", "rts", "raster", "RCurl", "sp", "rgdal", "rgeos", "RStoolbox", "shapefiles", "getSpatialData", "rasterVis", "spatialEco", "spatial.tools")
libraries("colorspace", "rts", "raster", "RCurl", "sp", "rgdal", "rgeos", "RStoolbox", "shapefiles", "getSpatialData", "rasterVis", "spatialEco", "spatial.tools")

# This analysis is bounded within our Area Of Interest (AOI).
# The AOI is the ecodistrict (468)
# Areas of no data exists in the NW corner (fingers; no FRI), and bottom (south point; no EVI - edge of landsat scene)
AOI <- shapefile("input/EVI/AOI/EcoDistr_AOI.shp")
plot(AOI)

#### Enhanced Vegetation Index Processing ####

# Preprocessing done in ArcGIS
# See supplemental information for Landsat interrogation of sample sites
# For each scene:
# Using the Landsat Quality Assessment (QA) ArcGIS Toolbox
# We used the DecodeQA script to code pixel band bits of the Pixel_QA band.
# The following raster (Decoded QA bits) was used to confirm clouds from the MLT (RGB) band.
# We used the ExtractQA script to extract and combine pixel reliability bits for cloud/snow
# Bits were selected for Cloud Shadow, Snow, Cloud, High Cloud Confidence, High Cirrus Confidence
# The resulting selected bits were combined into a single raster where 1 = clouds and 0 = no clouds, and NoData
# Cloud masks were clipped to the AOI (Extract by Mask)
# Cloud masks were reclassified to 1 (clouds) = NoData, and 0 (other) and NoData = 0
# EVI Landsat scene were clipped to the AOI (Extract by Mask)
# Using Extract by Mask, cloudy pixels from the cloud masks were removed from each EVI scene
# Here, we import EVI scene that are cloud free

# load Scene 1; May 14, 2017 
Scene1_EVI <- raster("input/EVI/ProcessedRasters/EVI_NoClouds_20170514.tif")
# load Scene 2: May 30, 2017
Scene2_EVI <- raster("input/EVI/ProcessedRasters/EVI_NoClouds_20170530.tif")
# load Scene 3; July 17, 2017
Scene3_EVI <- raster("input/EVI/ProcessedRasters/EVI_NoClouds_20170717.tif")
# load Scene 4: September 3, 2017
Scene4_EVI <- raster("input/EVI/ProcessedRasters/EVI_NoClouds_20170903.tif")

# rescale EVI Scenes by 0.0001
Scene1EVI_rescale <- Scene1_EVI*0.0001
Scene2EVI_rescale <- Scene2_EVI*0.0001
Scene3EVI_rescale <- Scene3_EVI*0.0001
Scene4EVI_rescale <- Scene4_EVI*0.0001

# create a raster brick with the four scenes
EVI_Brick <- brick(list(Scene1EVI_rescale, Scene2EVI_rescale, Scene3EVI_rescale, Scene4EVI_rescale))
# plot scenes
myTheme <- rasterTheme(region=sequential_hcl(6, power=0.8))
LayerNames <- c("May 14, 2017", "May 30, 2017","July 17, 2017", "September 13, 2017")
png("graphics/EVI/EVI_Original.png", width = 13, height = 6, units = "in", res = 600)
levelplot(EVI_Brick, par.settings = myTheme, names.attr = LayerNames)
dev.off()

# perform a linear interpolation between EVI time scenes to populate no data areas
# first check the distribution of elevation values in the raster
jpeg("graphics/EVI/EVIBrickHist.jpg")
hist(EVI_Brick) # The vegetation component of these data are normal; values 0 < or = 0 = water/NA
dev.off()

EVI_approx <- approxNA(EVI_Brick, method="linear", rule=2) # linear because hist = normal; rule 2 = remove all NAs; rule 1 some NA's retain via interpolation search constraints.
# Recheck histogram
jpeg("graphics/EVI/EVIApproxHist.jpg")
hist(EVI_approx)
dev.off()
# Plot Scenes
LayerNamesInterp <- c(" ", " ", " ", " ")

png("Graphics/EVI/EVI_Interpolate.png", width = 13, height = 6, units = "in", res = 600)
levelplot(EVI_approx, par.settings = myTheme, names.attr = LayerNamesInterp)
dev.off()

writeRaster(EVI_approx, "output/EVI/EVIApprox", format="GTiff", bylayer=T, suffix="names", overwrite=TRUE)

# Average rasters together
EVI_mean <-calc(EVI_approx, fun = mean)
jpeg("graphics/EVI/EVIMean.jpg")
plot(EVI_mean)
dev.off()
writeRaster(EVI_mean, "output/EVI/EVI_NonScaled_Averaged.tif", overwrite=TRUE)


##### Spatial Extraction of EVI Values ####
# load sample points and extract EVI values at those points
EVI <- raster("output/EVI/EVI_NonScaled_Averaged.tif")
samppts <- shapefile("input/EVI/SamplePoints/SamplePoints_StDMs.shp")
crs(EVI)
crs(samppts)
# reproject sample points to match raster
sampptsproj <- spTransform(samppts, CRS("+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
jpeg("graphics/EVI/EVIsamplepoints.jpg")
plot(EVI)
plot(sampptsproj, add = TRUE)
dev.off()
# get coordinates of sample points
SP <- SpatialPoints(coordinates(sampptsproj)[,-3])
# extract values from StDM rasters for our sample points
# need to unload tidyr from library as extract in tidyr doesn't work for rasters
SPraster <- extract(EVI,SP)
# join extracted raster values back
sampptsraster <- cbind(sampptsproj, SPraster)
View(sampptsraster)
sampptsraster@data
# change the column name to EVI2017
colnames(sampptsraster@data)[6] = "EVI2017"
# save Sample Points with 2017 EVI as a csv
write.csv(sampptsraster, "input/EVI/EVI_2017.csv")

# combine 2016 and 2017 data
EVI2016 <- read.csv("input/EVI/EVI_2016.csv")
EVI2017 <- read.csv("input/EVI/EVI_2017.csv")
# keep relevant columns 
EVI2016 <- add_column(EVI2016, Year = 2016)
EVI2016 <- subset(EVI2016, select = c(PlotName, Year, EVI))

EVI2017 <- EVI2017 %>% 
  dplyr::rename("EVI" = "EVI2017") %>%
  add_column(Year = 2017)
EVI2017 <- subset(EVI2017, select = c(PlotName, Year, EVI))
# combine years
EVI <- rbind(EVI2016, EVI2017)
# write csv 
write.csv(EVI, "input/EVI_2016_2017_R.csv")