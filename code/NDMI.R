# Author: Travis Heckford 
# Modified by: Isabella Richmond
# Date of last change: March 13, 2020


# Processing of Landsat imagery to obtain NDMI for 2016 and 2017
# NDMI data is used in my temporal stoich models 
# If steps where done in ArcGIS, the details of those steps are described in this code and 
# are also explained in the supplementary information for EVI

# install/load the necessary packages
install.packages("easypackages")
library(easypackages)
packages("rts", "raster", "RCurl", "sp", "rgdal", "rgeos", "RStoolbox", "shapefiles", "rasterVis", "spatialEco", "spatial.tools")
libraries("rts", "raster", "RCurl", "sp", "rgdal", "rgeos", "RStoolbox", "shapefiles",  "rasterVis", "spatialEco", "spatial.tools")

# This analysis is bounded within our Area Of Interest (AOI).
# The AOI is the ecodistrict (468)
# Areas of no data exists in the NW corner (fingers; no FRI), and bottom (south point; no EVI - edge of landsat scene)
AOI <- shapefile("input/EVI/AOI/EcoDistr_AOI.shp")
plot(AOI)

#### Normalized Difference Moisture Index (NDMI) Processing ####
# preprocessing done in ArcGIS
# see supplemental information for Landsat interrogation of sample sites
# For each scene:
# Using the Landsat Quality Assessment (QA) ArcGIS Toolbox
# We used the DecodeQA script to code pixel band bits of the Pixel_QA band.
# The following raster (Decoded QA bits) was used to confirm clouds from the MLT (RGB) band.
# We used the ExtractQA script to extract and combine pixel reliability bits for cloud/snow
# Bits were selected for Cloud Shadow, Snow, Cloud, High Cloud Confidence, High Cirrus Confidence
# The resulting selected bits were combined into a single raster where 1 = clouds and 0 = no clouds, and NoData
# Cloud masks were clipped to the AOI (Extract by Mask)
# Cloud masks were reclassified to 1 (clouds) = NoData, and 0 (other) and NoData = 0
# NDMI Landsat scene were clipped to the AOI (Extract by Mask)
# Using Extract by Mask, cloudy pixels from the cloud masks were removed from each NDMI scene
# Here, we import NDMI scene that are cloud free

# 2016 Scenes 
# load Scene 1; June 28, 2016
Scene1_NDMI <- raster("input/NDMI/ProcessedRasters/NDMI_NoClouds_20160628.tif")
# load Scene 2; August 15, 2016
Scene2_NDMI <- raster("input/NDMI/ProcessedRasters/NDMI_NoClouds_20160815.tif")
# load Scene 3; September 16, 2016
Scene3_NDMI <- raster("input/NDMI/ProcessedRasters/NDMI_NoClouds_20160916.tif")
# rescale EVI Scenes by 0.0001
Scene1NDMI_rescale <- Scene1_NDMI*0.0001
Scene2NDMI_rescale <- Scene2_NDMI*0.0001
Scene3NDMI_rescale <- Scene3_NDMI*0.0001
# create a raster brick with the three scenes
NDMI_Brick_2016 <- brick(list(Scene1NDMI_rescale, Scene2NDMI_rescale, Scene3NDMI_rescale))
# plot scenes
myTheme <- rasterTheme(region=sequential_hcl(6, h = 300, power=0.8))
LayerNames <- c("June 24, 2016", "August 15, 2016","September 16, 2016")
png("graphics/NDMI/NDMI_Original2016.png", width = 13, height = 6, units = "in", res = 600)
levelplot(NDMI_Brick_2016, par.settings = myTheme, names.attr = LayerNames)
dev.off()

# perform a linear interpolation between EVI time scenes to populate no data areas
# first check the distribution of elevation values in the raster
jpeg("graphics/NDMI/NDMIHist2016.jpg")
hist(NDMI_Brick_2016) # The vegetation component of these data are normal; values 0 < or = 0 = water/NA
dev.off()
NDMI_approx_2016 <- approxNA(NDMI_Brick_2016, method="linear", rule=2) # linear because hist = normal; rule 2 = remove all NAs; rule 1 some NA's retain via interpolation search constraints.
# recheck histogram
jpeg("graphics/NDMI/NDMIHistApprox2016.jpg")
hist(NDMI_approx_2016)
dev.off()
# plot scenes
LayerNamesInterp <- c(" ", " ", " ")

png("Graphics/NDMI/NDMI_Interpolate2016.png", width = 13, height = 6, units = "in", res = 600)
levelplot(NDMI_approx_2016, par.settings = myTheme, names.attr = LayerNamesInterp)
dev.off()

writeRaster(NDMI_approx_2016, "output/NDMI/ApproxNA2016", format="GTiff", bylayer=T, suffix="names", overwrite=TRUE)

# average rasters together
NDMI_mean_2016 <-calc(NDMI_approx_2016, fun = mean)
jpeg("graphics/NDMI/NDMIMean2016.jpg")
par(mfrow=c(1,1))
plot(NDMI_mean_2016)
dev.off()
writeRaster(NDMI_mean_2016, "output/NDMI/NDMI_NonScaled_Average_2016.tif", overwrite=TRUE)


# 2017 Scenes
# load Scene 4; May 30, 2017
Scene4_NDMI <- raster("input/NDMI/ProcessedRasters/NDMI_NoClouds_20170530.tif")
# load Scene 5; July 17, 2017
Scene5_NDMI <- raster("input/NDMI/ProcessedRasters/NDMI_NoClouds_20170717.tif")
# load Scene 3; September 3, 2017
Scene6_NDMI <- raster("input/NDMI/ProcessedRasters/NDMI_NoClouds_20170903.tif")
# rescale EVI Scenes by 0.0001
Scene4NDMI_rescale <- Scene4_NDMI*0.0001
Scene5NDMI_rescale <- Scene5_NDMI*0.0001
Scene6NDMI_rescale <- Scene6_NDMI*0.0001
# create a raster brick with the three scenes
NDMI_Brick_2017 <- brick(list(Scene4NDMI_rescale, Scene5NDMI_rescale, Scene6NDMI_rescale))
# plot scenes
myTheme <- rasterTheme(region=sequential_hcl(6, h = 300, power=0.8))
LayerNames <- c("May 30, 2017", "July 17, 2017","September 3, 2017")
png("graphics/NDMI/NDMI_Original2017.png", width = 13, height = 6, units = "in", res = 600)
levelplot(NDMI_Brick_2017, par.settings = myTheme, names.attr = LayerNames)
dev.off()
# perform a linear interpolation between NDMI time scenes to populate no data areas
# first check the distribution of elevation values in the raster
jpeg("graphics/NDMI/NDMIHist2017.jpg")
hist(NDMI_Brick_2017) # The vegetation component of these data are normal; values 0 < or = 0 = water/NA
dev.off()
NDMI_approx_2017 <- approxNA(NDMI_Brick_2017, method="linear", rule=2) # linear because hist = normal; rule 2 = remove all NAs; rule 1 some NA's retain via interpolation search constraints.
# recheck histogram
jpeg("graphics/NDMI/NDMIHistApprox2017.jpg")
hist(NDMI_approx_2017)
dev.off()
# plot scenes
LayerNamesInterp <- c(" ", " ", " ")

png("Graphics/NDMI/NDMI_Interpolate2017.png", width = 13, height = 6, units = "in", res = 600)
levelplot(NDMI_approx_2017, par.settings = myTheme, names.attr = LayerNamesInterp)
dev.off()

writeRaster(NDMI_approx_2017, "output/NDMI/ApproxNA2017", format="GTiff", bylayer=T, suffix="names", overwrite=TRUE)
# Average rasters together
NDMI_mean_2017 <-calc(NDMI_approx_2017, fun = mean)
jpeg("graphics/NDMI/NDMIMean2017.jpg")
par(mfrow=c(1,1))
plot(NDMI_mean_2017)
dev.off()
writeRaster(NDMI_mean_2017, "output/NDMI/NDMI_NonScaled_Average_2017.tif", overwrite=TRUE)

##### Spatial Extraction of Co-variate Values ####
# load rasters and sample points and extract NDMI values from 2016 and 2017 at each sample point
NDMI2016 <- raster("output/NDMI/NDMI_NonScaled_Average_2016.tif")
NDMI2017 <- raster("output/NDMI/NDMI_NonScaled_Average_2017.tif")
samppts <- shapefile("input/EVI/SamplePoints/SamplePoints_StDMs.shp")
# create raster brick for 2016/2017
NDMI20162017 <- brick(list(NDMI2016, NDMI2017))
# reproject sample points into raster CRS
crs(NDMI20162017)
crs(samppts)
sampptsproj <- spTransform(samppts, CRS("+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
# check that raster and shapefile match up
jpeg("graphics/NDMI/NDMIsamplepoints.jpg")
plot(NDMI2017)
plot(sampptsproj, add = TRUE)
dev.off()
# extract NDMI values at coordinates of sampling points
SP <- SpatialPoints(coordinates(sampptsproj)[,-3])
SPraster <- extract(NDMI20162017,SP)
sampptsraster <- cbind(sampptsproj, SPraster)
View(sampptsraster)
# rename columns 
sampptsraster@data
colnames(sampptsraster@data)[6] = "NDMI2016"
colnames(sampptsraster@data)[7] = "NDMI2017"

write.csv(sampptsraster, "output/NDMI/NDMI_20162017.csv")

# Clean up data in Excel - can fix and do this in R later 