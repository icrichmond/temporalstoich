#Author: Travis Heckford (for EVI processing)
#Edited by: Isabella Richmond
# Date of last change: 22/May/2019
# Notes to self (from Travis)
# Check with YW, should all scaled layers to be set to 0-1, or -1 to 1? 
# In the code below I just let the min and max standardized values up to the 
# original layers. 

# Code Notes:
# Data manipulation for spatial correlates
# Processing of Landsat NDMI
# If steps where done in ArcGIS, the details of those steps are described in 
# this code

# Set working directory
setwd("C:/Users/Isabella Richmond/Dropbox/Chapter 1 - Temporal Stoich/Soil Moisture Carrying Capacity/Landsat")

# Install/load the following packages
install.packages("easypackages")
library(easypackages)
packages("rts", "raster", "RCurl", "sp", "rgdal", "rgeos", "RStoolbox", "shapefiles", "getSpatialData", "rasterVis", "spatialEco", "spatial.tools")
libraries("rts", "raster", "RCurl", "sp", "rgdal", "rgeos", "RStoolbox", "shapefiles", "getSpatialData", "rasterVis", "spatialEco", "spatial.tools")

# This analysis is bounded within our Area Of Interest (AOI).
# The AOI is the ecodistrict (468)
# Areas of no data exists in the NW corner (fingers; no FRI), and bottom (south point; no EVI - edge of landsat scene)
AOI <- shapefile("C:/Users/Isabella Richmond/Dropbox/Chapter 1 - Temporal Stoich/EVI/2017/EcoDistr_AOI.shp")
plot(AOI)

#### Normalized Difference Moisture Index (NDMI) Processing ####
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
# NDMI Landsat scene were clipped to the AOI (Extract by Mask)
# Using Extract by Mask, cloudy pixels from the cloud masks were removed from each NDMI scene
# Here, we import NDMI scene that are cloud free

# 2016 Scenes 
# Load Scene 1; June 28, 2016
Scene1_NDMI <- raster("C:/Users/Isabella Richmond/Dropbox/Chapter 1 - Temporal Stoich/EVI/Landsat/2016/NDMI_NoClouds_20160628.tif")
# Load Scene 2; August 15, 2016
Scene2_NDMI <- raster("C:/Users/Isabella Richmond/Dropbox/Chapter 1 - Temporal Stoich/EVI/Landsat/2016/NDMI_NoClouds_20160815.tif")
# Load Scene 3; September 16, 2016
Scene3_NDMI <- raster("C:/Users/Isabella Richmond/Dropbox/Chapter 1 - Temporal Stoich/EVI/Landsat/2016/NDMI_NoClouds_20160916.tif")
# Rescale EVI Scenes by 0.0001
Scene1NDMI_rescale <- Scene1_NDMI*0.0001
Scene2NDMI_rescale <- Scene2_NDMI*0.0001
Scene3NDMI_rescale <- Scene3_NDMI*0.0001
# Create a raster brick with the four scenes
NDMI_Brick_2016 <- brick(list(Scene1NDMI_rescale, Scene2NDMI_rescale, Scene3NDMI_rescale))
# Plot Scenes
levelplot(NDMI_Brick_2016)
# Perform a linear interpolation between EVI time scenes to populate no data areas
# First check the distribution of elevation values in the raster
hist(NDMI_Brick_2016) # The vegetation component of these data are normal; values 0 < or = 0 = water/NA
NDMI_approx_2016 <- approxNA(NDMI_Brick_2016, method="linear", rule=2) # linear because hist = normal; rule 2 = remove all NAs; rule 1 some NA's retain via interpolation search constraints.
# Recheck histogram
hist(NDMI_approx_2016)
# Plot Scenes
plot(NDMI_approx_2016)
writeRaster(NDMI_approx_2016, "ApproxNA", format="GTiff", bylayer=T, suffix="names", overwrite=TRUE)
# Average rasters together
NDMI_mean_2016 <-calc(NDMI_approx_2016, fun = mean)
par(mfrow=c(1,1))
plot(NDMI_mean_2016)
writeRaster(NDMI_mean_2016, "NDMI_NonScaled_ForSampPts_2016.tif", overwrite=TRUE)
##Sample points and NDMI raster are in different projections. Instead of extracting in R,
##opened both files in ArcMap and used Extract Raster to Point tool, to get NDMI
##values for each sample point. Then copied and pasted table to csv.

# 2017 Scenes
# Load Scene 4; May 30, 2017
Scene4_NDMI <- raster("C:/Users/Isabella Richmond/Dropbox/Chapter 1 - Temporal Stoich/EVI/Landsat/2017/NDMI_NoClouds_20170530.tif")
# Load Scene 5; July 17, 2017
Scene5_NDMI <- raster("C:/Users/Isabella Richmond/Dropbox/Chapter 1 - Temporal Stoich/EVI/Landsat/2017/NDMI_NoClouds_20170717.tif")
# Load Scene 3; September 3, 2017
Scene6_NDMI <- raster("C:/Users/Isabella Richmond/Dropbox/Chapter 1 - Temporal Stoich/EVI/Landsat/2017/NDMI_NoClouds_20170903.tif")
# Rescale EVI Scenes by 0.0001
Scene4NDMI_rescale <- Scene4_NDMI*0.0001
Scene5NDMI_rescale <- Scene5_NDMI*0.0001
Scene6NDMI_rescale <- Scene6_NDMI*0.0001
# Export rasters to extract values for each scene in ArcMap
# Done because we want the individual values for each scene. If an average, extrapolated value is 
# desired, continue going through the code and skip this step
writeRaster(Scene1NDMI_rescale, "NDMI_2016_1", format = "GTiff", overwrite=TRUE)
writeRaster(Scene2NDMI_rescale, "NDMI_2016_2", format = "GTiff", overwrite=TRUE)
writeRaster(Scene3NDMI_rescale, "NDMI_2016_3", format = "GTiff", overwrite=TRUE)
writeRaster(Scene4NDMI_rescale, "NDMI_2017_1", format = "GTiff", overwrite=TRUE)
writeRaster(Scene5NDMI_rescale, "NDMI_2017_2", format = "GTiff", overwrite=TRUE)
writeRaster(Scene6NDMI_rescale, "NDMI_2017_3", format = "GTiff", overwrite=TRUE)

# Create a raster brick with the four scenes
NDMI_Brick_2017 <- brick(list(Scene4NDMI_rescale, Scene5NDMI_rescale, Scene6NDMI_rescale))
# Plot Scenes
levelplot(NDMI_Brick_2017)
# Perform a linear interpolation between NDMI time scenes to populate no data areas
# First check the distribution of elevation values in the raster
hist(NDMI_Brick_2017) # The vegetation component of these data are normal; values 0 < or = 0 = water/NA
NDMI_approx_2017 <- approxNA(NDMI_Brick_2017, method="linear", rule=2) # linear because hist = normal; rule 2 = remove all NAs; rule 1 some NA's retain via interpolation search constraints.
# Recheck histogram
hist(NDMI_approx_2017)
# Plot Scenes
plot(NDMI_approx_2017)
writeRaster(NDMI_approx_2017, "ApproxNA", format="GTiff", bylayer=T, suffix="names", overwrite=TRUE)
# Average rasters together
NDMI_mean_2017 <-calc(NDMI_approx_2017, fun = mean)
par(mfrow=c(1,1))
plot(NDMI_mean_2017)
writeRaster(NDMI_mean_2017, "NDMI_NonScaled_ForSampPts_2017.tif", overwrite=TRUE)
##Sample points and NDMI raster are in different projections. Instead of extracting in R,
##opened both files in ArcMap and used Extract Raster to Point tool, to get NDMI
##values for each sample point. Then copied and pasted table to csv.

##### Spatial Extraction of Co-variate Values ####
# Load layers and extract values of spatial co-variates
Samp.pts <- shapefile("SamplePoints_StDMs.shp")
EVI <- raster("EVI_NonScaled_ForSampPts.tif")
plot(EVI)

SP <- SpatialPoints(coordinates(Samp.pts)[,-3])
# Extract values from StDM rasters for our sample points
SP.raster <- extract(EVI, SP)
# Join extracted raster values back
Samp.pts.raster <- cbind(Samp.pts, SP.raster)
View(Samp.pts.raster)

Samp.pts.raster@data
plot(SP)
# Save Sample Points with Co-variates as a csv
write.csv(Samp.pts.raster, "SampPts_EVI_2017.csv")