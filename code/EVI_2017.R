#Author: Travis Heckford
#Edited by: Isabella Richmond
# Date of last change: 2/April/2019
# Notes to self (from Travis)
# Check with YW, should all scaled layers to be set to 0-1, or -1 to 1? In the code below I just let the min and max standardized values up to the original layers. 

# Code Notes:
# Data manipulation for spatial correlates
# Processing of Landsat (EVI), Elevation, Aspect, Slope, Forest Resource Inventory, and the CEC landcover dataset
# Extract of spatial co-variate values happens at the end
# If steps where done in ArcGIS, the details of those steps are described in this code

# Set working directory
setwd("C:/Users/Isabella Richmond/Dropbox/Chapter 1 - Temporal Stoich/EVI/Landsat/2017")

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

# Load Scene 1; May 14, 2017 
Scene1_EVI <- raster("EVI_NoClouds_20170514.tif")
# Load Scene 2: May 30, 2017
Scene2_EVI <- raster("EVI_NoClouds_20170530.tif")
# Load Scene 3; July 17, 2017
Scene3_EVI <- raster("EVI_NoClouds_20170717.tif")
# Load Scene 4: September 3, 2017
Scene4_EVI <- raster("EVI_NoClouds_20170903.tif")
# Rescale EVI Scenes by 0.0001
Scene1EVI_rescale <- Scene1_EVI*0.0001
Scene2EVI_rescale <- Scene2_EVI*0.0001
Scene3EVI_rescale <- Scene3_EVI*0.0001
Scene4EVI_rescale <- Scene4_EVI*0.0001

# Create a raster brich with the four scenes
EVI_Brick <- brick(list(Scene1EVI_rescale, Scene2EVI_rescale, Scene3EVI_rescale, Scene4EVI_rescale))
# Plot Scenes
levelplot(EVI_Brick)
# Perform a linear interpolation between EVI time scenes to populate no data areas
# First check the distribution of elevation values in the raster
hist(EVI_Brick) # The vegetation component of these data are normal; values 0 < or = 0 = water/NA

EVI_approx <- approxNA(EVI_Brick, method="linear", rule=2) # linear because hist = normal; rule 2 = remove all NAs; rule 1 some NA's retain via interpolation search constraints.
# Recheck histogram
hist(EVI_approx)
# Plot Scenes
plot(EVI_approx)
writeRaster(EVI_approx, "ApproxNA", format="GTiff", bylayer=T, suffix="names", overwrite=TRUE)

# Average rasters together
EVI_mean <-calc(EVI_approx, fun = mean)
plot(EVI_mean)
writeRaster(EVI_mean, "EVI_NonScaled_ForSampPts.tif", overwrite=TRUE)


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

##Sample points and EVI raster are in different projections. Instead of extracting in R,
##opened both files in ArcMap and used Extract Raster to Point tool, to get EVI
##values for each sample point. Then copied and pasted table to csv.
# For some of the sample locations, information on age class, height class, and crown density were missing, because our sample points was located within a wetland type land cover class in FRI, to correct for spatial inaccuracies I extracted values from the adjacent forest type land cover class - to 