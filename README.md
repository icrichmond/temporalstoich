# Temporal variation and its drivers in the elemental traits of four boreal plant species 

### Isabella C. Richmond; Shawn J. Leroux; Travis R. Heckford; Eric Vander Wal; Matteo Rizzuto; Juliana Balluffi-Fry; Joanie L. Kennah; Yolanda F. Wiersma  

#### Manuscript is published at the Journal of Plant Ecology. All data and code for the manuscript are available here and published on Zenodo.

This repository contains code, data, and results for Richmond et al. Temporal variation and its drivers in the elemental traits of four boreal plant species. 

Zenodo Repository: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4090893.svg)](https://doi.org/10.5281/zenodo.4090893)   
Paper: https://academic.oup.com/jpe/article-abstract/14/3/398/6044219?redirectedFrom=fulltext

**code**  
This folder contains all the R scripts necessary to reproduce our analyses.  

* 1 - WeatherCor.R tests correlations between Lethbridge and Clarenville weather station data obtained from Environment and Climate Change Canada so that 2016 data can be supplemented.
* 2 - GDD.R takes raw weather data, calculates GDD as per McMaster & Wilhelm (method 1) using 5 deg. Celsius as a base temperature as per Brown et al. 2019. Then, GDD is extracted for each sampling date during our study.
* 3 - NDMI.R processes Landsat imagery to obtain NDMI values at each of our sampling points in 2016 and 2017.
* 4 - EVI.R processes Landsat imagery to obtain EVI values at each of our sampling points in 2017 (2016 EVI values were provided by coauthor Travis Heckford, but were obtained using the same method).
* 5 - StandardizedPercentModels.R builds and analyzes our elemental composition models (% C, N, P) for our four study species. 
* 6 - RatioModels.R builds and analyzes our stoichiometric ratio models (C:N,C:P, N:P) for our four study species.
* 7 - VariationPartitioning.R  partitions the variance between site, year, and species for each element and ratio that we tested.
* 8 - ManuscriptFigures.R produces figures for the manuscript.

**graphics**  
This folder contains all the graphics and figures produced during the analysis.  

* EVI
  + Contains figures of the processed EVI for each of the dates with Landsat imagery during 2017 and the histograms for the distribution of data.
* Models
  + AIC
    - Contains visual representation of the AIC tables for the second step of modelling.
  + Boxplots
    - Contains boxplots and correlation figures used in the manuscript.
  + Correlations
    - Contains the visualization of correlation analyses between the explanatory variables in the models for each species.
  + ModelDiagnostics
    - Contains diagnostic plots for each of the groups of models that we run.
* NDMI
  + Contains figures of the processed EVI for each of the dates with Landsat imagery during 2016/2017 and the histograms for the distribution of data.
* WeatherCor
  + Contains figures of the correlations between weather stations for max, mean, and min temperature.
 
**input**  
This folder and all folders within contain the raw data used in analysis.

**output**  
This folder contains all the output from our analysis.  

* AIC 
  + Contains AIC tables for each model comparison.
* EVI 
  + Contains processed rasters from Landsat imagery of EVI values.
* NDMI 
  + Contains processed rasters from Landsat imagery of NDMI values.
* Summary 
  + Contains summary tables for each of the top models identified using AIC.

