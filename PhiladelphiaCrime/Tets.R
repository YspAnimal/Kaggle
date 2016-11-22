x <- c("ggmap", "rgdal", "rgeos", "maptools", "tidyr", "tmap")
install.packages(x) # warning: this may take a number of minutes
lapply(x, library, character.only = TRUE) # load the required packages

library(dismo)
library(data.table)
library(R.utils)
library(ggplot2)
library(plyr)
library(dplyr)
library(grid) # for grids
library(gridExtra) # for advanced plots

setwd("C:/R_repositories/OthersData/Kaggle/PhiladelphiaCrime")
destFile <- "crime.csv"
if (file.exists(destFile)){
    CrimeData <- fread(destFile)
}

head(CrimeData)
g <- gmap("Philadelphia, PA, United States", type='hybrid', zoom=11, scale=2)
plot(g, interpolate=TRUE)
Phil <- geocode('Philadelphia, 5500 BLOCK N 5TH ST')
merc <- Mercator(Phil[, c('longitude', 'latitude')])
points(merc, pch='.', col='red', cex=5)












g = gmap('Australia')
plot(g, inter=TRUE)
gs = gmap('Sydney, New South Wales, Australia', type='satellite')
plot(gs, inter=TRUE)
gs = gmap('Sydney, Australia', type='satellite', exp=3)
plot(gs, inter=TRUE)
gs = gmap('Sydney, Australia', type='hybrid', zoom=10, scale=2)
plot(gs, inter=TRUE)



