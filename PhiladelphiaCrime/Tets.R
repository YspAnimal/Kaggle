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
unique(CrimeData$Text_General_Code)
unique(CrimeData$Dc_Dist)


select(CrimeData, Text_General_Code) %>% unique %>% nrow


ranking <- group_by(CrimeData, Month, Dc_Dist, Text_General_Code)

x <- group_by(CrimeData, Text_General_Code, Month) %>%
        count(Text_General_Code) %>%
        summarise(n=sum(n, na.rm = TRUE))

y <- group_by(CrimeData, Month) %>%
    count(Month)



count(ranking ,Dc_Dist) %>% 
    summarize(N = sum)


ggplot(y, aes(x=Month, y=n,group = 1)) + geom_line() + geom_point()
qplot(y$Month, y$n, geom = "line")








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



