x <- c("ggmap", "rgdal", "rgeos", "maptools", "tidyr", "tmap")
install.packages(x) # warning: this may take a number of minutes
lapply(x, library, character.only = TRUE) # load the required packages

library(dismo)
library(data.table)
library(R.utils)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(grid) # for grids
library(gridExtra) # for advanced plots
library(scales)
library(bit64)
library(ggmap)

setwd("C:/R_repositories/OthersData/Kaggle/PhiladelphiaCrime")
destFile <- "crime.csv"
if (file.exists(destFile)){
    CrimeData <- fread(destFile)
}

head(CrimeData)
unique(CrimeData$Text_General_Code) #
unique(CrimeData$Dc_Dist)


select(CrimeData, Text_General_Code) %>% unique %>% nrow


ranking <- group_by(CrimeData, Month, Dc_Dist, Text_General_Code)

x <- group_by(CrimeData, Text_General_Code, Month) %>%
        count(Text_General_Code) %>%
        summarise(n=sum(n, na.rm = TRUE))

y <- group_by(CrimeData, Month) %>%
        count(Month)

CrimeToAnalyse <- head(arrange(x, desc(n)), 5)$Text_General_Code #Get only top 5 Crime type

x <- filter(CrimeData, Text_General_Code %in% CrimeToAnalyse) %>%
        group_by(Text_General_Code)


x.dateSum <- group_by(x, Text_General_Code, Dispatch_Date) %>% count()



####Plot day-time graphs for top 5 Crimes contains all data at dataset
x.timeSum <- group_by(x, Text_General_Code, Dispatch_Time) %>% count() #%>% summarise( )
x.timeSum$Text_General_Code<-as.factor(x.timeSum$Text_General_Code)
x.timeSum$Dispatch_Time <- as.POSIXct(x.timeSum$Dispatch_Time, format="%H:%M:%S", tz="GMT")  
lims <- c(x.timeSum$Dispatch_Time[1], tail(x.timeSum$Dispatch_Time)[1])
PlotTS <- ggplot(data=x.timeSum, aes(x=Dispatch_Time,
                                     y=n,
                                     group = Text_General_Code,
                                     color = Text_General_Code)) +
            geom_point() +
            facet_grid(facets = Text_General_Code ~ .) +
            scale_x_datetime(date_breaks="4 hour",
                             labels = date_format("%H:%M"),
                             limits=lims) + 
            xlab("Dispatch time") +
            ylab("Total") +
            ggtitle("Top 5 Crime types") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
#####


#####Geo information
CrimeToGeo <- select(CrimeData, Dc_Dist, Dispatch_Date_Time, Hour, Location_Block, Text_General_Code, Police_Districts, Lon, Lat)
PhilMap <- get_map(location="Philadelphia, PA, United States", maptype='hybrid')#, zoom=11, scale=2)
ggmap(PhilMap)
























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
ggplot(y, aes(x=Month, y=n,group = 1)) + geom_line() + geom_point() + stat_smooth(method = "lm", col = "red")#+ geom_line(y.lm, aes(x=Month, y=n,group = 1))



