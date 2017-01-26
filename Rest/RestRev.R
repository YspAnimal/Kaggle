library(ggplot2)
library(data.table)
library(dplyr)
library(tm)
library(slam)


setwd("C:/R_repositories/Kaggle/Rest")
destFile <- "1-restaurant-train.csv"
if (file.exists(destFile)){
    #ResReviewDataTrain <- readLines(destFile)#fread(destFile, sep= "?", header = FALSE)
    ResReviewDataTrain <- fread(destFile, nrows = 36024)#, sep= "?", header = FALSE)
    #ResReviewDataTrain2 <- fread(destFile, sep= "?", header = FALSE)
}
ResReviewDataTrain <- mutate(ResReviewDataTrain, review = ifelse(V1>=4, 1, 0)) %>% select(- V1)
DataTrainPos <- filter(ResReviewDataTrain, review == 1)
DataTrainNeg <- filter(ResReviewDataTrain, review == 0)


Pos.cor <- VectorSource(DataTrainPos$V2) %>% Corpus()
Pos.cor <- tm_map(Pos.cor, content_transformer(tolower))
Pos.cor <- tm_map(Pos.cor, removePunctuation)
Pos.cor <- tm_map(Pos.cor, removeNumbers)
Pos.cor <- tm_map(Pos.cor, removeNumbers)
#StopWords <- c(stopwords("english"))
Pos.cor <- tm_map(Pos.cor, removeWords, stopwords("english"))
#Pos.cor <- tm_map(Pos.cor, stripWhitespace)
DTM <- DocumentTermMatrix(Pos.cor)
ph.DTM3 <- rollup(DTM, 2, na.rm=TRUE, FUN = sum)
inspect(DTM[1:5,900:905])