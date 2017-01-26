##https://inclass.kaggle.com/c/restaurant-reviews
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
#StopWords <- c(stopwords("english"))
Pos.cor <- tm_map(Pos.cor, removeWords, stopwords("english"))
#Pos.cor <- tm_map(Pos.cor, stripWhitespace)
DTM <- DocumentTermMatrix(Pos.cor)
NewDTM <- removeSparseTerms(DTM, 0.98)
DTM_Mat <- as.matrix(NewDTM)
DTM_v <- sort(colSums(DTM_Mat),decreasing=TRUE)
DTM_d <- data.frame(word = names(DTM_v),freq=DTM_v)
table(DTM_d$freq)