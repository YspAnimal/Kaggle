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
DTMPos <- DocumentTermMatrix(Pos.cor)
DTMPos <- removeSparseTerms(DTMPos, 0.98)
DTMPos_Mat <- as.matrix(DTMPos)
DTMPos_v <- sort(colSums(DTMPos_Mat),decreasing=TRUE)
DTMPos_d <- data.frame(word = names(DTMPos_v),freq=DTMPos_v)
table(DTMPos_d$freq)

Neg.cor <- VectorSource(DataTrainNeg$V2) %>% Corpus()
Neg.cor <- tm_map(Neg.cor, content_transformer(tolower))
Neg.cor <- tm_map(Neg.cor, removePunctuation)
Neg.cor <- tm_map(Neg.cor, removeNumbers)
#StopWords <- c(stopwords("english"))
Neg.cor <- tm_map(Neg.cor, removeWords, stopwords("english"))
#Pos.cor <- tm_map(Pos.cor, stripWhitespace)
DTMNeg <- DocumentTermMatrix(Neg.cor)
DTMNeg <- removeSparseTerms(DTMNeg, 0.98)
DTMNeg_Mat <- as.matrix(DTMNeg)
DTMNeg_v <- sort(colSums(DTMNeg_Mat),decreasing=TRUE)
DTMNeg_d <- data.frame(word = names(DTMNeg_v),freq=DTMNeg_v)
table(DTMNeg_d$freq)




