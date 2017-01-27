##https://inclass.kaggle.com/c/restaurant-reviews
library(ggplot2)
library(data.table)
library(dplyr)
library(tm)
library(slam)
library(RWeka)
##################################################
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
meta(Pos.cor, tag = "type") <- "Positive"
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
Pos.cor <- tm_map(Pos.cor, toSpace, "@[^\\s]+") %>%
            tm_map(toSpace, "[^\\p{L}\\s[']]+") %>%
            tm_map(content_transformer(tolower)) %>%
            tm_map(removePunctuation) %>%
            tm_map(removeNumbers) %>%
            tm_map(removeWords, stopwords("english")) %>%
            tm_map(stripWhitespace)
DTMPos <- DocumentTermMatrix(Pos.cor)
DTMPos <- removeSparseTerms(DTMPos, 0.98)

Neg.cor <- VectorSource(DataTrainNeg$V2) %>% Corpus()
meta(Pos.cor, tag = "type") <- "Negative"
Neg.cor <- tm_map(Neg.cor, toSpace, "@[^\\s]+") %>%
            tm_map(Neg.cor, toSpace, "[^\\p{L}\\s[']]+") %>%
            tm_map(Neg.cor, content_transformer(tolower)) %>%
            tm_map(Neg.cor, removePunctuation) %>%
            tm_map(Neg.cor, removeNumbers) %>%
            tm_map(Neg.cor, removeWords, stopwords("english")) %>%
            tm_map(Neg.cor, stripWhitespace)
DTMNeg <- DocumentTermMatrix(Neg.cor)
DTMNeg <- removeSparseTerms(DTMNeg, 0.98)

library(RWeka)
options(mc.cores=1)
Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
unigram <- DocumentTermMatrix(Neg.cor, control = list(tokenize = Tokenizer))
bigram <- DocumentTermMatrix(Neg.cor, control = list(tokenize = BigramTokenizer))
trigram <- DocumentTermMatrix(Neg.cor, control = list(tokenize = TrigramTokenizer))

bigramNeg <- removeSparseTerms(bigram, 0.98)
trigramNeg <- removeSparseTerms(trigram, 0.9999)



# tm_trifreq <- sort(colSums(as.matrix(bigramNeg)), decreasing=TRUE)
# tm_triwordfreq <- data.frame(word=names(tm_trifreq), freq=tm_trifreq)
# head(tm_triwordfreq,5)




# DTMPos_Mat <- as.matrix(DTMPos)
# DTMPos_v <- sort(colSums(DTMPos_Mat),decreasing=TRUE)
# DTMPos_d <- data.frame(word = names(DTMPos_v),freq=DTMPos_v)
# #table(DTMPos_d$freq)
# 
# 
# DTMNeg_Mat <- as.matrix(DTMNeg)
# DTMNeg_v <- sort(colSums(DTMNeg_Mat),decreasing=TRUE)
# DTMNeg_d <- data.frame(word = names(DTMNeg_v),freq=DTMNeg_v)
# #table(DTMNeg_d$freq)

