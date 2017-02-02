##https://inclass.kaggle.com/c/restaurant-reviews
library(ggplot2)
library(data.table)
library(dplyr)
library(tm)
library(slam)
library(RWeka)
library(grid) # for grids
library(gridExtra) # for advanced plots

##################################################
setwd("C:/R_repositories/Kaggle/Rest")
destFile <- "1-restaurant-train.csv"
if (file.exists(destFile)){
    #ResReviewDataTrain <- readLines(destFile)#fread(destFile, sep= "?", header = FALSE)
    ResReviewDataTrain <- fread(destFile)#, nrows = 36024)#, sep= "?", header = FALSE)
    #ResReviewDataTrain2 <- fread(destFile, sep= "?", header = FALSE)
}

PreProcData <- function(x) {
    mutate(x, review = ifelse(V1>=4, 1, 0)) %>% 
        select(- V1)
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
#DTMPos <- removeSparseTerms(DTMPos, 0.98)

Neg.cor <- VectorSource(DataTrainNeg$V2) %>% Corpus()
meta(Pos.cor, tag = "type") <- "Negative"
Neg.cor <- tm_map(Neg.cor, toSpace, "@[^\\s]+") %>%
            tm_map(toSpace, "[^\\p{L}\\s[']]+") %>%
            tm_map(content_transformer(tolower)) %>%
            tm_map(removePunctuation) %>%
            tm_map(removeNumbers) %>%
            tm_map(removeWords, stopwords("english")) %>%
            tm_map(stripWhitespace)
DTMNeg <- DocumentTermMatrix(Neg.cor)
#DTMNeg <- removeSparseTerms(DTMNeg, 0.98)

library(RWeka)
options(mc.cores=1)
Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

unigramNeg <- DocumentTermMatrix(Neg.cor, control = list(tokenize = Tokenizer))
bigramNeg <- DocumentTermMatrix(Neg.cor, control = list(tokenize = BigramTokenizer))
trigramNeg <- DocumentTermMatrix(Neg.cor, control = list(tokenize = TrigramTokenizer))
unigramNeg <- removeSparseTerms(unigramNeg, 0.98)
bigramNeg <- removeSparseTerms(bigramNeg, 0.99)
trigramNeg <- removeSparseTerms(trigramNeg, 0.9995)
unigramPos <- DocumentTermMatrix(Pos.cor, control = list(tokenize = Tokenizer))
bigramPos <- DocumentTermMatrix(Pos.cor, control = list(tokenize = BigramTokenizer))
trigramPos <- DocumentTermMatrix(Pos.cor, control = list(tokenize = TrigramTokenizer))
unigramPos <- removeSparseTerms(unigramPos, 0.98)
bigramPos <- removeSparseTerms(bigramPos, 0.99)
trigramPos <- removeSparseTerms(trigramPos, 0.9995)

#####################################
##   Exploratory case   #############
#####################################

GetWordFreq <- function(data) {
    fr <- sort(colSums(as.matrix(data)), decreasing=TRUE)
    data.frame(word=names(fr), freq=fr)
}


NegBiFreq <- GetWordFreq(bigramNeg)
PosBiFreq <- GetWordFreq(bigramPos)

Posfreq <- sort(colSums(as.matrix(trigramPos)), decreasing=TRUE)
Poswordfreq <- data.frame(word=names(Posfreq), freq=Posfreq)
head(Poswordfreq,5)

Negfreq <- sort(colSums(as.matrix(trigramNeg)), decreasing=TRUE)
Negwordfreq <- data.frame(word=names(Negfreq), freq=Negfreq)
head(Negwordfreq,5)


makePlot <- function(data, label) {
    ggplot(data[1:20,], aes(reorder(word, -freq), freq)) +
        labs(x = label, y = "Frequency") +
        theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1)) +
        geom_bar(stat = "identity", fill = I("grey50"))
    }
PosTPlot <- makePlot(Poswordfreq, "20 Most Common Positive Trigram")
NegTPlot <- makePlot(Negwordfreq, "20 Most Common Negative Trigram")

NegBPlot <- makePlot(NegBiFreq, "20 Most Common Negative Bigram")
PosBPlot <- makePlot(PosBiFreq, "20 Most Common Negative Bigram")

grid.arrange(PosTPlot, NegTPlot, ncol = 2)  
grid.arrange(PosBPlot, NegBPlot, ncol = 2)  

























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

