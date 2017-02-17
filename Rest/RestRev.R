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



ResReviewDataTrain <- mutate(ResReviewDataTrain, review = ifelse(V1>=4, 2, ifelse(V1==3, 1, 0))) %>% select(- V1)

#ResReviewDataTrain <- mutate(ResReviewDataTrain, review = ifelse(V1>=4, 1, 0)) %>% select(- V1)
DataTrainPos <- filter(ResReviewDataTrain, review == 2)
DataTrainNet <- filter(ResReviewDataTrain, review == 1)
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
meta(Neg.cor, tag = "type") <- "Negative"
Neg.cor <- tm_map(Neg.cor, toSpace, "@[^\\s]+") %>%
            tm_map(toSpace, "[^\\p{L}\\s[']]+") %>%
            tm_map(content_transformer(tolower)) %>%
            tm_map(removePunctuation) %>%
            tm_map(removeNumbers) %>%
            tm_map(removeWords, stopwords("english")) %>%
            tm_map(stripWhitespace)
DTMNeg <- DocumentTermMatrix(Neg.cor)

Net.cor <- VectorSource(DataTrainNet$V2) %>% Corpus()
meta(Net.cor, tag = "type") <- "Neutral"
Net.cor <- tm_map(Net.cor, toSpace, "@[^\\s]+") %>%
    tm_map(toSpace, "[^\\p{L}\\s[']]+") %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace)
DTMNeg <- DocumentTermMatrix(Net.cor)

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
unigramNet <- DocumentTermMatrix(Net.cor, control = list(tokenize = Tokenizer))
bigramNet <- DocumentTermMatrix(Net.cor, control = list(tokenize = BigramTokenizer))
trigramNet <- DocumentTermMatrix(Net.cor, control = list(tokenize = TrigramTokenizer))
unigramNet <- removeSparseTerms(unigramNet, 0.98)
bigramNet <- removeSparseTerms(bigramNet, 0.99)
trigramNet <- removeSparseTerms(trigramNet, 0.9995)

#####################################
##   Exploratory step   #############
#####################################

GetWordFreq <- function(data) {
    fr <- sort(colSums(as.matrix(data)), decreasing=TRUE)
    data.frame(word=names(fr), freq=fr)
}

NegUniFreq <- GetWordFreq(unigramNeg)[1:130, ]
PosUniFreq <- GetWordFreq(unigramPos)[1:130, ]
NetUniFreq <- GetWordFreq(unigramNet)[1:130, ]

NegBiFreq <- GetWordFreq(bigramNeg)[1:130, ]
PosBiFreq <- GetWordFreq(bigramPos)[1:130, ]
NetBiFreq <- GetWordFreq(bigramNet)[1:130, ]

NegTriFreq <- GetWordFreq(trigramNeg)[1:130, ]
PosTriFreq <- GetWordFreq(trigramPos)[1:130, ]
NetTriFreq <- GetWordFreq(trigramNet)[1:130, ]

Posfreq <- sort(colSums(as.matrix(trigramPos)), decreasing=TRUE)
Poswordfreq <- data.frame(word=names(Posfreq), freq=Posfreq)
head(Poswordfreq,5)

Negfreq <- sort(colSums(as.matrix(trigramNeg)), decreasing=TRUE)
Negwordfreq <- data.frame(word=names(Negfreq), freq=Negfreq)
head(Negwordfreq,5)

Netfreq <- sort(colSums(as.matrix(trigramNet)), decreasing=TRUE)
Networdfreq <- data.frame(word=names(Netfreq), freq=Netfreq)
head(Networdfreq,5)


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

#####################################
##   Learning step   ################
#####################################

tmp <- inspect(unigramNeg[, as.vector(NegUniFreq$word)])
as.vector(NegUniFreq$word)



NegativeFeatureDF <- cbind(DataTrainNeg, data.frame(inspect(unigramNeg[, as.vector(NegUniFreq$word)])),
                           data.frame(inspect(bigramNeg[, as.vector(NegBiFreq$word)])),
                           data.frame(inspect(trigramNeg[, as.vector(NegTriFreq$word)])))

PositiveFeatureDF <- cbind(DataTrainPos, data.frame(inspect(unigramPos[, as.vector(PosUniFreq$word)])),
                           data.frame(inspect(bigramPos[, as.vector(PosBiFreq$word)])),
                           data.frame(inspect(trigramPos[, as.vector(PosTriFreq$word)])))

NeutralFeatureDF <- cbind(DataTrainNet, data.frame(inspect(unigramNet[, as.vector(NetUniFreq$word)])),
                           data.frame(inspect(bigramNet[, as.vector(NetBiFreq$word)])),
                           data.frame(inspect(trigramNet[, as.vector(NetTriFreq$word)])))

colnames(NegativeFeatureDF) <- c("t", "score")
colnames(PositiveFeatureDF) <- c("t", "score")
colnames(NeutralFeatureDF) <- c("t", "score")

DF <- rbind(NegativeFeatureDF, PositiveFeatureDF, NeutralFeatureDF)
DF <- DF[sample(nrow(DF)),] #shuffle rows in a dataframe
#DFMod <- subset(DF, select = -t)

rm(list=setdiff(ls(), c("DFMod", "DF"))) #Remove all from workspase except DFMod


library(e1071)
library(caret)
library(kernlab)

intrain<-createDataPartition(y=DF$score,p=0.7,list=FALSE)
DFtraining<-DF[intrain,]
DFtesting<-DF[-intrain,]


#####################################
## Try PCA for the first ############
#####################################
# colnames(DF) <- c("t", "score", as.character(c(1:300)))
# apply(DF,2,var)
# pca <- prcomp(DF[, c(-1,-2)])
# plot(pca)
# pca$rotation=-pca$rotation
# pca$x=-pca$x
# biplot (pca , scale =0)

#####################################
##   SVM section   ################
#####################################
colnames(DFtraining) <- c("t", "score", as.character(c(1:300)))
DFtraining$score <- as.factor(DFtraining$score)
colnames(DFtesting) <- c("t", "score", as.character(c(1:300)))
DFtesting

t_start <- Sys.time()
set.seed(3113)

svm_model <- svm(score ~ ., data=DFtraining[, -1], decision.values = T, probability = T)
# smDFtraining <- DFtraining[,-c(3:200)]
# svm_model <- svm(score ~ ., data=smDFtraining[,-1])

t_end <- Sys.time()
ModelTime <- t_end-t_start
summary(svm_model)

##Another svm(ksvm)
t_start <- Sys.time()
set.seed(4113)

ksvm_model <- ksvm(score ~ ., data=DFtraining[, -1], kernel = "rbfdot", decision.values = T, probability = T)

t_end <- Sys.time()
ModelTime <- t_end-t_start
summary(ksvm_model)

ksvm_test <- predict(ksvm_model, DFtesting[,c(-1, -2)])
table(ksvm_test, DFtesting[, 2])
mean(ksvm_test==DFtesting[, 2])



svm_test <- predict(svm_model, DFtesting[,c(-1, -2)], decision.values = T, probability = T)
head(svm_test[24615:24640], 20)

table(svm_test, DFtesting[, 2])
mean(svm_test==DFtesting[, 2])


#####################################
##   Random forest section  ###
#####################################

t_start <- Sys.time()
set.seed(3114)
forest <- train(score ~., data=DFtraining[, -1],
                method="rf",
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,
                ntree=10,
                do.trace=10,
                allowParallel=TRUE)
t_end <- Sys.time()

# Смотрим на модель и на время обучения
t_end-t_start
print(forest)

#####################################
##   Logistic regression section  ###
#####################################
DFtestingSub <- subset(DFtesting, select = -t) 
ScoreTest <- DFtesting[,1]
DFtestingSub <- subset(DFtestingSub, select = -score)

t_start <- Sys.time()

set.seed(3115)
glm_model <- glm(score ~ ., data=DFtraining[, -1])

t_end <- Sys.time()
ModelTime <- t_end-t_start

summary(glm_model)

glm_test <- predict(glm_model, DFtesting[, c(-1,-2)])

#####################################
##   Naive bayes section  ###########
#####################################

t_start <- Sys.time()
set.seed(3119)

nb_model <- naiveBayes(score~.,data = DFtraining[, -1])

t_end <- Sys.time()
ModelTime <- t_end-t_start
summary(nb_model)
nb_test <- predict(nb_model, DFtesting[,c(-1, -2)])
table(nb_test, DFtesting[, 2])
table(pred=nb_test,true=DFtesting[, 2])
mean(nb_test==DFtesting[, 2])

#####################################
##   Neural network section  ########
#####################################
DFtrainingNN <- DFtraining
colnames(DFtrainingNN) <- c("t", "score", paste0("V", as.character(c(1:390))))

library(neuralnet)
library(nnet)
tmp <- mutate(DFtrainingNN, NNscore = class.ind(DFtrainingNN$score))
DFtrainingNN$score <- class.ind(DFtrainingNN$score)

n <- names(DFtrainingNN[,c(-1, -2)])
f <- as.formula(paste("score0+score1+score2~", paste(n[!n %in% "score"], collapse = " + ")))
mf <- as.formula(paste("~ score +", paste(n[!n %in% "score"], collapse = " + ")))
m <- model.matrix( 
    mf, 
    data = DFtrainingNN[, -1] 
)

nn <- neuralnet(f, m[, -1], hidden=3, act.fct = "logistic", linear.output=F)

nn <- neuralnet(f, DFtrainingNN[, -1], hidden=c(10,5), linear.output=F)



















