library(plyr)
library(caret)
library(mlbench)
library(foreign)
library(ggplot2)
library(dplyr)
library(scales)
library(reshape)
library(e1071)
library(klaR)
library(C50)
deposit <- read.table("/Users/Poojitha/Desktop/mining assignment/archive.ics.uci.edu_ml_machine-learning-databases_letter-recognition_letter-recognition.pdf", header= F,sep=",")

summary(deposit)
class(deposit1)
#Exclude duration(V12) variable 
deposit1 <- deposit[,-12]
head(deposit1)
dim(deposit1)
summary(deposit1[1,6])
deposit1[,1:6]
typeof(deposit1)

#is there any missing values?
is.na(deposit1)
#check  whether all columns datatypes are factors or not?
is.factor(deposit1)
catvars <-c(2:7,11,15,16)
#deposit1[2:9],deposit1[,11],deposit1[,15],deposit1[,16]
#coverting to numeric

deposit1[,1:16] <-lapply(deposit1[,1:16], as.numeric)
colnames(deposit1)


class(deposita)
gc()
is.factor(deposit1)
#checking correlation

cormatrix <-  cor(deposit1[,-catvars],use = "pairwise.complete.obs")
table(is.na(cormatrix))
cor.test(deposit1$V3,deposit1$V16)
# Columns indexes to remove

colremove <- findCorrelation(cormatrix, cutoff =0.01 , verbose=T)
?findCorrelation
#column names to remove
colnames(cormatrix[,colremove])
# New dataset with removed columns
deposit2 <- deposit1[,-which(names(deposit1) %in% colnames(cormatrix[,colremove]))]
colnames(deposit2)
#factor levels
faclength <- deposit2 %>% Filter(f = is.factor) %>% sapply(levels) %>% sapply(length) %>% (function(x) x[x<2])
facname <-names(deposit2) %in% names(faclength)
deposit3 <- deposit2[!facname]

#non-zero variance
nzerov <- nearZeroVar(deposit3)
deposit4 <- deposit3[,-nzerov]
#sampling the data

sampdata <- createDataPartition(deposit4$V17, p=0.1, list = FALSE)

# Create Training Data as subset 
trainData <- deposit4[sampdata,]
dim(trainData)
trainData <- lapply(trainData, as.factor)
trainData <- as.data.frame(trainData)

levels(trainData)
class(trainData)
is.factor(trainData)
typeof(trainData)
trainData <-as.factor(trainData)
# creating test data.

testdata <- deposit4[-sampdata,]
levels(testdata)
testdata <- lapply(testdata, as.factor)
testdata <- as.data.frame(testdata)
length(testdata)
#

testdata <- as.factor(testdata)
#10 fold cross validation to train the model

TrainingParam <- trainControl(method = "cv", number = 10) 
# decision tree model
DecTree <- train(trainData[,-9], trainData$V17,method = "C5.0", trControl= TrainingParam ,na.action = na.omit)
warnings()
# check tree
DecTree
#Predict
DTPredict <-predict(DecTree, testdata ,na.action = na.pass)
dim(DTPredict)
# See predictions
DTPredict
na.omit(DTPredict)
length(DTPredict)
colnames(testdata)
# Create confusion matrix
?confusionMatrix
levels(testdata)
is.na(testdata)
length(testdata)
cmatrix <- confusionMatrix(DTPredict, testdata$V17)
cm$overall
cm$byClass

#classification with NN
trainData$V1
levels(testdata$V3)

NoTrainingParameters <- trainControl(method = "none")
set.seed(2464)
NNModel <- train(trainData[,-9], trainData$V17,
                 method = "nnet",MaxNWts = 31246,
                 trControl= NoTrainingParameters,
                 tuneGrid = data.frame(size = 3,
                                       decay = 0))
memory.limit(size = 2500)
NNModel
trainData$V1= NULL
te


NNetPredict <-predict(NNModel, testdata )
# See predictions
NNPredict
# Create confusion matrix
cmNN <-confusionMatrix(NNetPredict, testdata$V17)
cmNN$overall

#SVM model
SVModel <- train(V17 ~ ., data = trainData,
                 method = "svmPoly",
                 trControl= NoTrainingParameters,
                 tuneGrid = data.frame(degree = 1,
                                       scale = 1,
                                       C = 1
                 )
                 
)

SVMPredit <-predict(SVModel, testdata)
#  predictions
SVMPredict
#  confusion matrix
cmSVM <-confusionMatrix(SVMPredict, testdata$V17)
cmSVM$overall


















