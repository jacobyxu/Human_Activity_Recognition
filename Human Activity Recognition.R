#' ---	
#' title: "Human Activity Recognition"	
#' author: "Puxin Xu"	
#' date: "January 22, 2016"	
#' output: md_document	
#' ---	
#' 	
#' 	
#' 	
library(lattice)	
library(ggplot2)	
library(caret)	
library(rpart)	
library(rpart.plot)	
library(RColorBrewer)	
library(rattle)	
library(randomForest)	
library(knitr)	
library(e1071)	
#' 	
#' 	
#' ##Background	
#' Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement ?C a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har  (see the section on the Weight Lifting Exercise Dataset).	
#' 	
#' ##Data	
#' The training data for this project are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv	
#' 	
#' The test data are available here:	
#' https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv	
#' 	
#' The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har . If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.	
#' 	
#' ##Getting and Cleaning the data	
#' ###Getting the data	
#' 	
if (!dir.exists("data")) { 	
dir.create("./data")}	
setwd("./data")	
fileUrl_training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"	
download.file(fileUrl_training, destfile = "./pml-training.csv")	
fileUrl_testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"	
download.file(fileUrl_testing, destfile = "./pml-testing.csv")	
training <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))	
testing <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))	
#' 	
#' ###Cleaning the data	
#' 	
nzv <- nearZeroVar(training, saveMetrics=TRUE)	
training <- training[,nzv$nzv==FALSE]	
training <- training[c(-1)]	
trainingtemp <- training	
for(i in 1:length(training)) {	
    if( sum( is.na( training[, i] ) ) /nrow(training) >= .7) {	
        for(j in 1:length(trainingtemp)) {	
            if( length( grep(names(training[i]), names(trainingtemp)[j]) ) == 1)  {	
                trainingtemp <- trainingtemp[ , -j]	
            }   	
        } 	
    }	
}	
training <- trainingtemp	
rm(trainingtemp)	
inTrain <- createDataPartition(training$classe, p=0.6, list=FALSE)	
myTraining <- training[inTrain, ]	
myTesting <- training[-inTrain, ]	
clean1 <- colnames(myTraining)	
clean2 <- colnames(myTraining[, -58])	
myTesting <- myTesting[clean1]	
testing <- testing[clean2]	
for (i in 1:length(testing) ) {	
    for(j in 1:length(myTraining)) {	
        if( length( grep(names(myTraining[i]), names(testing)[j]) ) == 1)  {	
            class(testing[j]) <- class(myTraining[i])	
        }      	
    }      	
}	
testing <- rbind(myTraining[2, -58] , testing)	
testing <- testing[-1,]	
#' 	
#' ##Prediction	
#' ###Decision Trees	
#' 	
set.seed(123456)	
modFitA1 <- rpart(classe ~ ., data=myTraining, method="class")	
fancyRpartPlot(modFitA1)	
predictionsA1 <- predict(modFitA1, myTesting, type = "class")	
cmtree <- confusionMatrix(predictionsA1, myTesting$classe)	
cmtree	
plot(cmtree$table, col = cmtree$byClass, main = paste("Decision Tree Confusion Matrix: Accuracy =", round(cmtree$overall['Accuracy'], 4)))	
#' 	
#' 	
#' ###Random Forests	
#' 	
set.seed(123456)	
modFitB1 <- randomForest(classe ~ ., data=myTraining)	
predictionB1 <- predict(modFitB1, myTesting, type = "class")	
cmrf <- confusionMatrix(predictionB1, myTesting$classe)	
cmrf	
plot(modFitB1)	
plot(cmrf$table, col = cmtree$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(cmrf$overall['Accuracy'], 4)))	
#' 	
#' 	
#' ###Generalized Boosted Regression	
#' 	
library(survival)	
library(gbm)	
library(splines)	
library(parallel)	
library(plyr)	
set.seed(123456)	
fitControl <- trainControl(method = "repeatedcv",	
                           number = 5,	
                           repeats = 1)	
	
gbmFit1 <- train(classe ~ ., data=myTraining, method = "gbm",	
                 trControl = fitControl,	
                 verbose = FALSE)	
gbmFinMod1 <- gbmFit1$finalModel	
gbmPredTest <- predict(gbmFit1, newdata=myTesting)	
gbmAccuracyTest <- confusionMatrix(gbmPredTest, myTesting$classe)	
gbmAccuracyTest	
plot(gbmFit1, ylim=c(0.9, 1))	
#' 	
#' 	
#' ##Prediction Results	
#' `Random Forest` gives an Accuracy in the myTesting dataset of `99.89%`, which was more accurate that what I got from the Decision Trees and GBM. The expected `out-of-sample error` is 100%-99.89% = `0.11%`. Thus I choose `Random Forest` to be the final predictor.	
#' 	
predictionB2 <- predict(modFitB1, testing, type = "class")	
predictionB2	
#' 	
#' 	
#' Write the results to a text file for submission	
#' 	
pml_write_files = function(x){	
    n = length(x)	
    for(i in 1:n){	
        filename = paste0("problem_id_",i,".txt")	
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)	
    }	
}	
#pml_write_files(predictionB2)	
#' 	
#' 	
