library(caret); library(knitr);library(randomForest)
library(rattle); library(rpart.plot);library(rpart)
library(gbm); library(corrplot); library(ggplot2)
library(colorspace); library(e1071)
#load("pml-testing.csv")
#load("pml-training.csv")
#load data
trainData <- read.csv("pml-training.csv"); dim(trainData)
testData <- read.csv("pml-testing.csv"); dim(testData)
set.seed(100)
#create sets
inTrain<-createDataPartition(y=trainData$classe, p=0.7, list=FALSE)
trainSet<- trainData[inTrain, ]
cvSet<- trainData[-inTrain, ]
#find variables near zero
NZV <- nearZeroVar(trainSet)
trainSet <- trainSet[, -NZV]
cvSet  <- cvSet[, -NZV]
dim(trainSet)
#remove variables that are mostly NA (mNA)
mostlyNA <- sapply(trainSet, function(x) mean(is.na(x))) > 0.95
trainSet <- trainSet[, mostlyNA==FALSE]
cvSet <- cvSet[, mostlyNA==FALSE]
#remove identification only variables (columns 1 to 5)
trainSet <- trainSet[, -(1:5)]
cvSet <- cvSet[, -(1:5)]

corMatrix <- cor(trainSet[, -54])
corrplot(corMatrix, order = "FPC", method = "color", type = "lower", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))

#Random Forest model fit
trControlRF <-trainControl(method = "cv", number = 3, verbose = FALSE)
modFitRF <-train(classe ~ ., data = trainSet, method = "rf", trControl = trControlRF)
modFitRF$finalModel

predictRF <- predict(modFitRF, newdata = cvSet)
confMatrixRF <- confusionMatrix(predictRF, cvSet$classe)
confMatrixRF

plot(confMatrixRF$table, col = confMatrixRF$byClass, 
     main = paste("Random Forest - Accuracy =",
                  round(confMatrixRF$overall['Accuracy'], 4)))

#Decision Trees model fit
set.seed(4000)
modFitDT <- rpart(classe ~ ., data=trainSet, method="class")
fancyRpartPlot(modFitDT)

predictDT <- predict(modFitDT, newdata=cvSet, type="class")
confMatrixDT <- confusionMatrix(predictDT, cvSet$classe)
confMatrixDT

plot(confMatrixDT$table, col = confMatrixDT$byClass, 
     main = paste("Decision Tree - Accuracy =",
                  round(confMatrixDT$overall['Accuracy'], 4)))

#Generalized Boosted Model model fit
set.seed(4000)
trControlGBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
modFitGBM  <- train(classe ~ ., data=trainSet, method = "gbm",
                     trControl = trControlGBM, verbose = FALSE)
modFitGBM$finalModel

predictGBM <- predict(modFit_GBM, newdata=cvSet)
confMatrixGBM <- confusionMatrix(predictGBM, cvSet$classe)
confMatrixGBM

plot(confMat_GBM$table, col = confMat_GBM$byClass, 
     main = paste("GBM - Accuracy =", round(confMat_GBM$overall['Accuracy'], 4)))


randomForest <- print(paste("Random Forest - Accuracy =",
                           round(confMatrixRF$overall['Accuracy'], 4)))
decisionTree <- print(paste("Decision Tree - Accuracy =",
                            round(confMatrixDT$overall['Accuracy'], 4)))
GBM <- print(paste("GBM - Accuracy =", round(confMatrixGBM$overall['Accuracy'], 4)))

predictTEST <- predict(modFitRF, newdata=testData)
predictTEST

summary(predictTEST)

predictTEST <- c(as.character(predictTEST))

#Length of the predicted vector
length(predictTEST)

outOfSampleError.accuracy <- sum(predictTEST == cvSet$classe)/length(predictTEST)

outOfSampleError.accuracy

outOfSampleError <- 1 - outOfSampleError.accuracy
outOfSampleError

e <- outOfSampleError * 100
paste0("Out of sample error estimation: ", round(e, digits = 2), "%")