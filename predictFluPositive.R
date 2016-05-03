setwd('/Users/kalpanatripathi/Documents/sjsu/AWSBackup/ILI_Prediction/data')
# Reading the data set
#ILIData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)

#MySQL Database
library(RMySQL)
library(DBI)
mysqlconnection = dbConnect(MySQL(), user = 'root', password = '', dbname = 'ili_prediction',host = 'localhost')
dbListTables(mysqlconnection)
result = dbSendQuery(mysqlconnection, "select * from finaldataset")
ILIData = fetch(result, n = -1)

training<-ILIData[1:520,1:10]
# The 9 columns are attributes
training_9<-training[,1:9]
#The 10 column(ILI Positive Pct) is the result
training_10<-as.numeric(training[,10])

# Here we are not scaling the data as we did not see changes in the predictions.
training2 <- training_9

#Here we are dividing into training and test sets using random sampling.
index <- 1:nrow(training2)
training2[,10]<-training_10
testindex <- sample(index, trunc(length(index)/3))
testset <- training2[testindex, ]
trainingset <- training2[-testindex, ]

#Using Decision Trees
library(rattle)  
library(rpart)
library(RColorBrewer)
rpart.model <- rpart(V10 ~ ., data = trainingset,control=rpart.control(maxdepth=5, minsplit=2))
prp(rpart.model) 
fancyRpartPlot(rpart.model)
predictedata <- predict(rpart.model, trainingset[, -10])
error <- trainingset[,10] - predictedata
#Training Error using decision trees
(trainingError <- sqrt(sum(error*error))/(length(trainingset[,10]))) 
predictedata <- predict(rpart.model, testset[, -10])
error <- testset[,10] - predictedata
#Test Error using decision trees
(testError <- sqrt(sum(error*error))/(length(testset[,10]))) 

# Using SVM
svm.model <- svm(V10 ~ ., data = trainingset, cost = 100, gamma = 1)
predictedata<- predict(svm.model, testset[, -10])
error <- testset[,10] - predictedata
#Test Error using SVM before tuning
(testError <- sqrt(sum(error*error))/(length(testset[,10]))) 
# Tuning the model
obj <- tune(svm, V10~., data = trainingset,
            ranges = list(gamma = 2^(-12:2), cost = 5^(2:4)),
            extratuned = tune.control(sampling = "cross"))

obj$best.parameters
plot(obj,col.lab='green',col.main="red")
# Re-building the model using the best parameters
svm.model <- svm(V10 ~ ., data = trainingset, cost = 25, gamma = 0.125)
#plot(svm.model, trainingset, FluSpecimens ~ Region,color.palette = rainbow.colors)
predictedata<- predict(svm.model, trainingset[, -10])
error <- trainingset[,10] - predictedata
#Training Error using SVM after tuning
(trainingError <- sqrt(sum(error*error))/(length(trainingset[,10]))) 

#Test error
predictedata<- predict(svm.model, testset[, -10])
error <- testset[,10] - predictedata
#Test Error using SVM after tuning
(testError <- sqrt(sum(error*error))/(length(testset[,10]))) 


#Using Linear regression
lmILI <- lm(V10~., data = trainingset)
#Predicting the error using linear regression
lmFit <- predict(lmILI,newdata = testset[,-10])
error <- round(testset[,10]) - round(lmFit)
(testError <- sqrt(sum(error*error))/(length(testset[,10]))) 
#Error using linear regression 
testError
plot(round(lmFit) ~ WeightedILIPCT, data = testset,ylab='% Flu Positive', xlab = '% Weighted ILI', main='Plot of % Flu Positive & % Weighted ILI',pch=c(10,10),bg="black",col.axis = 'blue', col.lab = 'darkgreen',col.main="darkred",col="red")
