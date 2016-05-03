#Team Datadrillers
#setwd('/Users/kalpanatripathi/Documents/sjsu/AWSBackup/ILI_Prediction/data')
#ILIData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)

library(RMySQL)
library(DBI)
mysqlconnection = dbConnect(MySQL(), user = 'root', password = '', dbname = 'ili_prediction',host = 'localhost')
dbListTables(mysqlconnection)
result = dbSendQuery(mysqlconnection, "select * from finaldataset")
ILIData = fetch(result, n = -1)

training<-ILIData[1:520,1:12]
# Using columns 1-9 as training data
training_9<-training[,1:9]
# The 10 column ILI Severity is the result
training_10<-training[,12]
out <- knn.cv(training_9,training_10,k=1)
#Error using KNN before scaling
(1-sum(abs(training_10 == out))/length(out))
training2 <- training_9
for(i in seq(from = 1, to = ncol(training_9))){
  v = var(training_9[,i])
  m = mean(training_9[,i])
  training2[,i] <- (training_9[,i]-m)/sqrt(v)
}


training2[,10]<-training_10
(out <- knn.cv(training2,training_10,k=1))
#Error with KNN with k=1 after scaling:
(1-sum(training_10 == out)/length(out))
Err <- rep(0,40)
for(kk in seq(from=1,to=40)){
  out <- knn.cv(training2,training_10,k=kk)
  Error <- 1-sum(abs(training_10 == out))/length(out)
  Err[kk] <- Error   
}
#Best k value 
(which.min(Err))
#min_Error using Knn
(min(Err))
plot(Err,pch=c(15,15),col="red",xlab = 'K value', ylab='Error')

# Now include ILI postive pct & ILI severity, cos decision trees dont seem to do very well without these attributes
training_9<-training[,1:11]
training_10<-training[,12]
# Not re scaling as it doesn't make much of a change 
training2 <- training_9

index <- 1:nrow(training2)
training2[,12]<-training_10
#Finding the test set
testindex <- sample(index, trunc(length(index)/3))
testset <- training2[testindex, ]
trainingset <- training2[-testindex, ]

# Predicting using decision trees
library(rpart)
rpart.model <- rpart(V12 ~ ., data = trainingset,control=rpart.control(maxdepth=5, minsplit=2))
prp(rpart.model) 
fancyRpartPlot(rpart.model)
rpart.pred <- predict(rpart.model, testset[, -12])
print('Error using decision trees')
(1-(sum(rpart.pred == testset[,12])/length(testset[,12])))

# Predicting using SVM
svm.model <- svm(V12 ~ ., data = trainingset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[, -12])
print('Error using SVM before tuning')
(1-sum(svm.pred == testset[,12])/length(testset[,12]))
obj <- tune(svm, V12~., data = trainingset,
            ranges = list(gamma = 2^(-12:2), cost = 5^(2:4)),
            extratuned = tune.control(sampling = "cross"))

obj$best.parameters
plot(obj)
svm.model <- svm(V12 ~ ., data = trainingset, cost = 625, gamma = 0.00390625)
plot(svm.model, trainingset, WeightedILIPCT ~ Region, svSymbol = 1, dataSymbol = 2, 
     color.palette = cm.colors)

svm.pred <- predict(svm.model, testset[, -12])
print('Error using SVM after tuning : ')
(1-sum(svm.pred == testset[,12])/length(testset[,12]))

# Linear regression
lmILI <- lm(V12~., data = trainingset)
print("Predicting error using linear regression")
lmFit <- predict(lmILI,newdata = testset[,-12])
dataaccuracy<-sum(round(lmFit)== testset[,12])/length(testset[,12])
print('Error using linear regression : ')
(1-dataaccuracy)
plot(round(lmFit) ~ WeightedILIPCT, data = testset,ylab='Flu Severity', xlab = '% Weighted ILI', main='Plot of Flu Severity & % Weighted ILI',pch=c(16,16),col="red")

