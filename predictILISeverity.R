#Team Datadrillers
#setwd('/Users/kalpanatripathi/Documents/sjsu/AWSBackup/ILI_Prediction/data')
set.seed(pi)
rm(list=ls())
require(klaR)
library(MASS)
library(randomForest)
library(rpart.plot)  
library(rattle)   
library(RColorBrewer) 
library(party) 
library(partykit)  
library(caret) 
library(ggplot2)
library(DAAG)
library("parcor")
library(lars)
library(ridge)
library(class)
library(e1071)

# read data from csv database
#ILIData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)

# MYSQL database
library(RMySQL)
library(DBI)
mysqlconnection = dbConnect(MySQL(), user = 'root', password = '', dbname = 'ili_prediction',host = 'localhost')
dbListTables(mysqlconnection)
result = dbSendQuery(mysqlconnection, "select * from finaldataset")
ILIData = fetch(result, n = -1)

train<-ILIData[1:520,1:11]

# use the first 9 columns into train_x
train_x<-train[,1:10]

# use the 11th col ILI Severity for result
train_y<-train[,11]
par(mar = rep(4, 4))

#Use Knn befor scaling data
out <- knn.cv(train_x,train_y,k=1)

print('Error with KNN before scaling:')
(1-sum(abs(train_y == out))/length(out))
train2 <- train_x

#Scaling data with for KNN
for(i in seq(from = 1, to = ncol(train_x))){
  v = var(train_x[,i])
  m = mean(train_x[,i])
  train2[,i] <- (train_x[,i]-m)/sqrt(v)
}
(out <- knn.cv(train2,train_y,k=1))
print('RMSE Error with KNN with k=1 after executing scaling:')
(1-sum(train_y == out)/length(out))
Err <- rep(0,40)
for(jj in seq(from=1,to=40)){
  out <- knn.cv(train2,train_y,k=jj)
  Error <- 1-sum(abs(train_y == out))/length(out)
  Err[jj] <- Error   
}
print('Best values selected for k : ')
(which.min(Err))
print('Minimum error with KNN : ')
(min(Err))
# plot(Err,pch=c(15,15),col="red",xlab = 'K value', ylab='Error')

#Divinding dataset into train dataset & test dataset selected randomly so works just like cross validation
index <- 1:nrow(train2)
train2[,11]<-train_y
testIn <- sample(index, trunc(length(index)/3))
testset <- train2[testIn, ]
trainset <- train2[-testIn, ]

# Using decision trees 
library(rpart)
rpart.model <- rpart(V11 ~ ., data = trainset,control=rpart.control(maxdepth=5, minsplit=2))
prp(rpart.model)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(rpart.model)
rpart.pred <- predict(rpart.model, testset[, -11])
print('RMSE Error for decision trees')
(1-(sum(rpart.pred == testset[,11])/length(testset[,11])))

# Using random forests
trainset[,11] <- as.factor(trainset[, 11])
modelRF <- randomForest(V11 ~ ., data = trainset)
modelRF
plot(modelRF, log="y")
legend("topright", legend=unique(trainset$V11), col=unique(trainset$V11), pch=19)
(predRF <- predict(modelRF, testset[, -11]))
print('RMSE Error for Random forest : ')
(1-(sum(predRF == testset[,11])/length(testset[,11])))


#Using SVM Radial basis function
svm.model <- svm(V11 ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[, -11])
print('RMSE Error for SVM before tuning')
(1-sum(svm.pred == testset[,11])/length(testset[,11]))
# tuning the SVM model
obj <- tune(svm, V11~., data = trainset,
            ranges = list(gamma = 2^(-12:2), cost = 5^(2:4)),
            tunecontrol = tune.control(sampling = "cross"))
obj$best.parameters
plot(obj)
# retry with the best calculated parameters after tuning
svm.model <- svm(V11 ~ ., data = trainset, method = "C-classification",kernel = "radial", cost = 625, gamma = 0.03125)
plot(svm.model, trainset, WeightedILIPCT ~ Region, svSymbol = 1, dataSymbol = 2, 
     color.palette = cm.colors)
svm.pred <- predict(svm.model, testset[, -11])
print('RMSE Error for SVM after tuning : ')
(1-sum(svm.pred == testset[,11])/length(testset[,11]))
#plot(svm.pred)


# Using Linear regression
lmflu <- lm(V11~., data = trainset)
print("Predicting error using above Linear Regression model")
lmFit <- predict(lmflu,newdata = testset[,-11])
accuracy<-sum(round(lmFit)== testset[,11])/length(testset[,11])
print('RMSE Error with linear regression : ')
(1-accuracy)
plot(round(lmFit) ~ WeightedILIPCT, data = testset,ylab='ILI Severity', xlab = '% Weighted ILI', main='Plot of ILI Severity & % Weighted ILI',pch=c(16,16),col="red")

#ridge regression
# library(glmnet)
# grid=10^seq(10,-2,length=100)
# X <- as.matrix(trainset[,-10])
# ridge.mod=glmnet(X,train2[,10],alpha=0,lambda=grid)
# pred<-predict(ridge.mod,s=50,type="coefficients")[1:10,]
# pred
# plot(ridge.mod)



# Visualize the flu data set for whole dataset
library(gridExtra)
colnames(ILIData)=c('Region','Year','Week','Season','Total No. Patients','No. Of Providers','PositiveWeightedPct','% Positive Unweighted','Total Influenza Test Specimens','InfluenzaPositive','ILISeverity','FluSeverity')
m<-qplot(Region,Week, data = ILIData, color = ILISeverity, size=InfluenzaPositive,na.rm = TRUE)
n<-m+scale_y_continuous(breaks=1:52)+scale_x_discrete(labels = c("CT, ME, MA.. ","NJ, NY..","DE, DC, MD..","AL, FL, GA..","IL, IN, MI..","AR, LA, NM..","IA, KS, MO..","CO, MT, ND..","AZ, CA,NV..","AK, ID, OR.."))
n

ggplot(ILIData, aes(x=PositiveWeightedPct)) +
  geom_histogram(position="dodge", binwidth=3) +
  scale_fill_brewer()+scale_x_discrete(breaks=0:8)

