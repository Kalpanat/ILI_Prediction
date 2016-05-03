library(shiny)
library(rpart.plot)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #-------------------------------
  library("randomForest")
  fluData <- read.table('/Users/kalpanatripathi/Documents/sjsu/AWSBackup/ILI_Prediction/ILI_Prediction/data/FinalDataSet_3_3.csv',sep=',',header=TRUE)
  train<-fluData[1:520,1:10]
  # 1st 9 columns are attributes
  
  train_x<-train[,1:9]
  print(train_x)
  
  #10th column (Flu Positive Pct) is the result
  train_y<-as.numeric(train[,10])
  print(train_y)
  
  #Tried out KNN but didnt get good predictions. I'm not scaling the data as I did not see changes in the predictions while using other models
  train2 <- train_x
  index <- 1:nrow(train2)
  train2[,10]<-train_y
  testindex <- sample(index, trunc(length(index)/3))
  testset <- train2[testindex, ]
  trainset <- train2[-testindex, ]
  ##1stfile 
  # Try out decision trees
  library(rpart)
  output$decigPlot <- renderPlot({
    rpart.model <- rpart(V10 ~ ., data = trainset,control=rpart.control(maxdepth=5, minsplit=2))
    prp(rpart.model) 
    plot(rpart.model)
    
    yhat <- predict(rpart.model, trainset[, -10])
    dY <- trainset[,10] - yhat
    print('Train Error with decision trees')
    (trainError <- sqrt(sum(dY*dY))/(length(trainset[,10]))) 
    yhat <- predict(rpart.model, testset[, -10])
    dY <- testset[,10] - yhat
    print('Test Error with decision trees')
    (testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 
  })
  #-----------------------------------------------------------
  #library("svm")
  output$gPlot <- renderPlot({
    svm.model <- svm(V10 ~ ., data = trainset, cost = 100, gamma = 1)
    yhat<- predict(svm.model, testset[, -10])
    dY <- testset[,10] - yhat
    print('Test Error with SVM before tuning')
    (testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 
    # tune the model
    obj <- tune(svm, V10~., data = trainset,
                ranges = list(gamma = 2^(-12:2), cost = 5^(2:4)),
                tunecontrol = tune.control(sampling = "cross"))
    
    obj$best.parameters
    plot(obj)
    # re build model using the best parameters
    svm.model <- svm(V10 ~ ., data = trainset, cost = 25, gamma = 0.125)
    plot(svm.model, trainset, FluSpecimens ~ Region,color.palette = cm.colors)
    yhat<- predict(svm.model, trainset[, -10])
    dY <- trainset[,10] - yhat
    print('Train Error with SVM after tuning')
    (trainError <- sqrt(sum(dY*dY))/(length(trainset[,10]))) 
    
    #test error
    yhat<- predict(svm.model, testset[, -10])
    dY <- testset[,10] - yhat
    print('Test Error with SVM after tuning')
    (testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 
    
    
  })
  #--------------------------------------------------------------
  output$graphPlot <- renderPlot({
    
    
    # Try out random forest
    modelRF <- randomForest(V10 ~ ., data = trainset)
    modelRF
    plot(modelRF, log="y")
    
  })
  
  
  #-------------------------------------------------------------------------
  output$lmPlot <- renderPlot({
    lmflu <- lm(V10~., data = trainset)
    print("Predicting error using above model")
    lmFit <- predict(lmflu,newdata = testset[,-10])
    dY <- round(testset[,10]) - round(lmFit)
    (testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 
    print('Error with linear regression : ')
    testError
    plot(round(lmFit) ~ WeightedILIPCT, data = testset,ylab='% Flu Positive', xlab = '% Weighted ILI', main='Plot of % Flu Positive & % Weighted ILI',pch=c(16,16),col="red")
  })
  
  #----------------------------------------------------------------------------
  output$histPlot <- renderPlot({
    library(maps)
    library(maptools)
    library(sp)
    txt <- "CT ME MA NH RI VT NJ NY PR VI DE MP MD PA VA WV AL FL GA KY MS NC SC TN IL IN MI MN OH WI AR LA NM OK TX IA KS MO NE CO MT ND SD UT WY AZ CA HI NV AK ID OR WA FM GU AS PW
    1 1 1 1 1 1 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 3 3 3 3 3 5 5 5 5 4 4 4 4 4 4 5 5 5 5 3 3 3 3 8 10 7 9"
    
    
    dat <- stack(read.table(text = txt,  header = TRUE))
    names(dat)[2] <-'state.abb'
    dat$states <- tolower(state.name[match(dat$state.abb,  state.abb)])
    mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
    nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
    USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
    idx <- match(unique(nms),  dat$states)
    dat2 <- data.frame(value = dat$value[idx], state = unique(nms))
    row.names(dat2) <- unique(nms)
    
    USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
    spplot(USAsp['value'],xlab='Regions',ylab='ILI Severity')
    
  })
  ##2nd file
  output$gPlot4 <- renderPlot({
    
    svm.model <- svm(V12 ~ ., data = trainset, cost = 100, gamma = 1)
    svm.pred <- predict(svm.model, testset[, -12])
    print('Error with SVM before tuning')
    (1-sum(svm.pred == testset[,12])/length(testset[,12]))
    obj <- tune(svm, V12~., data = trainset,
                ranges = list(gamma = 2^(-12:2), cost = 5^(2:4)),
                tunecontrol = tune.control(sampling = "cross"))
    
    obj$best.parameters
    plot(obj)
    svm.model <- svm(V12 ~ ., data = trainset, cost = 625, gamma = 0.00390625)
    plot(svm.model, trainset, WeightedILIPCT ~ Region, svSymbol = 1, dataSymbol = 2, 
         color.palette = cm.colors)
    
    
  })
  
  output$gPlot2 <- renderPlot({
    setwd('/Users/kalpanatripathi/Documents/sjsu/AWSBackup/ILI_Prediction/ILI_Prediction/data/')
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:12]
    # Use columns 1-9 as training data
    train_x<-train[,1:9]
    # The last column Flu Severity is the result
    train_y<-train[,12]
    par(mar = rep(4, 4))
    # out <- knn.cv(train_x,train_y,k=1)
    print('Error with KNN before scaling:')
    #(1-sum(abs(train_y == out) )/length(out))
    train2 <- train_x
    for(i in seq(from = 1, to = ncol(train_x))){
      v = var(train_x[,i])
      m = mean(train_x[,i])
      train2[,i] <- (train_x[,i]-m)/sqrt(v)
    }
    
    
    train2[,10]<-train_y
    # (out <- knn.cv(train2,train_y,k=1))
    print('Error with KNN with k=1 after scaling:')
    #(1-sum(train_y == out)/length(out))
    Err <- rep(0,40)
    # for(kk in seq(from=1,to=40)){
    #out <- knn.cv(train2,train_y,k=kk)
    # Error <- 1-sum(abs(train_y == out))/length(out)
    # Err[kk] <- Error   
    #}
    print('Best k value : ')
    (which.min(Err))
    print('min Error with Knn : ')
    (min(Err))
    plot(Err,pch=c(15,15),col="red",xlab = 'K value', ylab='Error')
    
    # Now include flu postive pct & ILI severity, cos decision trees dont seem to do very well without these attributes
    train_x<-train[,1:11]
    train_y<-train[,12]
    # Not re scaling as it doesn't make much of a change for the other methods
    train2 <- train_x
    # for(i in seq(from = 1, to = ncol(train_x))){
    #   v = var(train_x[,i])
    #   m = mean(train_x[,i])
    #   train2[,i] <- (train_x[,i]-m)/sqrt(v)
    # }
    index <- 1:nrow(train2)
    train2[,12]<-train_y
    #find the test set
    testindex <- sample(index, trunc(length(index)/3))
    testset <- train2[testindex, ]
    trainset <- train2[-testindex, ]
    
    # Predict using decision trees
    library(rpart)
    rpart.model <- rpart(V12 ~ ., data = trainset,control=rpart.control(maxdepth=5, minsplit=2))
    prp(rpart.model) 
    plot(rpart.model)
    rpart.pred <- predict(rpart.model, testset[, -12])
    print('Error with decision trees')
    (1-(sum(rpart.pred == testset[,12])/length(testset[,12])))
    
    # Treat the result as factor and predict with random forests
    trainset[,12] <- as.factor(trainset[, 12])
    modelRF <- randomForest(V12 ~ ., data = trainset)
    modelRF
    plot(modelRF, log="y")
    
    
  })
  
  output$gPlot3 <- renderPlot({
    setwd('/Users/kalpanatripathi/Documents/sjsu/AWSBackup/ILI_Prediction/ILI_Prediction/data/')
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:12]
    # Use columns 1-9 as training data
    train_x<-train[,1:9]
    # The last column Flu Severity is the result
    train_y<-train[,12]
    par(mar = rep(4, 4))
    # out <- knn.cv(train_x,train_y,k=1)
    print('Error with KNN before scaling:')
    #(1-sum(abs(train_y == out) )/length(out))
    train2 <- train_x
    for(i in seq(from = 1, to = ncol(train_x))){
      v = var(train_x[,i])
      m = mean(train_x[,i])
      train2[,i] <- (train_x[,i]-m)/sqrt(v)
    }
    
    
    train2[,10]<-train_y
    # (out <- knn.cv(train2,train_y,k=1))
    print('Error with KNN with k=1 after scaling:')
    #(1-sum(train_y == out)/length(out))
    Err <- rep(0,40)
    # for(kk in seq(from=1,to=40)){
    #out <- knn.cv(train2,train_y,k=kk)
    # Error <- 1-sum(abs(train_y == out))/length(out)
    # Err[kk] <- Error   
    #}
    print('Best k value : ')
    (which.min(Err))
    print('min Error with Knn : ')
    (min(Err))
    plot(Err,pch=c(15,15),col="red",xlab = 'K value', ylab='Error')
    
    # Now include flu postive pct & ILI severity, cos decision trees dont seem to do very well without these attributes
    train_x<-train[,1:11]
    train_y<-train[,12]
    # Not re scaling as it doesn't make much of a change for the other methods
    train2 <- train_x
    # for(i in seq(from = 1, to = ncol(train_x))){
    #   v = var(train_x[,i])
    #   m = mean(train_x[,i])
    #   train2[,i] <- (train_x[,i]-m)/sqrt(v)
    # }
    index <- 1:nrow(train2)
    train2[,12]<-train_y
    #find the test set
    testindex <- sample(index, trunc(length(index)/3))
    testset <- train2[testindex, ]
    trainset <- train2[-testindex, ]
    lmflu <- lm(V12~., data = trainset)
    print("Predicting error using above model")
    lmFit <- predict(lmflu,newdata = testset[,-12])
    accuracy<-sum(round(lmFit)== testset[,12])/length(testset[,12])
    print('Error with linear regression : ')
    (1-accuracy)
    plot(round(lmFit) ~ WeightedILIPCT, data = testset,ylab='Flu Severity', xlab = '% Weighted ILI', main='Plot of Flu Severity & % Weighted ILI',pch=c(16,16),col="red")
    
  })
  ##3rd file
  
  output$gPlot11 <- renderPlot({
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
    #library(ridge)
    library(class)
    #setwd('data')
    library(e1071)
    # read in the data set
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:11]
    # Store the first 9 columns (exclude flu positive pct) into train_x
    train_x<-train[,1:9]
    # Store the 11th col ILI Severity as the result
    train_y<-train[,11]
    par(mar = rep(4, 4))
    #Try out knn before scaling
    out <- knn.cv(train_x,train_y,k=1)
    print('Error with KNN before scaling:')
    (1-sum(abs(train_y == out))/length(out))
    train2 <- train_x
    #Now scale the data
    for(i in seq(from = 1, to = ncol(train_x))){
      v = var(train_x[,i])
      m = mean(train_x[,i])
      train2[,i] <- (train_x[,i]-m)/sqrt(v)
    }
    (out <- knn.cv(train2,train_y,k=1))
    print('Error with KNN with k=1 after scaling:')
    (1-sum(train_y == out)/length(out))
    Err <- rep(0,40)
    for(kk in seq(from=1,to=40)){
      out <- knn.cv(train2,train_y,k=kk)
      Error <- 1-sum(abs(train_y == out))/length(out)
      Err[kk] <- Error   
    }
    print('Best k value : ')
    (which.min(Err))
    print('min Error with Knn : ')
    (min(Err))
    # plot(Err,pch=c(15,15),col="red",xlab = 'K value', ylab='Error')
    
    #Now Divide into trainset & testset selected randomly so works just like cross validation
    index <- 1:nrow(train2)
    train2[,10]<-train_y
    testindex <- sample(index, trunc(length(index)/3))
    testset <- train2[testindex, ]
    trainset <- train2[-testindex, ]
    
    trainset[,10] <- as.factor(trainset[, 10])
    modelRF <- randomForest(V10 ~ ., data = trainset)
    modelRF
    plot(modelRF, log="y")
  })
  
  output$gPlot12 <- renderPlot({
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
    #library(ridge)
    library(class)
    #setwd('data')
    library(e1071)
    # read in the data set
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:11]
    # Store the first 9 columns (exclude flu positive pct) into train_x
    train_x<-train[,1:9]
    # Store the 11th col ILI Severity as the result
    train_y<-train[,11]
    par(mar = rep(4, 4))
    #Try out knn before scaling
    out <- knn.cv(train_x,train_y,k=1)
    print('Error with KNN before scaling:')
    (1-sum(abs(train_y == out))/length(out))
    train2 <- train_x
    #Now scale the data
    for(i in seq(from = 1, to = ncol(train_x))){
      v = var(train_x[,i])
      m = mean(train_x[,i])
      train2[,i] <- (train_x[,i]-m)/sqrt(v)
    }
    (out <- knn.cv(train2,train_y,k=1))
    print('Error with KNN with k=1 after scaling:')
    (1-sum(train_y == out)/length(out))
    Err <- rep(0,40)
    for(kk in seq(from=1,to=40)){
      out <- knn.cv(train2,train_y,k=kk)
      Error <- 1-sum(abs(train_y == out))/length(out)
      Err[kk] <- Error   
    }
    print('Best k value : ')
    (which.min(Err))
    print('min Error with Knn : ')
    (min(Err))
    # plot(Err,pch=c(15,15),col="red",xlab = 'K value', ylab='Error')
    
    #Now Divide into trainset & testset selected randomly so works just like cross validation
    index <- 1:nrow(train2)
    train2[,10]<-train_y
    testindex <- sample(index, trunc(length(index)/3))
    testset <- train2[testindex, ]
    trainset <- train2[-testindex, ]
    svm.model <- svm(V10 ~ ., data = trainset, cost = 625, gamma = 0.03125)
    plot(svm.model, trainset, WeightedILIPCT ~ Region, svSymbol = 1, dataSymbol = 2, 
         color.palette = cm.colors)
    svm.pred <- predict(svm.model, testset[, -10])
    print('Error with SVM after tuning : ')
    (1-sum(svm.pred == testset[,10])/length(testset[,10]))
    plot(svm.pred)
  })
  
  output$gPlot13 <- renderPlot({
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
    #library(ridge)
    library(class)
    #setwd('data')
    library(e1071)
    # read in the data set
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:11]
    # Store the first 9 columns (exclude flu positive pct) into train_x
    train_x<-train[,1:9]
    # Store the 11th col ILI Severity as the result
    train_y<-train[,11]
    par(mar = rep(4, 4))
    #Try out knn before scaling
    out <- knn.cv(train_x,train_y,k=1)
    print('Error with KNN before scaling:')
    (1-sum(abs(train_y == out))/length(out))
    train2 <- train_x
    #Now scale the data
    for(i in seq(from = 1, to = ncol(train_x))){
      v = var(train_x[,i])
      m = mean(train_x[,i])
      train2[,i] <- (train_x[,i]-m)/sqrt(v)
    }
    (out <- knn.cv(train2,train_y,k=1))
    print('Error with KNN with k=1 after scaling:')
    (1-sum(train_y == out)/length(out))
    Err <- rep(0,40)
    for(kk in seq(from=1,to=40)){
      out <- knn.cv(train2,train_y,k=kk)
      Error <- 1-sum(abs(train_y == out))/length(out)
      Err[kk] <- Error   
    }
    print('Best k value : ')
    (which.min(Err))
    print('min Error with Knn : ')
    (min(Err))
    # plot(Err,pch=c(15,15),col="red",xlab = 'K value', ylab='Error')
    
    #Now Divide into trainset & testset selected randomly so works just like cross validation
    index <- 1:nrow(train2)
    train2[,10]<-train_y
    testindex <- sample(index, trunc(length(index)/3))
    testset <- train2[testindex, ]
    trainset <- train2[-testindex, ]
    
    
    lmflu <- lm(V10~., data = trainset)
    print("Predicting error using above model")
    lmFit <- predict(lmflu,newdata = testset[,-10])
    accuracy<-sum(round(lmFit)== testset[,10])/length(testset[,10])
    print('Error with linear regression : ')
    (1-accuracy)
    plot(round(lmFit) ~ WeightedILIPCT, data = testset,ylab='ILI Severity', xlab = '% Weighted ILI', main='Plot of ILI Severity & % Weighted ILI',pch=c(16,16),col="red")
    
  })
  output$valueP1 <- renderPrint({
    
    setwd('/Users/kalpanatripathi/Documents/sjsu/AWSBackup/ILI_Prediction/ILI_Prediction/data/')
    # read in the data set
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:10]
    # 1st 9 columns are attributes
    train_x<-train[,1:9]
    #10th column (Flu Positive Pct) is the result
    train_y<-as.numeric(train[,10])
    #Tried out KNN but didnt get good predictions. I'm not scaling the data as I did not see changes in the predictions while using other models
    train2 <- train_x  
    
    # Now try dividing into train & test sets using random sampling
    index <- 1:nrow(train2)
    train2[,10]<-train_y
    testindex <- sample(index, trunc(length(index)/3))
    testset <- train2[testindex, ]
    trainset <- train2[-testindex, ]
    svm.model <- svm(V10 ~ ., data = trainset, cost = 100, gamma = 1)
    yhat<- predict(svm.model, testset[, -10])
    dY <- testset[,10] - yhat
    #print('Test Error with SVM before tuning')
    (testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 
    # tune the model
    obj <- tune(svm, V10~., data = trainset,
                ranges = list(gamma = 2^(-12:2), cost = 5^(2:4)),
                tunecontrol = tune.control(sampling = "cross"))
    
    obj$best.parameters
    plot(obj)
    # re build model using the best parameters
    svm.model <- svm(V10 ~ ., data = trainset, cost = 25, gamma = 0.125)
    plot(svm.model, trainset, FluSpecimens ~ Region,color.palette = cm.colors)
    yhat<- predict(svm.model, trainset[, -10])
    dY <- trainset[,10] - yhat
    # print('Train Error with SVM after tuning')
    (trainError <- sqrt(sum(dY*dY))/(length(trainset[,10]))) 
    
    #test error
    yhat<- predict(svm.model, testset[, -10])
    dY <- testset[,10] - yhat
    #print('Test Error with SVM after tuning')
    print(testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 
    
  })
  
  output$valueP2 <- renderPrint({
    setwd('/Users/kalpanatripathi/Documents/sjsu/AWSBackup/ILI_Prediction/ILI_Prediction/data/')
    # read in the data set
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:10]
    # 1st 9 columns are attributes
    train_x<-train[,1:9]
    #10th column (Flu Positive Pct) is the result
    train_y<-as.numeric(train[,10])
    #Tried out KNN but didnt get good predictions. I'm not scaling the data as I did not see changes in the predictions while using other models
    train2 <- train_x  
    
    # Now try dividing into train & test sets using random sampling
    index <- 1:nrow(train2)
    train2[,10]<-train_y
    testindex <- sample(index, trunc(length(index)/3))
    testset <- train2[testindex, ]
    trainset <- train2[-testindex, ]
    
    # Try out decision trees
    library(rpart)
    rpart.model <- rpart(V10 ~ ., data = trainset,control=rpart.control(maxdepth=5, minsplit=2))
    prp(rpart.model) 
    fancyRpartPlot(rpart.model)
    yhat <- predict(rpart.model, trainset[, -10])
    dY <- trainset[,10] - yhat
    
    (trainError <- sqrt(sum(dY*dY))/(length(trainset[,10]))) 
    yhat <- predict(rpart.model, testset[, -10])
    dY <- testset[,10] - yhat
    
    print(testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 
    
    
  })
  output$valueP3 <- renderPrint({
    setwd('/Users/kalpanatripathi/Documents/sjsu/AWSBackup/ILI_Prediction/ILI_Prediction/data/')
    # read in the data set
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:10]
    # 1st 9 columns are attributes
    train_x<-train[,1:9]
    #10th column (Flu Positive Pct) is the result
    train_y<-as.numeric(train[,10])
    #Tried out KNN but didnt get good predictions. I'm not scaling the data as I did not see changes in the predictions while using other models
    train2 <- train_x  
    
    # Now try dividing into train & test sets using random sampling
    index <- 1:nrow(train2)
    train2[,10]<-train_y
    testindex <- sample(index, trunc(length(index)/3))
    testset <- train2[testindex, ]
    trainset <- train2[-testindex, ]
    
    lmflu <- lm(V10~., data = trainset)
    
    lmFit <- predict(lmflu,newdata = testset[,-10])
    dY <- round(testset[,10]) - round(lmFit)
    (testError <- sqrt(sum(dY*dY))/(length(testset[,10]))) 
    
    print(testError)
    
  })
  
  output$valueP4 <- renderPrint({
    
    setwd('/Users/kalpanatripathi/Documents/sjsu/AWSBackup/ILI_Prediction/ILI_Prediction/data/')
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:12]
    # Use columns 1-9 as training data
    train_x<-train[,1:9]
    # The last column Flu Severity is the result
    train_y<-train[,12]
    par(mar = rep(4, 4))
    out <- knn.cv(train_x,train_y,k=1)
    #print('Error with KNN before scaling:')
    (1-sum(abs(train_y == out) )/length(out))
    train2 <- train_x
    for(i in seq(from = 1, to = ncol(train_x))){
      v = var(train_x[,i])
      m = mean(train_x[,i])
      train2[,i] <- (train_x[,i]-m)/sqrt(v)
    }
    
    
    train2[,10]<-train_y
    
    # Now include flu postive pct & ILI severity, cos decision trees dont seem to do very well without these attributes
    train_x<-train[,1:11]
    train_y<-train[,12]
    # Not re scaling as it doesn't make much of a change for the other methods
    train2 <- train_x
    # for(i in seq(from = 1, to = ncol(train_x))){
    #   v = var(train_x[,i])
    #   m = mean(train_x[,i])
    #   train2[,i] <- (train_x[,i]-m)/sqrt(v)
    # }
    index <- 1:nrow(train2)
    train2[,12]<-train_y
    #find the test set
    testindex <- sample(index, trunc(length(index)/3))
    testset <- train2[testindex, ]
    trainset <- train2[-testindex, ]
    
    # Predict using decision trees
    library(rpart)
    rpart.model <- rpart(V12 ~ ., data = trainset,control=rpart.control(maxdepth=5, minsplit=2))
    prp(rpart.model) 
    #fancyRpartPlot(rpart.model)
    rpart.pred <- predict(rpart.model, testset[, -12])
    #print('Error with decision trees')
    print(1-(sum(rpart.pred == testset[,12])/length(testset[,12])))
    
  })
  
  output$valueP5 <- renderPrint({
    setwd('/Users/kalpanatripathi/Documents/sjsu/AWSBackup/ILI_Prediction/ILI_Prediction/data/')
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:12]
    # Use columns 1-9 as training data
    train_x<-train[,1:9]
    # The last column Flu Severity is the result
    train_y<-train[,12]
    par(mar = rep(4, 4))
    out <- knn.cv(train_x,train_y,k=1)
    # print('Error with KNN before scaling:')
    (1-sum(abs(train_y == out) )/length(out))
    train2 <- train_x
    for(i in seq(from = 1, to = ncol(train_x))){
      v = var(train_x[,i])
      m = mean(train_x[,i])
      train2[,i] <- (train_x[,i]-m)/sqrt(v)
    }
    
    
    train2[,10]<-train_y
    (out <- knn.cv(train2,train_y,k=1))
    #print('Error with KNN with k=1 after scaling:')
    (1-sum(train_y == out)/length(out))
    Err <- rep(0,40)
    for(kk in seq(from=1,to=40)){
      out <- knn.cv(train2,train_y,k=kk)
      Error <- 1-sum(abs(train_y == out))/length(out)
      Err[kk] <- Error   
    }
    #print('Best k value : ')
    (which.min(Err))
    #print('min Error with Knn : ')
    (min(Err))
    #plot(Err,pch=c(15,15),col="red",xlab = 'K value', ylab='Error')
    
    # Now include flu postive pct & ILI severity, cos decision trees dont seem to do very well without these attributes
    train_x<-train[,1:11]
    train_y<-train[,12]
    # Not re scaling as it doesn't make much of a change for the other methods
    train2 <- train_x
    
    index <- 1:nrow(train2)
    train2[,12]<-train_y
    #find the test set
    testindex <- sample(index, trunc(length(index)/3))
    testset <- train2[testindex, ]
    trainset <- train2[-testindex, ]
    svm.model <- svm(V12 ~ ., data = trainset, cost = 100, gamma = 1)
    svm.pred <- predict(svm.model, testset[, -12])
    #print('Error with SVM before tuning')
    (1-sum(svm.pred == testset[,12])/length(testset[,12]))
    obj <- tune(svm, V12~., data = trainset,
                ranges = list(gamma = 2^(-12:2), cost = 5^(2:4)),
                tunecontrol = tune.control(sampling = "cross"))
    
    obj$best.parameters
    #plot(obj)
    svm.model <- svm(V12 ~ ., data = trainset, cost = 625, gamma = 0.00390625)
    plot(svm.model, trainset, WeightedILIPCT ~ Region, svSymbol = 1, dataSymbol = 2, 
         color.palette = cm.colors)
    svm.pred <- predict(svm.model, testset[, -12])
    #print('Error with SVM after tuning : ')
    (1-sum(svm.pred == testset[,12])/length(testset[,12]))
    print(1-sum(svm.pred == testset[,12])/length(testset[,12]))
    
    
    
  })
  output$valueP6 <- renderPrint({
    setwd('/Users/kalpanatripathi/Documents/sjsu/AWSBackup/ILI_Prediction/ILI_Prediction/data/')
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:12]
    # Use columns 1-9 as training data
    train_x<-train[,1:9]
    # The last column Flu Severity is the result
    train_y<-train[,12]
    par(mar = rep(4, 4))
    out <- knn.cv(train_x,train_y,k=1)
    #print('Error with KNN before scaling:')
    (1-sum(abs(train_y == out) )/length(out))
    train2 <- train_x
    for(i in seq(from = 1, to = ncol(train_x))){
      v = var(train_x[,i])
      m = mean(train_x[,i])
      train2[,i] <- (train_x[,i]-m)/sqrt(v)
    }
    
    
    train2[,10]<-train_y
    (out <- knn.cv(train2,train_y,k=1))
    #print('Error with KNN with k=1 after scaling:')
    (1-sum(train_y == out)/length(out))
    Err <- rep(0,40)
    for(kk in seq(from=1,to=40)){
      out <- knn.cv(train2,train_y,k=kk)
      Error <- 1-sum(abs(train_y == out))/length(out)
      Err[kk] <- Error   
    }
    #print('Best k value : ')
    (which.min(Err))
    #print('min Error with Knn : ')
    (min(Err))
    #plot(Err,pch=c(15,15),col="red",xlab = 'K value', ylab='Error')
    
    # Now include flu postive pct & ILI severity, cos decision trees dont seem to do very well without these attributes
    train_x<-train[,1:11]
    train_y<-train[,12]
    # Not re scaling as it doesn't make much of a change for the other methods
    train2 <- train_x
    
    index <- 1:nrow(train2)
    train2[,12]<-train_y
    #find the test set
    testindex <- sample(index, trunc(length(index)/3))
    testset <- train2[testindex, ]
    trainset <- train2[-testindex, ]
    lmflu <- lm(V12~., data = trainset)
    #print("Predicting error using above model")
    lmFit <- predict(lmflu,newdata = testset[,-12])
    accuracy<-sum(round(lmFit)== testset[,12])/length(testset[,12])
    #print('Error with linear regression : ')
    (1-accuracy)
  })
  output$valueP7 <- renderPrint({
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
    #library(ridge)
    library(class)
    #setwd('data')
    library(e1071)
    # read in the data set
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:11]
    # Store the first 9 columns (exclude flu positive pct) into train_x
    train_x<-train[,1:9]
    # Store the 11th col ILI Severity as the result
    train_y<-train[,11]
    par(mar = rep(4, 4))
    #Try out knn before scaling
    out <- knn.cv(train_x,train_y,k=1)
    #print('Error with KNN before scaling:')
    (1-sum(abs(train_y == out))/length(out))
    train2 <- train_x
    #Now scale the data
    for(i in seq(from = 1, to = ncol(train_x))){
      v = var(train_x[,i])
      m = mean(train_x[,i])
      train2[,i] <- (train_x[,i]-m)/sqrt(v)
    }
    svm.model <- svm(V10 ~ ., data = trainset, cost = 100, gamma = 1)
    svm.pred <- predict(svm.model, testset[, -10])
    #print('Error with SVM before tuning')
    (1-sum(svm.pred == testset[,10])/length(testset[,10]))
    # tune the model
    obj <- tune(svm, V10~., data = trainset,
                ranges = list(gamma = 2^(-12:2), cost = 5^(2:4)),
                tunecontrol = tune.control(sampling = "cross"))
    obj$best.parameters
    #plot(obj)
    # retry with the best parameters after tuning
    svm.model <- svm(V10 ~ ., data = trainset, cost = 625, gamma = 0.03125)
    #plot(svm.model, trainset, WeightedILIPCT ~ Region, svSymbol = 1, dataSymbol = 2, 
    #color.palette = cm.colors)
    svm.pred <- predict(svm.model, testset[, -10])
    #print('Error with SVM after tuning : ')
    print(1-sum(svm.pred == testset[,10])/length(testset[,10]))
    
  })
  
  output$valueP8 <- renderPrint({
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
    # library(ridge)
    library(class)
    #setwd('data')
    library(e1071)
    # read in the data set
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:11]
    # Store the first 9 columns (exclude flu positive pct) into train_x
    train_x<-train[,1:9]
    # Store the 11th col ILI Severity as the result
    train_y<-train[,11]
    par(mar = rep(4, 4))
    #Try out knn before scaling
    out <- knn.cv(train_x,train_y,k=1)
    #print('Error with KNN before scaling:')
    (1-sum(abs(train_y == out))/length(out))
    train2 <- train_x
    #Now scale the data
    for(i in seq(from = 1, to = ncol(train_x))){
      v = var(train_x[,i])
      m = mean(train_x[,i])
      train2[,i] <- (train_x[,i]-m)/sqrt(v)
    }
    library(rpart)
    rpart.model <- rpart(V10 ~ ., data = trainset,control=rpart.control(maxdepth=5, minsplit=2))
    prp(rpart.model) 
    # fancyRpartPlot(rpart.model)
    rpart.pred <- predict(rpart.model, testset[, -10])
    #print('Error with decision trees')
    print(1-(sum(rpart.pred == testset[,10])/length(testset[,10])))
    
  })
  
  output$valueP9 <- renderPrint ({
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
    #library(ridge)
    library(class)
    #setwd('data')
    library(e1071)
    # read in the data set
    fluData <- read.table('FinalDataSet_3_3.csv',sep=',',header=TRUE)
    train<-fluData[1:520,1:11]
    # Store the first 9 columns (exclude flu positive pct) into train_x
    train_x<-train[,1:9]
    # Store the 11th col ILI Severity as the result
    train_y<-train[,11]
    par(mar = rep(4, 4))
    #Try out knn before scaling
    out <- knn.cv(train_x,train_y,k=1)
    #print('Error with KNN before scaling:')
    (1-sum(abs(train_y == out))/length(out))
    train2 <- train_x
    #Now scale the data
    for(i in seq(from = 1, to = ncol(train_x))){
      v = var(train_x[,i])
      m = mean(train_x[,i])
      train2[,i] <- (train_x[,i]-m)/sqrt(v)
    }
    lmflu <- lm(V10~., data = trainset)
    #print("Predicting error using above model")
    lmFit <- predict(lmflu,newdata = testset[,-10])
    accuracy<-sum(round(lmFit)== testset[,10])/length(testset[,10])
    #print('Error with linear regression : ')
    print(1-accuracy)
  })
})