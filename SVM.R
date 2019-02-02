########################################################################
#Support Vector Machine
library(e1071) #SVM methodology
svm.model <- svm(class~., data=data.train, kernel = 'radial', gamma=1, cost=1)
svm.model
svm.model$decision.values

svm.model.tune <- tune(svm, class~., data=data.train, kernel='radial',
                       ranges = list(cost=2^c(-5:5),
                                     gamma=2^c(-5:5)))
svm.model.best <- svm.model.tune$best.model

svm.predict <- predict(svm.model.best, newdata = data.test)
table(Truth = data.test[,'class'], Predicted=svm.predict)
svm.misclass <- 1-sum(svm.predict == data.test$class)/length(svm.predict)




#Trying dummy variables to SVM to improve accuracy
library(lattice)

#Create dummy variables for train set
dummies <- dummyVars(~., data = data.train[,-length(data.train)])
c2 <- predict(dummies, data.train[, -length(data.train)])
d.train <- as.data.frame(cbind(class = data.train$class,c2))

#Create dummy variables for test set
dummies <- dummyVars(~., data=data.test[,-length(data.test)])
c2 <- predict(dummies, data.test[,-length(data.test)])
d.test <- as.data.frame(cbind(class=data.test$class, c2))

#Convert risk class to factor
d.train$class <- as.factor(ifelse(d.train$class==2,'good','bad'))
d.test$class <- as.factor(ifelse(d.test$class==2,'good','bad'))

svm.tune <- tune.svm(class~., data=d.train, 
                     kernel='radial', cost=2^(-5:5),
                     gamma = 2^(-5:5))
svm.tune.best <- svm.tune$best.model

svm.tune.predict <- predict(svm.tune.best, newdata = d.test[,-1])
svm.conf.matrix <- confusionMatrix(svm.tune.predict, d.test$class, dnn=c('true','predict'), 
                positive = 'good')
svm.accuracy <- round(svm.conf.matrix$overall[1]*100, 2)
svm.bad.good <- svm.conf.matrix$table[3]




#Conclusion
cat('The accuracy by using SVM model is: ', svm.accuracy, '%',
    sep = '')
cat('We predict good risk when it is actually bad risk for',svm.bad.good,'observations')
########################################################################