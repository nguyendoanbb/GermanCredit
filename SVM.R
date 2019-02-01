#######Support Vector Machine
library(e1071) #SVM methodology
svm.model <- svm(class~., data=data.train, kernel = 'radial', gamma=1, cost=1)
svm.model
summary(svm.model)

svm.model.tune <- tune(svm, class~., data=data.train, kernel='radial',
                       ranges = list(cost=10^c(-5:5),
                                     gamma=10^c(-5:5)))
summary(svm.model.tune)
svm.model.best <- svm(class~., data=data.train, kernel = 'radial', gamma = 0.01, cost = 10, type = 'C-classification')
svm.predict <- predict(svm.model.best, newdata = data.test, 
                       type='raw', decision.values = TRUE)
table(Truth = data.test[,'class'], Predicted=svm.predict)
svm.misclass <- 1-sum(attributes(svm.predict)$names == data.test$class)/length(svm.predict)

#Implementing dummy variables to SVM
library(lattice)
dummies <- dummyVars(~., data = data.train[,-length(data.train)])
c2 <- predict(dummies, data.train[, -length(data.train)])
d_training <- as.data.frame(cbind(class = data.train$class,c2))

dummies <- dummyVars(~., data=data.test[,-length(data.test)])
c2 <- predict(dummies, data.test[,-length(data.test)])
d_test <- as.data.frame(cbind(class=data.test$class, c2))

d_training$class <- as.factor(d_training$class) # 2=good, 1=bad
d_test$class <- as.factor(d_test$class)

svm.tune <- tune.svm(class~., data=d_training, 
                     kernel='radial', cost=2^(-3:5),
                     gamma = 10^(-3:5))
svm.tune.best <- svm(class~., data=d_training, kernel='radial',
                     cost=2,gamma=0.01,probability=TRUE)
svm.tune.predict <- predict(svm.tune.best, newdata = d_test[,-1], probability = TRUE)
confusionMatrix(svm.tune.predict, as.factor(d_test$class), dnn=c('true','predict'))