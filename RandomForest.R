########################################################################
#Random forest
rf.model <- randomForest(class~., data=data.train, importance = TRUE, ntree=300, mtry=5,
                         nodesize = 40)
plot(rf.model)
rf.predict <- predict(rf.model, newdata = data.test)
table(rf.predict, data.test[,'class'], dnn = c('Truth','Predict'))
1-mean(rf.predict!=data.test$class)
varImpPlot(rf.model)

#searching for optimal number of variables randomly sampled as candidates at each split
set.seed(1234)
tuneRF(x=data.train[,-length(data.train)], y=data.train[,'class'],
       mtry=10, ntree=500,
       stepFactor = 2, trace=TRUE, plot=TRUE)
########################################################################