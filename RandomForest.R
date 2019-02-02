########################################################################
#Random forest

#optimize number of variables randomly sampled as candidates at each split (mtry parameter)
set.seed(1234)
rf.tune <- tuneRF(x=data.train[,-length(data.train)], y=data.train[,'class'],
                  stepFactor = 2, trace=TRUE, plot=TRUE, doBest = TRUE)




rf.model <- randomForest(class~., data=data.train, importance = TRUE,
                         mtry = rf.tune$mtry)
plot(rf.model)
legend('right', colnames(rf.model$err.rate), col=1:3, fill=1:3)

rf.predict <- predict(rf.model, newdata = data.test)
rf.conf.matrix <- table(rf.predict, data.test[,'class'], dnn = c('Truth','Predict'))
rf.accuracy <- round((1-mean(rf.predict!=data.test$class))*100,2)
rf.bad.good <- rf.conf.matrix[3]




importance <- importance(rf.model)

varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

#ranking importance of features
ggplot(rankImportance, aes(x = reorder(Variables, Importance) , 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55,size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() +
  theme_bw()




#Conclusion
cat('The accuracy by using Random Forest model is: ', rf.accuracy, '%',
    sep = '')
cat('We predict good risk when it is actually bad risk for',rf.bad.good,'observations')
########################################################################