############Predictive modeling
###Data set-up for training and testing
set.seed(123)
train <- sample(1:nrow(data), nrow(data)*2/3)
data.train <- data[train,]
data.test <- data[-train,]
########Logistic regression
##logistic model
log1 <- glm(class~., family = 'binomial', data=data.train)
summary(log1)
#performing stepwise selection 
log2 <- step(log1)
summary(log2)
pchisq(deviance(log2), df.residual(log2), lower.tail = F) #checking model validity
#model diagnostics: check residuals
plot(residuals(log2)) #the variance shows no unusual pattern
plot(residuals(log2)~fitted.values(log2))
halfnorm(rstudent(log2)) #no outliers detected
inf.point <- influence(log2)
halfnorm(inf.point$hat) #there is some indication that observation 319 may have some leverage

log2.predict <- predict(log2, newdata = data.test, type = 'response')
log2.class <- log2.predict > 0.5 #select 50% scoring should be a reasonable level
log2.class <- ifelse(log2.class == TRUE, 'good', 'bad')
table(data.test$class,log2.class, dnn = c('Truth','Predicted'))
1-mean(log2.class!=data.test$class) #misclassification rate
#We predict incorrectly about 25.7% of the time, which is quite low. But the model can still be improved.
#Accuracy is 74.25%

#attempt cross validation to improve accuracy
library(caret)
log_train <- trainControl(method = 'cv', number=10)
log.model <- train(class~checking_status + duration + credit_history + purpose + 
                     credit_amount + savings_status + employment + installment_commitment + 
                     personal_status + other_parties + age + other_payment_plans + 
                     foreign_worker, data=data.train, trControl = log_train, method='glm', family=binomial())
log.model$results #with cross validation, we improve accuracy to 76.13% with training set
summary(log.model)
pchisq(595.32,629,lower.tail = F) #legit model 
log.model.pred <- predict(log.model, data.test, type = 'prob')
log.model.class <- ifelse(log.model.pred$bad > log.model.pred$good, 'bad', 'good')
table(data.test$class, log.model.class, dnn = c('Truth', 'Predicted') )
1-mean(log.model.class!=data.test$class)
#accuracy in testing set is improved to 76.95%

#ROC Curve for Logistic Regression
rocplot(pred = log2.predict, data.test$class, main = 'Logistic Testing Data')
abline(0,1, lty=2)


#ROC curve for SVM
rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", 'fpr')
  plot(perf,...)
}
svm.predict <- attributes(svm.predict)$decision.values
par(mfrow=c(1,2))
rocplot(svm.tune.predict, data.test$class, col = 2, main = 'SVM Testing Data')
abline(0,1, lty=2)

##Comparing Logistic and SVM

roc_log <- roc(data.test$class, as.numeric(log.model.class))
auc(roc_log) #log auc is 0.7979
roc_svm <- roc(data.test$class, as.numeric(svm.tune.predict))
auc(roc_svm) #svm auc is 0.788




