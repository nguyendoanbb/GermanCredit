##Classification Portfolio##
rm(list=ls()) #clear current R environment
setwd('~/Documents')
data <- read.csv('dataset_31_credit-g.csv')

#Objectives: classify a customer's credit risk based on other features

################Data transformation
head(data) #20 features and 1000 observations, some variables need editing in order to be useful for analysis
str(data) 
summary(data)
table(data$class, data$credit_history)

#removing single quote from all values in the table
library(dplyr) #dataframe manipulation
library(base) #replacement of element in string
for (i in c(1,3:4,6:7,9:10,12,14:15,17,19:20)){
  data[,i] <- gsub("'",'',data[,i])
}
data$personal_status <- ifelse(data$personal_status == 'female div/dep/mar', 'female div/sep/mar', data$personal_status)
data <- data %>% mutate_if(is.character, as.factor) #converge character to factor
str(data) #checking if convergence works
summary(data) #we can now summarize the data

#############Exploratory data analysis
data_bad <- data[which(data$class == 'bad'),]
data_good <- data[which(data$class == 'good'),]

####categorical features
library(ggplot2)
ggplot(data, aes(x=class, fill = housing)) +
  geom_bar(position = 'stack') +
  theme(legend.position = 'right') +
  theme_bw() #majority of people who own a house has good credit,
#not many people who reside for free have credit card
ggplot(data, aes(x=class, fill = checking_status)) +
  geom_bar(position = 'stack') +
  theme(legend.position = 'right') +
  theme_bw() #no checking appears to associate with good credit
ggplot(data, aes(x=class, fill = credit_history))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() #all existing paid appears to associate with good credit
#however, critical account and other existing credit also appears to associate with 
#good credit instead of bad credit
ggplot(data, aes(x=class, fill = purpose))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() #majority with good credit use card for radio/tv, furniture/equipment, and new car
ggplot(data, aes(x=class, fill=savings_status))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() #saving <100 have more good credit. Probably b/c they depend on credit a lot. 
#It is easy to notice that people with saving < 100 use credit card the most. 
ggplot(data, aes(x=class, fill=employment))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() #it is unclear to infer anything from this, not a lot of unemployed use credit card (can't pay)
#or 4<=X<7 use credit card much
ggplot(data, aes(x=class, fill=personal_status))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() #mostly male single and female div/sep/mar use credit card 
ggplot(data, aes(x=class, fill=other_parties))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() #not much to infer from this
ggplot(data, aes(x=class, fill =property_magnitude))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() #people with car and real estate maintain good credit
ggplot(data, aes(x=class, fill=other_payment_plans))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() #not much to infer
ggplot(data, aes(x=class, fill=job))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() #skilled employees maintain good credit
ggplot(data, aes(x=class, fill=own_telephone))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() #not much to infer
ggplot(data, aes(x=class, fill =foreign_worker))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() #a lot of foreing workers use credit card

###continuous features
ggplot(data, aes(x=class, y=duration, fill=class))+
  geom_boxplot() +
  theme_bw()
ggplot(data, aes(x=credit_amount, fill=class)) + 
  geom_density(alpha=0.5) + 
  geom_vline(xintercept = c(mean(data_good$credit_amount), mean(data_bad$credit_amount)), 
             colour = c('light blue','red'), linetype = 'dashed') #class good has lower credit_amount vs class bad
#in addition, large portion of people with good credit use less money than people with bad credit do
ggplot(data, aes(x=class, y=age, fill=class))+
  geom_boxplot()+
  theme_bw() #average age with good credit is higher than bad credit 

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
library(faraway)
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
install.packages('ROCR')
library(ROCR)
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
install.packages('pROC')
library(pROC)
roc_log <- roc(data.test$class, as.numeric(log.model.class))
auc(roc_log) #log auc is 0.7979
roc_svm <- roc(data.test$class, as.numeric(svm.tune.predict))
auc(roc_svm) #svm auc is 0.788


###Random forest
library(randomForest)
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


