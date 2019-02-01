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
