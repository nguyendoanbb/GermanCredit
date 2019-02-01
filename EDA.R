########################################################################
#Split data to 2 groups: bad and good class
data_bad <- data[which(data$class == 'bad'),]
data_good <- data[which(data$class == 'good'),]
########################################################################

########################################################################
#Categorical features


#Majority of people who own a house has good credit, not many people who reside for free have credit card
ggplot(data, aes(x=class, fill = housing)) +
  geom_bar(position = 'stack') +
  theme(legend.position = 'right') +
  theme_bw()

#No checking appears to associate with good credit
ggplot(data, aes(x=class, fill = checking_status)) +
  geom_bar(position = 'stack') +
  theme(legend.position = 'right') +
  theme_bw() 

#All existing paid appears to associate with good credit
#however, critical account and other existing credit also appears to associate with 
#good credit instead of bad credit
ggplot(data, aes(x=class, fill = credit_history))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() 

#Majority with good credit use card for radio/tv, furniture/equipment, and new car
ggplot(data, aes(x=class, fill = purpose))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() 

#Saving <100 have more good credit. Probably b/c they depend on credit a lot. 
#It is easy to notice that people with saving < 100 use credit card the most. 
ggplot(data, aes(x=class, fill=savings_status))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() 

#it is unclear to infer anything from this, not a lot of unemployed use credit card (can't pay)
#or 4<=X<7 use credit card much
ggplot(data, aes(x=class, fill=employment))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() 

#mostly male single and female div/sep/mar use credit card 
ggplot(data, aes(x=class, fill=personal_status))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() 

#people with car and real estate maintain good credit
ggplot(data, aes(x=class, fill =property_magnitude))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() 

#skilled employees maintain good credit
ggplot(data, aes(x=class, fill=job))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() 

#a lot of foreing workers use credit card
ggplot(data, aes(x=class, fill =foreign_worker))+
  geom_bar(position = 'stack')+
  theme(legend.position = 'right') +
  theme_bw() 
########################################################################

########################################################################
#continuous features

#Good credit has a lower average duration
ggplot(data, aes(x=class, y=duration, fill=class))+
  geom_boxplot() +
  theme_bw()

#class good has lower credit_amount vs class bad
#in addition, large portion of people with good credit use less money than people with bad credit do
ggplot(data, aes(x=credit_amount, fill=class)) + 
  geom_density(alpha=0.5) + 
  geom_vline(xintercept = c(mean(data_good$credit_amount), mean(data_bad$credit_amount)), 
             colour = c('light blue','red'), linetype = 'dashed')

#average age with good credit is higher than bad credit
#maybe older people want to maintain good credit
ggplot(data, aes(x=class, y=age, fill=class))+
  geom_boxplot()+
  theme_bw() 
########################################################################
