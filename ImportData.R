'
Name: Nguyen Doan
Date: 2/1/2019
R Ver: 3.5.2 (2018-12-20) Eggshell Igloo
Git ver: 2.20.1
'

#Import Data
rm(list=ls()) #clear current R environment
setwd("~/Downloads/") #need to modify to directory that contains the dataset
data <- read.csv('Credit_Dataset.csv')
