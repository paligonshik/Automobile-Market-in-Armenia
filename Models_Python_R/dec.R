#rm(list = ls())
#install.packages("rpart.plot")
library(caret)
library(ggplot2)
library(ISLR)
library(pROC)
library(rpart.plot)
library(ROCR)
library(pROC)

data<-read.csv("final2.csv")
data<-data[ , colSums(is.na(data)) == 0]


#######################################################################################

data$model<-paste(data$brand,data$model)
#data<-data[data$model %in% names(which(table(data$model) > 35)), ]
data$brand<-NULL
data$id_val<-NULL
data$date<-NULL
set.seed(41)
train_index<-createDataPartition(data$sold,p = 0.8,list = F)
train<-data[train_index,]
test<-data[-train_index,]
table(data$model)

