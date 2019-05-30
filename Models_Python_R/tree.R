#rm(list = ls())
#install.packages("rpart.plot")
library(caret)
library(ggplot2)
library(ISLR)
library(pROC)
library(rpart.plot)
library(ROCR)
library(pROC)
library(car)
data<-read.csv("final2.csv")
table(data$Drive_train)
df<-data[ apply(data, 2, function(x) !any(is.na(x)))]
df$date<-NULL
df$id_val<-NULL
colnames(df)
data$Drive_train<-recode(data$Drive_train, "c('NaN', '')='Not Mentionded'")
data$Interior_Color<-recode(data$Interior_Color, "c('NaN', '')='Not Mentionded'")
head(df)

write.csv("final2.csv",x = data)


levels(data$Drive_train1) <-levels(data$Drive_train)
data$Drive_train[1:5]

table(data$Drive_train)

data<-data[ , colSums(is.na(data)) == 0]
levels(data$Color)
#######################################################################################
data$model<-paste(data$brand,data$model)
data<-data[data$model %in% names(which(table(data$model) > 35)), ]
#data$brand<-NULL
data$id_val<-NULL
#data$Interior_Color<-NULL
#data$Drive_train<-NULL
df$Body_Style<-NULL
data$date<-NULL
colnames(df)
write.csv("tree.csv",x = df)

set.seed(41)
train_index<-createDataPartition(data$sold,p = 0.8,list = F)
train<-data[train_index,]
test<-data[-train_index,]

table(data$model)



set.seed(42)
tree <- rpart(sold ~., data = train, method = "class", control = rpart.control(cp = 0, maxdepth = 30,minsplit = 100))
predictions <- predict(tree, test, type="prob")
roc_obj <- roc(test$sold, predictions[,2])
ggroc(roc_obj,alpha = 0.5, colour = "green", linetype = 1, size = 1)+ggtitle("ROC curve for Model AIC")
auc(roc_obj)
coords(roc_obj, "best")
pred<-factor(ifelse(predictions[,2]>0.35,"Yes","No"))
confusionMatrix(pred,test$sold,positive = "Yes")
library(ROCR)
p_test<-prediction(predictions[,2],test$sold)
perf <- performance(p_test,"tpr","fpr")
plot(perf,colorize=TRUE)
FPR<-unlist(perf@x.values)
TPR<-unlist(perf@y.values)
alpha<-unlist(perf@alpha.values)
df<-data.frame(FPR,TPR,alpha)
ggplot(df,aes(FPR,TPR,color=alpha))+geom_line(size=2)+geom_abline(slope = 1)+
  ggtitle("ROC CURVE")+theme(plot.title = element_text(hjust = 0.5))+annotate("text", x = 0.8, y = 0.1, label = "AUC is 0.65")
library(pROC)
roc_obj <- roc(test$sold, predictions[,2])
ggroc(roc_obj,alpha = 0.5, colour = "green", linetype = 1, size = 1)+ggtitle("ROC curve for Model AIC")

auc(roc_obj)

save(tree, file = "decision.rda")

load("decision.rda")




