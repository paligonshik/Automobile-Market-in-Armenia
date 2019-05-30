data<-read.csv("final2.csv")
df<-data[ apply(data, 2, function(x) !any(is.na(x)))]
df$date<-NULL
df$id_val<-NULL
head(df)

library(glmnet)
library(glmnetUtils)
library(caret)
library(dplyr)
library(reticulate)
library(ROCR)
library(pROC)
train_index<-createDataPartition(df$sold,p = 0.8,list = F)
train<-df[train_index,]
test<-df[-train_index,]
lambda=seq(from=0.02,to = 1,by=0.05)
library(glmnet)
set.seed(42)
myModel <- cv.glmnet(model.matrix(sold~.,data = train),train$sold,family = "binomial",nfolds = 5,alpha = 1,
                     lambda=lambda)

myModel$cvm

z<-predict(myModel,newx  = model.matrix(sold~.,data = test),type="response")
min(z)
z1<-factor(ifelse(z>0.35,"Yes","No"))
aa<-ifelse(test$sold==
             "Yes",1,0)
rrr<-roc(test$sold,z)
rrr
ggroc(rrr,alpha = 0.5, colour = "green", linetype = 1, size = 1)+ggtitle("ROC curve for Model AIC")
auc(rrr)
coords(rrr, "best")
confusionMatrix(z1,test$sold,positive = "Yes")

p_test<-prediction(z,test$sold)
perf <- performance(p_test,"tpr","fpr")
plot(perf,colorize=TRUE)
FPR<-unlist(perf@x.values)
TPR<-unlist(perf@y.values)
alpha<-unlist(perf@alpha.values)
df1<-data.frame(FPR,TPR,alpha)
ggplot(df1,aes(FPR,TPR,color=alpha))+geom_line(size=2)+geom_abline(slope = 1)+
  ggtitle("ROC CURVE")+theme(plot.title = element_text(hjust = 0.5))+annotate("text", x = 0.8, y = 0.1, label = "AUC is 0.5901")
