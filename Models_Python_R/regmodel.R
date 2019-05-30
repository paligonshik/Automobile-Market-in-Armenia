reg_predict<-function(Data){
  Data$model <- factor(paste(Data$brand,Data$model))
  load("my_model1.rda")
  price<-exp(predict(model,Data))
  print(paste("Estimatet price is ",price))
}

data1<-read.csv("regression.csv")

data1$price<-NULL

z<-data1[1]

reg_predict(z)
library(reticulate)
repl_python()

exit
