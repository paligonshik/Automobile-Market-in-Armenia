dec_predict<-function(data){
  load("decision.rda")
  data$model<-paste(data$brand,data$model)
  prob=predict(tree,data)[2]
  class=ifelse(prob>0.35,"sold","unsold")
  print(paste("Probability of the car to be purchased is ",prob,"so it is predicted as",class))
 }

