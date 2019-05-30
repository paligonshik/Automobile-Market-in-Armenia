library(dplyr)
library(ggplot2)
data<-read.csv("final2.csv")

plot<-data[data$brand %in% names(which(table(data$brand) > 100)), ]

ggplot(plot, aes(fill=sold, x=brand)) + 
  geom_bar(position="dodge")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+ggtitle("Distribution of Sold Cars by Brands")+xlab("Brands")+
  ylab("Number of cars")+
  theme(plot.title = element_text(hjust = 0.5))



ggplot(data,aes(x=sold)) + geom_bar(position="dodge",stat =)#
  +theme(axis.text.x = element_text(angle = 45,hjust = 1))+ggtitle("Distribution of Sold Cars by Brands")+xlab("Brands")+
  ylab("Number of cars")
pp <- ggplot(data=data, aes(x=sold)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)))
pp
  
  library(scales)
pp + scale_y_continuous(labels = percent)
ggplot(data=data, aes(x=sold)) + 
  geom_bar(aes(y = ..prop.., group = 1))
counts <- table(data$sold)
barplot(counts / sum(counts), main="Car Distribution", xlab="Unsold VS  Sold") 


