data<-read.csv("final2.csv")
sapply(data, function(x) sum(is.na(x)))
factors<-c("brand","Color","Hand_Drive","Gear","Drive_train","sold")
#table(data$model)
#colnames(data)
library(ggplot2)
library(plotly)
only_num<-dplyr::select_if(data, is.numeric)
colnames(only_num)
dim(data[data$Mileage>400000,])
summary(data$Mileage)
colnames(only_num)
#scatterplots1
ggplot(data,aes(price,Horsepower,color=Gearbox))+geom_point(alpha=0.1)+
  labs(title = "Relationship between Engine volume and Horsepower",y="Engine Volume")+theme(plot.title = element_text(hjust = 0.5))
#scatterplots2
summary(data$price)
ggplot(data,aes(year,log(Mileage)))+geom_point(alpha=0.1)+
  ggtitle("Relationship between Engine volume and Horsepower")


ggplot(data,aes(x=brand,y=sold,color=))+geom_histogram(stat = "identity")+
  theme(axis.text.x =element_text(angle =70,hjust = 1))+
  labs(title="Distribution of the cars by Brand",x="Brand",y="Count")
ggplot(data,aes(x=sold))+geom_histogram(stat = "count")+facet_grid(.~brand)+
  theme(axis.text.x =element_text(angle =70,hjust = 1))+
  labs(title="Distribution of the cars by Brand",x="Brand",y="Count")


library(dplyr)
only_num<-dplyr::select_if(data, is.numeric)
only_num<-na.omit(only_num)
library(ggcorrplot)
ggcorrplot(cor(only_num), hc.order = TRUE, type = "lower",
           lab = TRUE)+ggtitle("Correlation plot")+theme(plot.title = element_text(hjust = 0.5))
str(data)
options(scipen =999)
ggplot(df,aes(sold,y=Mileage))+geom_bar(stat = 'identity')+
  labs(y='Number of observations',title = 'Barplot representing number of low or normal birth weigths')



##################################
df<-na.omit(data)
str(df)
df$date<-NULL
df$id_val<-NULL
df$sold<-NULL
only_num<-dplyr::select_if(df, is.numeric)
ggcorrplot(cor(only_num), hc.order = TRUE, type = "lower",
           lab = TRUE)+labs(title="Distribution of the cars by Brand",x="Brand",y="Count")

model<-lm(log(price)~model+brand+year+Color+Body_Style+Horsepower+Hand_Drive+Engine+log(Mileage)+Gearbox+Drive_train,data = df)
summary(model)

g1 = ggplot(data = df, aes(y = price, x = Gearbox,color = Gearbox))+
  geom_boxplot()+
  scale_x_discrete(labels= levels(df$Gearbox) )+
  stat_summary(fun.y = "mean",geom = 'point',col='red', shape=17, size=4)+
  labs(title ="Price by Gearbox " ,color =  "Gearbox",y='Price',x="Gearbox of a Car")+
  theme(plot.title = element_text(hjust = 0.5))
g2 = ggplot(data = df, aes(y = price, x = job ,color = job ))+
  geom_boxplot()+
  scale_x_discrete(labels= levels(Data$job) )+
  stat_summary(fun.y = "mean",geom = 'point',col='red', shape=17, size=4)+
  labs(title ="GPA by Job status" ,color =  "job statues",y='GPA',x="Job status")+
  theme(plot.title = element_text(hjust = 0.5))
g3 = ggplot(data = Data, aes(y = gpa, x = type,color = type))+
  geom_boxplot()+
  stat_summary(fun.y = "mean",geom = 'point',col='red', shape=18, size=4)+
  labs(title ="GPA by Study tyoe " ,y='GPA',x="Type")+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(g1, g2, g3,nrow=1)







s