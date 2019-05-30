#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(knitr)
library(slickR)
library(htmlwidgets)
library(htmltools)

data<-read.csv("final.csv")
Data<-read.csv("regression.csv")
Data1<-read.csv("tree.csv")
load("my_model1.rda")
load("decision.rda")


reg_predict<-function(Data){
  Data$model <- factor(paste(Data$brand,Data$model))
  load("my_model1.rda")
  price<-exp(predict(model,Data, interval="confidence"))
  price
}  

x<-function(y){
  qqnorm(y)
  qqline(y, col="red")
}

dec_predict<-function(data){
  load("decision.rda")
  data$model<-paste(data$brand,data$model)
  prob=predict(tree,data)[2]
  prob
  #class=ifelse(prob>0.35,"sold","unsold")
  #print(paste("Probability of the car to be purchased is ",prob,"so it is predicted as",class))
}


shinyServer(function(input, output) {
#=============================================================
#=============== Functions fot the menuitem 1 ================
#=============================================================

# Slides
  output$slickr <- renderSlickR({ imgs <- list.files(path = "slides" , pattern=".PNG", full.names = TRUE)
    slickR(imgs)})   
  
  # Download{
# just creating the reactive for datas 
dataserinput <- reactive({switch(input$dataset,"Data1"=Data,"Data2"=data)#switch
})# datasetinput 
#DOWNLOAD file
output$downloadData <- downloadHandler(filename = function(){paste(input$dataset, ".csv", sep = "")},#filename
    content = function(file){write.csv(dataserinput(), file, row.names = FALSE) 
}#content  
)#downloadHandler
# Download} 

#====================Link==============
output$link <- renderUI({
  tagList(h3(a("Click on me", href="https://www.auto.am/",target="_blank")))
})
#====================Link==============

output$shiny<- renderImage({  filename <- normalizePath(file.path('./images', paste('shiny', '.PNG', sep='')))
list(src = "shiny.PNG",   contentType = "image/PNG", alt = "shiny",width = 250,height = 89)},deleteFile = FALSE) 


#=============================================================
#=============== Functions fot the menuitem 2 ================
#=============================================================
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({ ggplot(data = data, aes_string(x = input$x, y = input$y,color = input$z))+geom_point()})
  # Create data table
  library(dplyr)
  output$datastable <- DT::renderDataTable({brushedPoints(data, brush = input$plot_brush)%>%
      select(c(brand, model, price, sold))}, options=list(order=list(list(4, 'desc'))))
  # Create data a correlation 
  output$correlation <- renderText({round(cor(data[, input$x], data[, input$y], use = "pairwise"), 3)})
  # Create boxplot object the plotOutput function is expecting
  output$boxplot <- renderPlot({
    ggplot(data = data, aes_string(x = input$boxx, y = input$boxy,color = input$boxz)) +
      geom_boxplot()+theme(axis.text.x =element_text(angle =60,hjust = 1))})
  # Create data table
  output$datastable2 <- DT::renderDataTable({brushedPoints(data, brush = input$plot_brush2)%>%
      select(brand, model, price,sold)}, options=list(order=list(list(4, 'desc'))))
  # Create histogram object the plotOutput function is expecting
  output$histogram <- renderPlot({ggplot(data = data, aes_string(x = input$yhist)) +geom_histogram(bins = input$bins,alpha=0.3, color="black",fill="forestgreen")+theme(axis.text.x =element_text(angle =60,hjust = 1))+labs(title=toupper(paste("Histogram of ",input$yhist)),
          x=toupper(paste(input$yhist)),y="COUNT")})
#=============================================================
#=============== Functions fot the menuitem 3 ================
#=============================================================
#Corplots{
output$corplot1 <- renderPlot({
    only_num<-dplyr::select_if(Data, is.numeric)
    if (input$name == "Before") {ggcorrplot(cor(only_num), hc.order = TRUE, type = "lower",lab = TRUE)##first plot. 
    }
  else if(input$name == "After") {only_num$Engine_Volume<-NULL
    ggcorrplot(cor(only_num), hc.order = TRUE, type = "lower", lab = TRUE)##second plot.
    }
  else {"CHOSE SOMETHING"}})
#Corplots}  
#New observation{
  output$brand <- renderUI({selectInput(inputId = "brand",label = "Brand",choices = as.character(unique(Data$brand)),selected = "BMW")})
  output$model <- renderUI({available <- Data[Data$brand == input$brand, "model"]
  selectInput(inputId = "model",label = "Model",choices = unique(available),selected = unique(available)[1])})
  output$Hand_Drive <- renderUI({selectInput(inputId = "Hand_Drive",label = "Hand Driver",choices = as.character(unique(Data$Hand_Drive)),selected = "Left")})
#New observation}
#Making table reactive
  table1 <- reactive({data.frame(brand=input$brand,model=input$model, year = input$year, Horsepower = input$Horsepower, Hand_Drive = input$Hand_Drive, Engine_Cylinders= input$Engine_Cylinders, Engine = input$Engine, Interior_Color=input$Interior_Color,Mileage=input$Mileage,	Gearbox=input$Gearbox,	Drive_train=input$Drive_train)})
#Table print{
output$table <- renderPrint({kable(table1())})
#Table print}  
#Regression Formula{
  output$RegressionFormula<- renderImage({  filename <- normalizePath(file.path('./images', paste('RegressionFormula', '.PNG', sep='')))
  list(src = "RegressionFormula.PNG",   contentType = "image/PNG", alt = "RegressionFormula",width = 1200,height = 150)},deleteFile = FALSE) 
#Regression Formula}  
#regression{  
  output$lower <- renderText({round(reg_predict(table1())[2])})
  output$regression <- renderText({round(reg_predict(table1())[1])})
  output$upper <- renderText({round(reg_predict(table1())[3])})
#regression} 
#The output of plots from regression{
  output$plot1 <- renderPlot(ggplot()+geom_point(aes(model$fitted.values,model$residuals))+geom_abline(intercept = 0,slope = 0,size=2,color='blue')+labs(title="Residuals vs Fitted Values",y="residuals",x='Fitted Values')) 
 output$plot2 <- renderPlot(x(model$residuals))
 #The output of plots from regression}
  
#=============================================================
#=============== Functions fot the menuitem 4 ================
#=============================================================
 #pictures
 # Slides
# output$slickr2 <- renderSlickR({ imgs2 <- list.files(path = "slides2", pattern=".PNG", full.names = TRUE)
# slickR(imgs2)})   
 
 output$boxp11 <- renderImage({
     if (input$name1 == "Bar plot 1") {
     
     filename <- normalizePath(file.path('./images', paste('barplot1', '.PNG', sep='')))
     list(src = "barplot1.PNG", contentType = "image/PNG", alt = "timeline",width = 1250, height = 750)
   }
   else if(input$name1 == "Bar plot 2") {
   
   filename <- normalizePath(file.path('./images', paste('barr', '.PNG', sep='')))
   list(src = "barr.png", contentType = "image/PNG", alt = "timeline",width = 850, height = 550)
   }else if(input$name1 == "Table"){
     filename <- normalizePath(file.path('./images', paste('table', '.PNG', sep='')))
     list(src = "table.png", contentType = "image/PNG", alt = "timeline",width = 450, height = 750)
       }
   else {"CHOSE SOMETHING"}},deleteFile = FALSE)
 
 
 # output$boxp1 <- renderImage({  filename <- normalizePath(file.path('./images', paste('barplot1', '.PNG', sep='')))
 # list(src = "barplot1.PNG", contentType = "image/PNG", alt = "timeline",width = 1150, height = 600)},deleteFile = FALSE)
 #output$boxp2 <- renderImage({  filename <- normalizePath(file.path('./images', paste('barplot2', '.PNG', sep='')))
 #list(src = "barplot2.PNG", contentType = "image/PNG", alt = "timeline",width = 350, height = 300)},deleteFile = FALSE)
 
 output$wilcox <- renderImage({  filename <- normalizePath(file.path('./images', paste('Wilcoxon', '.PNG', sep='')))
 list(src = "Wilcoxon.PNG", contentType = "image/PNG", alt = "timeline",width =500, height = 90)},deleteFile = FALSE)
 
 output$roc1 <- renderImage({  filename <- normalizePath(file.path('./images', paste('rocl', '.PNG', sep='')))
 list(src = "rocl.png", contentType = "image/PNG", alt = "timeline",width = 450, height = 400)},deleteFile = FALSE)
 output$roc2 <- renderImage({  filename <- normalizePath(file.path('./images', paste('rocd', '.PNG', sep='')))
 list(src = "rocd.png", contentType = "image/PNG", alt = "timeline",width = 450, height = 400)},deleteFile = FALSE)
 output$logitconfus <- renderImage({  filename <- normalizePath(file.path('./images', paste('logitconfus', '.PNG', sep='')))
 list(src = "logitconfus.png", contentType = "image/PNG", alt = "timeline",width = 350, height = 400)},deleteFile = FALSE)
 #
 output$confusion <- renderImage({  filename <- normalizePath(file.path('./images', paste('confusion', '.PNG', sep='')))
 list(src = "confusion.PNG", contentType = "image/PNG", alt = "timeline",width = 350, height = 400)},deleteFile = FALSE)
##New observation{
 output$b1 <- renderUI({selectInput(inputId = "brand1",label = "Brand:", choices = as.character(unique(Data1$brand)),selected = "BMW")})
 output$model1 <- renderUI({available1 <- Data1[Data1$brand == input$brand1, "model"]
 selectInput(inputId = "model1",label = "model:", choices = unique(available1), selected = unique(available1)[1])})
# making table reactive()
 table2 <- reactive({data.frame(brand=input$brand1,model=input$model1, price=input$Price, year = input$year1, Color=input$Color1, Hand_Drive = input$Hand_Drive1, Engine = input$Engine1, Interior_Color=input$Interior_Color1, Mileage=input$Mileage1,	Gearbox=input$Gearbox1,	Drive_train=input$Drive_train1)})
#Table{
 output$tabletwo <- renderPrint({kable(table2())}) # Because of reactivity()
#Table}  
#Prediction{
 output$cutoffoutput <- renderText({input$cutoff})
 output$predictionofdec<-renderText({round(dec_predict(table2()),3)})
 output$soldunsold<-renderText({print(paste(ifelse( as.numeric(dec_predict(table2()))> input$cutoff,"sold","unsold"))) })

#}Prediction
 
 
 
 #=============================================================
 #=============== Functions fot the menuitem 5 ================
 #============================================================= 
  
})
