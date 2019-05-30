# You need to create account in Shinyapps.io
# rsconnect::setAccountInfo(name='zilfi',
#token='EA9C6D6730242CE4B7DFBC3706A7FE43',
#secret='<SECRET>')

library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(slickR)
library(shinyjs)

factors<-c("brand","Color","Hand_Drive","Gearbox","Drive_train","sold")
Data<-read.csv("regression.csv")
Data1<-read.csv("tree.csv")
# Define UI for dashbord of project
shinyUI(
  dashboardPage( title = "Vazgen Tadevosyan Master Thesis 2019 AUA",
     
    # dashboardPage( dashboardHeader(), dashboardSidebar(), dashboardBody())
     dashboardHeader( title = "Master Thesis 2019"), 
     #left panel
     dashboardSidebar(
        sidebarMenu( style = "position: fixed; overflow: visible;",
          
             menuItem(text = "Preliminary steps", tabName = "firsttab", icon = icon("table"), badgeLabel = "Link", badgeColor = "blue" ), # menuItem #1
             menuItem(text = "Describing the data", tabName = "secondtab", icon = icon("bar-chart-o")), # menuItem #2
             menuItem(text = "Regression", tabName = "thirdtab", icon = icon("box-open"), badgeLabel = "DM", badgeColor = "blue"), # menuItem #3
             menuItem(text = "Classification", tabName = "fourthtab", icon = icon("code-fork"),badgeLabel = "DM", badgeColor = "blue"), # menuItem #4
             menuItem(text = "Summary", tabName = "fifthtab", icon = icon("check")) # menuItem #5
        )#sidebarMenu
     ), # dashboardSidebar 
     
    
    dashboardBody(
       tabItems(
#=============================================================         
                   # Page from the menuitem 1
#=============================================================
         
         tabItem(tabName = "firsttab", 
           
           fluidRow(# fluidRow 1
             
          valueBox(value = "Automobile market in Armenia", subtitle = em(h4("Vazgen Tadevosian", br(), "Dr. Habet Madoyan")), icon = icon("car"), color = "blue", width = 12),
               
          # ==================slides====================   
           box( slickROutput("slickr", width="1250px"), width = 12, height = 460, solidHeader = T),
          #==========================Adding download button}===================
          
              box(# box for download button 
           
               align="center", solidHeader = T, width = 4, height = 105,
           box(selectInput("dataset", "Please select the data", choices = c("Data1","Data2")), solidHeader = T, height = 90), # for small selects
           box(downloadButton("downloadData", h6("Download")),solidHeader = T, height = 90)  # for small selects      
               
             ),#box for download button
          #====================link=========================
         # box(uiOutput("link"), width = 4, height = 110, solidHeader = T,icon = icon("box-open")),
        valueBox(value = uiOutput("link"), subtitle = "URL link", icon = icon("link"), color = "blue", width = 4),
             box( imageOutput("shiny"),align="center", solidHeader = T, width = 4, height = 105)
                                     )#FrluidRow
             ), #tabItem #1
         

         
#=============================================================
                   # Page from the menuitem 2
#=============================================================
         
         tabItem(tabName = "secondtab",
           
           fluidRow( #fluidRow 1 from menuidem 2
             valueBox(value = "62664 records", subtitle = "The finalized form of the data contains", icon = icon("file-excel"), color = "blue"),
             valueBox(value = "18 variables", subtitle = "The finalized form of the data contains", icon = icon("file-contract"), color = "light-blue"),
             valueBox(value = "2000-2018", subtitle = "Car production year  interval", icon = icon("hourglass-half"), color = "blue")
           ), #fluidRow 1 from menuidem 2
           
#====
#Scatterplots
#====
           fluidRow( #fluidRow 2 from menuidem 2
             box( #fluidRow 2 box 1
               title = "Scatertplots for describing the data", solidHeader = T, 
               status = "primary", height = 700,
               # insert boxes to set them into one line 
               box(selectInput(inputId = "y", label = "Y-axis", choices = c("Price" = "price", "Year" = "year","Horsepower" = "Horsepower", "Mileage", "Engine Cylinders" = "Engine_Cylinders", "Engine Volume" = "Engine_Volume"), selected = "Price", width = 200), width = 4,solidHeader = T),
               box(selectInput(inputId = "x",label = "X-axis", choices = c("Price"  = "price", "Year"  = "year", "Horsepower"  = "Horsepower", "Mileage", "Engine Cylinders"="Engine_Cylinders", "Engine Volume"  ="Engine_Volume"), selected = "Horsepower", width = 200),width = 4,solidHeader = T),
             box(selectInput(inputId = "z", label = "Grouped by", choices = factors, selected = "Gearbox", width = 200),width = 4,solidHeader = T),
               
               # Show thr correlation
               valueBox( subtitle = "Correlation between the variables", value = textOutput(outputId = "correlation"), color = "light-blue", width = 12, icon = icon("arrows-alt-h")),
               
               # Show scatterplot with brushing capability
               plotOutput(outputId = "scatterplot", brush = "plot_brush")
                           ),#fluidRow 2 box 1
             
             box(#fluidRow 2 box 2
               title = "Data from the plot",
               status = "primary", solidHeader = T,
               height = 700,  
               DT::dataTableOutput(outputId = "datastable") 
               )#fluidRow 2 box 2

            
           ), #fluidRow 2 from menuidem 2
          
#===
# Value Boxes           
#===           
            fluidRow(#fluidRow 3 from menuidem 2
             valueBox(value = "Automatic gearbox", subtitle = "Majority", icon = icon("highlighter"), color = "light-blue"),
             valueBox(value = "Mechanical gearbox", subtitle = "Cheaper, less horsepower", icon = icon("highlighter"), color = "blue"),
             valueBox(value = "Hand drive -> the price", subtitle = "Left hand drive cars are more expensive ", icon = icon("highlighter"), color = "light-blue")
             
             ), #fluidRow 3 from menuidem 2
 #====
 # Boxplots          
 #====                    
           fluidRow( #fluidRow 4 from menuidem 2
             box( #fluidRow 4 box 1
               title = "Boxplots for describing the data", solidHeader = T, 
               status = "primary", height = 700,
               # insert boxes to set them into one line 
               box(selectInput(inputId = "boxy", label = "Y-axis", choices = c("Price" = "price", "Year" = "year","Horsepower" = "Horsepower", "Mileage", "Engine Cylinders" = "Engine_Cylinders", "Engine Volume" = "Engine_Volume"), selected = "Price", width = 200), width = 4,solidHeader = T),
               box(selectInput(inputId = "boxx",label = "X-axis", choices = factors, selected = "Hand_Drive", width = 200),width = 4,solidHeader = T),
               box(selectInput(inputId = "boxz", label = "Grouped by", choices = factors, selected = "Hand_Drive", width = 200),width = 4,solidHeader = T),
               
             # Show scatterplot with brushing capability
               plotOutput(outputId = "boxplot", brush = "plot_brush2")
               
               
             ),#fluidRow 4 box 1
              
              box(#fluidRow 4 box 2
                title = "Data from the plot",
                status = "primary", solidHeader = T,
                height = 700,  
                DT::dataTableOutput(outputId = "datastable2") 
              )#fluidRow 4 box 2
              
             
           ), #fluidRow 4 from menuidem 2
           
#====
# Histogram          
#====            
           fluidRow(#fluidRow 5 from menuidem 2
             # y from fluidRow 2
             
             box( #fluidRow 5 box 1
               title = "Histograms for describing the data", solidHeader = T,
               status = "primary", height = 570,
               box(solidHeader=T,selectInput(inputId = "yhist", label = "Y-axis", choices = c("Price" = "price", "Year" = "year","Horsepower" = "Horsepower", "Mileage", "Engine Cylinders" = "Engine_Cylinders", "Engine Volume" = "Engine_Volume"), selected = "Price", width = 200), width = 4,height = 92),
               box(solidHeader=T,sliderInput(inputId = "bins", 
                 label = "Choose the bins", 
                 min = 10, max = 70, 
                 value = 11), width = 8, height = 92),
               # Show boxplot with brushing capability
               plotOutput(outputId = "histogram", brush = "plot_brush")
             ),#fluidRow 5 box 1
             
             infoBox(title="The price", value = "Imbalanced, right-skewed", subtitle = " ",icon = icon("exclamation"), color = "teal", width = 6),
             infoBox(title="The price", value = "Not normally distributed", subtitle = " ",icon = icon("exclamation"), color = "light-blue", width = 6),
             infoBox(value="The median of the price are the same for two groups", title = "The Null Hypothesis", subtitle = "med(sold)=med(undols)",icon = icon("equals"), color = "teal", width = 6),
             infoBox(value="The median of the price are not the same for two groups", title = "The alternative hypothesis", subtitle = "med(sold)<>med(undols)",icon = icon("not-equal"), color = "light-blue", width = 6),
              box( solidHeader = T,status = "primary", title = "Wilcoxon test", imageOutput("wilcox"),align="center",  width = 6, height = 150 )
             
                         )#fluidRow 5 from menuidem 2
           
           ), #tabItem #2

         
                  
#=============================================================
                  # Page from the menuitem 3
#=============================================================
         
         tabItem(tabName = "thirdtab", 
          
           fluidRow(#fluidRow 0
             
              
             box(radioButtons("name", " ", choices = c("Before", "After"), "Before"), height = 520, width = 6,status = "info", solidHeader = T, align="center",
               plotOutput("corplot1")),#box1
             infoBox(title="Mileage - Year", value = "-0.52", subtitle = "Negative correlation",icon = icon("minus-square"), color = "teal", width = 6),
             infoBox(title="Horsepower - Engine Volume", value = "0.89", subtitle = "Positive correlation",icon = icon("plus-square"), color = "light-blue", width = 6), 
             infoBox(title="Engine Cylinders - Engine Volume", value = "0.83", subtitle = "Positive correlation",icon = icon("plus-square"), color = "blue", width = 6),
             
             infoBox(title="Engine Cylinders - Horsepower", value = "0.78", subtitle = "Positive correlation",icon = icon("plus-square"), color = "blue", width = 6),
             infoBox(title="Adjusted R squared", value = "0.92", subtitle = "",icon = icon("star"), color = "navy", width = 6)
             
             
             ), #fluidRow 0
           
           
           fluidRow( title =  "New Observation",
             box(#Box 1
              
               title = "Choose the own observation", width = 12,
               status = "primary", solidHeader = T,
        box(uiOutput("brand"),width = 2, height = 93),
        box(uiOutput("model"),width = 2, height = 93),
        box(uiOutput("Hand_Drive"), width = 2, height = 93),
        box(numericInput(inputId = "year",label = "Year",value = 2005),width = 2,solidHeader = T),
        box(numericInput(inputId = "Mileage",label = "Mileage",value = 70000),width = 2,solidHeader = T),
        box(numericInput(inputId = "Horsepower",label = "Horsepower",value = 150),width = 2,solidHeader = T),
        box(selectInput(inputId = "Gearbox", label = "Gearbox", choices = as.character(unique(Data$Gearbox)), selected = "Automatic"),width = 2,solidHeader = T),
        box(selectInput( inputId = "Interior_Color",  label = "Interior_Color", choices = as.character(unique(Data$Interior_Color)), selected = "black"),width = 2,solidHeader = T),
        box(selectInput( inputId = "Drive_train",  label = "Drive_train", choices = as.character(unique(Data$Drive_train)), selected = "Front"),width = 2,solidHeader = T),
        box(selectInput( inputId = "Engine",  label = "Engine", choices = as.character(unique(Data$Engine)), selected = "Petrol"),width = 2,solidHeader = T),
        box(numericInput( inputId = "Engine_Cylinders",  label = "Engine_Cylinders", value = 4), width = 2,solidHeader = T),#,
        box(verbatimTextOutput("table"), width = 12, solidHeader = T)       
           ) # Box 1
        

             ), # fluidRow #1
            fluidRow( title = "Regression Analysis",
             box(title = "Regression Analysis", status = "primary", solidHeader = T, width = 12,
             box( imageOutput("RegressionFormula"),align="center", solidHeader = T, width = 12, height = 200),
               
        valueBox( subtitle = "Lower interval", value = textOutput("lower"), color = "navy", width = 2, icon = icon("arrow-alt-circle-down")),
        valueBox( subtitle = "Estimated price of choosen car", value = textOutput("regression"), color = "blue", width = 8, icon = icon("dollar-sign")),
        valueBox( subtitle = "Upper interval", value = textOutput("upper"), color = "navy", width = 2, icon = icon("arrow-alt-circle-up"))
                              ), #box for formula and predicted values
              box(width = 12,
                box(plotOutput(outputId = "plot1"), width = 6, solidHeader = T), # box1
              box(plotOutput(outputId = "plot2"), width = 6, solidHeader = T) # box2
                )#box for qqplot and ggplot
              
             )#fluidRow #2
           
             
                      ), #tabItem #3
         
#=============================================================     
#                Page from the menuitem 4
#=============================================================

         tabItem( tabName = "fourthtab",
           fluidRow(title = "Logistic Analysis vs Decision trees",
             valueBox(value = "29% of the sample", subtitle = "Sold cars", icon = icon("percent"), color = "light-blue"),
             valueBox(value = "29 unique brands", subtitle = "_", icon = icon("bold"), color = "blue"),
             valueBox(value = "221 unique models", subtitle = "_", icon = icon("car-side"), color = "light-blue"),
         #    box( imageOutput("boxp1"),align="center", solidHeader = T, width =12, height = 650 ),
          #   box( imageOutput("boxp2"),align="center", solidHeader = T, width = 4, height = 320 ),
           #  box( slickROutput("slickr2", width="1500px"), width = 12, height = 700),
             
             box( h6(radioButtons("name1", " ", choices = c("Bar plot 1", "Bar plot 2", "Table"), "Bar plot 1"),inline=T, width=6), width = 12, status = "info",
               solidHeader = T, align="center",
               imageOutput("boxp11"), height = 900 ),#box1
             
               valueBox(value = "Top 10 popular cars", subtitle = "Hyundai Sonata, Nissan Teana, Nissan X-Trail, Opel Zafira, Hyundai Elantra",
                 icon = icon("fire"), color = "blue", width = 6),
             valueBox(value = "Top 10 popular cars", subtitle = "Toyota Camry, Mercedes-Benz E 320, Opel Astra, Nissan Tiida, Mercedes-Benz E 350", icon = icon("fire"), color = "blue", width = 6),
             
             
             #tabBox(title = "Logit",
             tabBox(title = "Logistic Regression", #solidHeader=T, status ="primary",
               tabPanel(title = "Logit ROC",imageOutput("roc1"),align="center", solidHeader = T, width = 6 ),
               tabPanel(title = "Logit confusion", imageOutput("logitconfus"),align="center", solidHeader = T, width = 6 )
                         ),
             #),#tabBox 1 from fluid Row 1
             tabBox(title = "DT",
               tabPanel(title = "DT ROC",imageOutput("roc2"),align="center", solidHeader = T, width = 6 ),
               tabPanel(title = "DT confusion", imageOutput("confusion"),align="center", solidHeader = T, width = 6 )
               
               )#tabBox 2 from fluid Row 1
           ),#fluidRow #1
             
           fluidRow( title ="New overvation",
             box(uiOutput("b1"),width = 2, height = 93),
            box(uiOutput("model1"),width = 2, height = 93),
            box(numericInput(inputId = "year1",label = "Year",value = 2005),width = 2,solidHeader = T),
            box(numericInput(inputId = "Mileage1",label = "Mileage",value = 70000),width = 2,solidHeader = T),
            box(selectInput(inputId = "Gearbox1", label = "Gearbox", choices = as.character(unique(Data1$Gearbox)), selected = "Automatic"),width = 2,solidHeader = T),
            box(selectInput( inputId = "Interior_Color1",  label = "Interior_Color", choices = as.character(unique(Data1$Interior_Color)), selected = "black"),width = 2,solidHeader = T),
            box(selectInput( inputId = "Drive_train1",  label = "Drive_train", choices = as.character(unique(Data1$Drive_train)), selected = "Front"),width = 2,solidHeader = T),
            box(selectInput( inputId = "Engine1",  label = "Engine", choices = as.character(unique(Data1$Engine)), selected = "Petrol"),width = 2,solidHeader = T),
            box(selectInput(inputId = "Hand_Drive1",label = "Hand Driver",choices = as.character(unique(Data1$Hand_Drive)),selected = "Left"),width = 2,solidHeader = T),
             box(numericInput(inputId = "Price",label = "Price",value = 6000),width = 2,solidHeader = T),
             box(selectInput(inputId = "Color1",label = "Color",choices = as.character(unique(Data1$Color)),selected = "black"),width = 2,solidHeader = T),
             box(verbatimTextOutput("tabletwo"), width = 12, solidHeader = T),
             #predictions
             box( #box prediction
               title = "Prediction base on DT", status = "primary", solidHeader = T,
               width = 12,
            valueBox(subtitle = " Probability of the car to be purchased", value = textOutput("predictionofdec"), color = "blue", width = 4, icon = icon("product-hunt")),
            box(sliderInput("cutoff", label = h6("Please select the treshhold"), min = 0, max = 1, value = 0.35), width = 4, height = 110),
            valueBox(subtitle = "The status", value = textOutput("soldunsold"), color = "blue", width = 4, icon = icon("car-crash"))
               
                                  )#box prediction
             
            
              )#fluidRow
         ), #tabItem#4
#=============================================================     
             # Page from the menuitem 5
#=============================================================
       tabItem(tabName = "fifthtab", 
           tabBox(
             #Tabpanel 1
             tabPanel( title = "Problems and Suggestions", 
               tags$ul ( 
                 tags$li(em(h3("The   methods   and   sources   of   gathering   data   do   not   guarantee   100%   confidence   inwhether a car was sold or no"),style="color:blue"),
                   em(h3("Having a clear indication of whether a car was sold either on the listings directory or elsewhere.",br(),
                     "Having this data available before starting the research would substantially increase the accuracy of models and predictions."))),
                 
                 tags$li(em(h3("The car listing platform auto.am does not provide information on price changes andfinal sale price."),style="color:blue"),
                   em(h3("Real sale price information would also help to increase the accuracy of the models.", br(),
                        "Having precise publish date would allow to do survival analysis, which is highly dependent on time indication."))),
                 
                 tags$li(em(h3("There  are  more  error-prone  data pieces  in  car   market  research,   which  are  out  of control   of   the   researcher."),style="color:blue"),
                   em(h3("For example, many   sellers   publish   incorrect   information(mileage, car condition) to mislead buyers and secure in-person car inspections.")))
                 
                 )
               ),
             #Tabpanel 2
             tabPanel( title = "Thank you", 
               tags$b (h2("Thank you for your attention and patience!"), style ="color:darkblue")#tags$b
               , align = "center") #tabPanel 2 from tabBox
         , width = 15 )# tabBox from tabItem #5
           ) #tabItem #5
         
         
         
         
#================================================================         
         
        )#tabItems
     
      ) # dashboardBody
  
    )#dashboardPage

  )#shinyUI


