data<-read.csv("final.csv")
# Load packages
library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)


# Define UI for application that plots features of movies
ui <- fluidPage(
  br(),sidebarLayout(sidebarPanel(
      # Select variable for y-axis
      selectInput(inputId = "yhist", 
                  label = "Y-axis:",
                  choices = c("Price"= "price", 
                              "Year"= "year", 
                              "Horsepower"= "Horsepower",
                              "Mileage",
                              "Engine Cylinders"="Engine_Cylinders",
                              "Engine Volume"  ="Engine_Volume"), selected = "Price"),
      sliderInput(inputId = "bins", 
                  label = "bins:", 
                  min = 30, max = 70, 
                  value = 2)),
 mainPanel(
      # Show boxplot with brushing capability
      plotOutput(outputId = "histogram", brush = "plot_brush"),
      
      br()
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$histogram <- renderPlot({
    ggplot(data = data, aes_string(x = input$yhist)) +
      geom_histogram(bins = input$bins,alpha=0.3
                     ,color="black",fill="forestgreen")+theme(axis.text.x =element_text(angle =60,hjust = 1))+labs(title=toupper(paste("Histogram of ",input$yhist)),
                                                                                                                                 x=toupper(paste(input$yhist)),y="COUNT")
  })
  }

# Create a Shiny app object
shinyApp(ui = ui, server = server)