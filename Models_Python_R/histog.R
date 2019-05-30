data<-read.csv("final.csv")
# Load packages
library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
factors<-c("brand","Color","Hand_Drive","Gearbox","Drive_train","sold","Engine")


# Define UI for application that plots features of movies
ui <- fluidPage(
  
  br(),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs
    sidebarPanel(
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Price"= "price", 
                              "Year"= "year", 
                              "Horsepower"= "Horsepower",
                              "Mileage",
                              "Engine Cylinders"="Engine_Cylinders",
                              "Engine Volume"  ="Engine_Volume"), 
                  selected = "Price"))
                
    
    # Output:
    mainPanel(
      # Show boxplot with brushing capability
      plotOutput(outputId = "boxplot", brush = "plot_brush"),
    
      br()
    )
  ))

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$boxplot <- renderPlot({
    ggplot(data = data, aes_string( x = input$x,color = input$z)) +
      geom_histogram()+theme(axis.text.x =element_text(angle =60,hjust = 1))
  })}
  # Create data table
  

# Create a Shiny app object
shinyApp(ui = ui, server = server)