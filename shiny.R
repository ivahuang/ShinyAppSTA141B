library(tidyverse)
library(jsonlite)
library(httr)
library(shiny)

object <- GET(
  "https://api.harvardartmuseums.org/Object", 
  query = list(
    apikey = "c08f4b60-5a63-11ea-8cff-0d9fd01ffb33"))
stop_for_status(object)
json <- content(object, as = "text")
a <-fromJSON(json)

ui <- fluidPage(
  
  #Application title
  titlePanel("Explore Harvard Art Museum"),
  
  # Sidebar with a slider input
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "century",
                  label = "Select century of the exhibits:",
                  choices = unique(a$century),
                  selected = "17th century")
    ),
  
  mainPanel(
    tabPanel("Exhibit's Details",  tableOutput("detail"))
  )
)
)

server <- function(input, output) {
  
  #Filter based on input
  aa <- reactive({
    filter(a, origin == input$century)
  })
  
  # Render data table for selected flights
  output$dtab <- renderDataTable(aa())
  
  # Generate min, max, mean and median of corresponding arrival delay
  output$detail <- renderTable({
    if (length(input$checkBox) == 0) {
      return (NULL)
    }
    
    filter(aa(), origin == input$century) %>%
      select(century, title, url)
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server)
