library(tidyverse)
library(jsonlite)
library(httr)
library(stringr)
library(shiny)

mydata3<-data.frame()
for (i in 1:3950){
  r <- GET(
    "https://api.harvardartmuseums.org/person",
    query = list(
      apikey = "bc3fb240-5a61-11ea-b877-8f943796feb8",
      page = i
    )
  )
  stop_for_status(r)
  json <- content(r, as = "text", encoding = "UTF-8")
  fromJSON(json)
  k<-as.data.frame(fromJSON(json))%>%
    select(records.displayname,records.culture,records.gender,records.birthplace,records.deathplace,records.displaydate,records.url)
  mydata3<-rbind(mydata3,k)
}
data3<-mydata3 %>% drop_na() %>% arrange(records.displayname)
data3[data3$records.displayname == "Rudy Burckhardt",]
names<-data3$records.displayname

shinyApp(
  ui = fluidPage(
    selectizeInput("variable", "Select Artist Name:", choices = c(data3$records.displayname), options = list(maxOptions = 4010)),
    tableOutput("data")
  ),
  server = function(input, output) {
    b <- reactive({
      a<-t(data3[data3$records.displayname == input$variable,])
      as.data.frame(cbind(Information = c("Name:","Country:","Gender:","Birth Place:","Death Place:","Date:","Havard Museum Access URL:"),
                          Information = a))
    })
    output$data <- renderTable({
      b()
    }, rownames = FALSE)
  }
)
