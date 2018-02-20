
library(shiny)
source("predModel.r")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  #output$predWord <- renderText({predWord <- input$userWords})
  output$predWord <- renderText({ paste(filter.text(get.pred(input$userWords))) })
})
