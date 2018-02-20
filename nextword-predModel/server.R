#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#source("helpers.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$predWord <- renderText({predWord <- input$userWords})
  #output$predWord <- renderText({ paste(filter_text(get_pred(input$input_str))) })
})
