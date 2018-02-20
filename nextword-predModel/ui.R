#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Welcome To Next word Predection App"),
  
  # gives brief info about what the app is using for
  fluidRow(column(12,helpText("This Application helps you to save more time while typing, it predicts the next word you may want to say "))),
  
  # input section for words the user gona enter 
  sidebarLayout(
    sidebarPanel(
      # Take input typing
      helpText("Enter the words you want to Say in the next textbox "),
      textInput("userWords",label="Input Area", value = " ")
    ), # close sidebar panel
    mainPanel(
      # Show results
      verbatimTextOutput("predWord") 
    ), # close of main panel
    
    position = c("left","right")
  )
 

  ))# close of Shiny Page 
