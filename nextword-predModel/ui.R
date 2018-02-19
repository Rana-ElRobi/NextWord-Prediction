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
  headerPanel("This Application helps you to save more time while typing, it predicts the next word you may want to say "),
  
  
  # Sidebar with a slider input for number of bins 
  
  sidebarLayout(
    
    # input section for words the user gona enter 
    sidebarPanel(
      helpText("Enter the words you want to Say in the next textbox "),
      textInput("userWords",label="Input Area", value = "Type here ..."),
      sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
