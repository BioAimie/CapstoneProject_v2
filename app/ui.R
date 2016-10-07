# load the necessary packages
library(shiny)

shinyUI(fluidPage(
  
  includeCSS('style.css'),
  
  headerPanel('Predictive Text App'),
  
  fluidRow(

    column(12, textOutput('appDesc'))
  ),
  
  fluidRow(
    
    column(8,
           textInput('text', label=h3('Please type some text:'), value=''),
           submitButton(text='Predict Next Word'),
           uiOutput('predictedText', class = 'outputText')
    ),
    
    # add a word cloud here
    column(4,
           plotOutput('plot')
    )
  )
))