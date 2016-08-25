# set working directory... take this out when the app goes live
# setwd('~/Coursera/Capstone/app')

# load the necessary packages
library(shiny)
library(wordcloud)
library(RColorBrewer)

# source the data used by the app
# one.gram.freq <- read.csv('oneGramTrim.csv')
# two.gram.freq <- read.csv('twoGramTrim.csv')
# three.gram.freq <- read.csv('threeGramTrim.csv')
# four.gram.freq <- read.csv('fourGramTrim.csv')
# 
# # source the predictText algorithm
# source('predictText.R')

# create the server object
shinyServer(function(input, output) {
  
  output$appDesc <- renderText('This app allows the user to enter text in the box below. When the user presses the "Predict Next Word" button, an algorithm will run and output the predicted next word based on a corpus. A word cloud will also be generated to the right of the screen showing all the possible next words based on the inputed text.')    
  
  runPrediction <- reactive({ 
  
    predictText(input$text)
  })
  
  output$predictedText <- renderUI(
    
    paste(input$text, runPrediction(), sep=' ')
  )
  
  makeCloudContent <- reactive({
    
    wordCloudTerms(input$text)
  })
  
  output$plot <- renderPlot({
    
    wordcloud(makeCloudContent()$Words, makeCloudContent()$Freq, scale=c(5, 0.5), min.freq = min(makeCloudContent()$Freq), max.words = length(makeCloudContent()$Words), colors=brewer.pal(8, 'Dark2'))
  })

})