# set working directory... take this out when the app goes live
# setwd('app/')

# load the necessary packages
library(shiny)
library(wordcloud)
library(RColorBrewer)

# source the data used by the app
one.gram.freq <- read.csv('oneGramTrim.csv')
two.gram.freq <- read.csv('twoGramTrim.csv')
three.gram.freq <- read.csv('threeGramTrim.csv')
four.gram.freq <- read.csv('fourGramTrim.csv')

# source the predictText algorithm
source('predictiveText.R')

# create the server object
shinyServer(function(input, output) {
  
  output$appDesc <- renderText('This app allows the user to enter text in the box below. When the user presses the "Predict Next Word" button, an algorithm will run and output the predicted next word based on a corpus. A word cloud will also be generated to the right of the screen showing all the possible next words based on the inputed text.')    
  
  # when the button is pressed, pass the input text to the predictiveText funtion... this returns a data frame
  runPrediction <- reactive({

    data.frame(predictiveText(input$text, TRUE))
  })
  
  # make the output for the text box
  output$predictedText <- renderUI(

    paste(input$text, as.character(runPrediction()[runPrediction()$Freq==max(runPrediction()$Freq),'Words']), sep=' ')
  )

  # make the output for the word cloud
  output$plot <- renderPlot({

    wordcloud(runPrediction()$Words, runPrediction()$Freq, scale=c(5, 0.5), min.freq = min(runPrediction()$Freq), max.words = length(runPrediction()$Words), colors=brewer.pal(8, 'Dark2'))
  })

})