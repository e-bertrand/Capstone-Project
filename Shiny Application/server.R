library(shiny)
library(data.table)
library(DT)
source("functions/nextWordPredictor.R")
accuracy_results <- readRDS("data/accuracy_results_sbkf.rds")

function(input, output, session) {
  
  word1 <- reactiveValues(data = NULL)
  word2 <- reactiveValues(data = NULL)
  word3 <- reactiveValues(data = NULL)
  
  observe({
    updateActionButton(session, "msg", 
                       label = " Predictive applications are ready",
                       icon = icon("thumbs-up"))
      
    validate(
      need(grepl("(\\s+|\\.|\\;|\\!|\\?|\\:)$", input$text), 
           message = 'Type a blank, space or an end of sentence punctuation')
    )
      
    nw <- predNextWord(input$text,"phrase")$nextWord
    
    word1$data <- nw[1]
    word2$data <- nw[2]
    word3$data <- nw[3]
    
    updateActionButton(session, "w1", label = word1$data)
    updateActionButton(session, "w2", label = word2$data)
    updateActionButton(session, "w3", label = word3$data)
    
  })
  
  observeEvent(input$w1, {
    newText <- paste(input$text, word1$data, " ")
    newText <- gsub("\\s+", " ", newText)
    newText <- trimws(newText, which = "left")
    updateTextAreaInput(session, "text", value = newText)
  })

  observeEvent(input$w2, {
    newText <- paste(input$text, word2$data, " ")
    newText <- gsub("\\s+", " ", newText)
    newText <- trimws(newText, which = "left")
    updateTextAreaInput(session, "text", value = newText)
  })

  observeEvent(input$w3, {
    newText <- paste(input$text, word3$data, " ")
    newText <- gsub("\\s+", " ", newText)
    newText <- trimws(newText, which = "left")
    updateTextAreaInput(session, "text", value = newText)
  })
  
  observeEvent(input$nword, {
    output$nw <- renderText(
                    isolate(
                        paste("<font color=\"#8E0000\", size=\"4\"><b>",
                              predNextWord(input$phrase)$nextWords[1],
                              "</b></font>")
                        )
                    )
  })
  
  output$fourgrams <- DT::renderDataTable({
      DT::datatable(fourgramNextWord[c(750000, 750001, 750003)],
                    options = list(searching = FALSE, info = FALSE, 
                                   lengthChange = FALSE, paging = FALSE))
 })
  
  output$trigrams <- DT::renderDataTable({
      DT::datatable(trigramNextWord[549999:550001],
                    options = list(searching = FALSE, info = FALSE, 
                                   lengthChange = FALSE, paging = FALSE))
  })
  
  output$accuracy <- DT::renderDataTable({
      DT::datatable(accuracy_results,
                    options = list(searching = FALSE, info = FALSE, 
                                   lengthChange = FALSE, paging = FALSE))
  })
  
  output$process <- renderImage({
      filename <- normalizePath(file.path('./images', "Model.jpeg"))
      list(src = filename, width = 900, height = 540, alt = "Process schema")
  }, deleteFile = FALSE)
  
}