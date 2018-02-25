library(shiny)
library(tm)
library(RColorBrewer)
library(qdap)
library(dplyr)
library(shinythemes)
dat2_sb <- readRDS("data-EN/dat2_sb.rds")
dat3_sb <- readRDS("data-EN/dat3_sb.rds")
dat4_sb <- readRDS("data-EN/dat4_sb.rds")
dat5_sb <- readRDS("data-EN/dat5_sb.rds")

source("data-EN/predict_sb.R", local = TRUE)
##########################################################

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("PredNext - Predicting the next word with Stupid Backoff", windowTitle = "PredNext"),
  
    sidebarPanel(

    br(),
    h3("Instructions"), hr(),
    "- Start typing text in the box below",br(),
    "- Predictions show when you type a SPACE after a word",br(),
    "- 5 top predictions are shown below the text box",br(),
    "- Click on any of them to complete the sentence",br(),
    "- A longer list is shown on the right panel",br(),hr(),
    h3("Enter text below"),
    textInput(inputId = "text", 
            label = "", 
            placeholder = "Start typing ..."),
    h3("Predictions"),
    htmlOutput(outputId = "word1",inline = T),
    htmlOutput(outputId = "word2",inline = T),
    htmlOutput(outputId = "word3",inline = T),
    htmlOutput(outputId = "word4",inline = T),
    htmlOutput(outputId = "word5",inline = T),
    width = 6
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Prediction app",(
          DT::dataTableOutput(outputId = "wordsDF")
          )
        ),#close tabpanel 1
  tabPanel("Language Model",
           
           
           tags$div(
             br(),
             "We followed this process in order to create an app that predicts the next word: ", 
             br(), br(), tags$li("Get training data: it consists of large corpuses of blogs, news articles and tweets. "), 
             tags$li("Pre-process the text data: since our language model is based on ngrams (a succession of 1, 2 or more words), we needed to preprocess the data by isolating each sentence as a line of text, removing profane language, lowercasing, . "), 
             tags$li("Extract all ngrams, we used here unigrams, bigrams, trigrams, fourgrams and fivegrams. "), 
             tags$li("Test most popular language models adapted to predicting the next word: Katz backoff, Kneser-Ney, or Stupid Backoff. "), 
             tags$li("Build the function and the shiny application. "), br(), 
             "We selected for the final product the stupid backoff algorithm. Even though the other options like Katz backoff and especially Kneser-Ney Smoothing offer a superior logic, the stupid backoff algorithm performs well enough.",
             h3("Stupid Backoff"), 
             "It's the simplest solution for web-scale ngrams, it allows to compute very quickly a score (rather than a probability).", br(),
             tags$img(src = "http://52opencourse.com/?qa=blob&qa_blobid=7041983226883037607", width = "`660px", height = "280px"), br(),
             
             "To illustrate how it was implemented, I take an example with the beginning of a Beatles song: 'with a little help' . .", 
             
             h4("Starting with 5gram candidates (search for fourgram input in fivegram dataset):"), 
             tags$li("Our input text is the fourgram 'with a little help'"), 
             tags$li("We search for 5gram candidates through our fivegrams dataset, all 5grams that begin with '^with a little help'."), 
             tags$li("We find in this example only one 5gram starting with 'with a little help ', we measure the score: count('with a little help from') / count('with a little help'): 64/75 = 0.85"), 
             h4("Back off to next lower order ngram (search for 3gram input in 4gram dataset): "),    
             tags$li("We want to find more suggestions since we only have one so far, we back off to one lower order ngram. "), 
             tags$li("Our new input text is now a trigram: 'a little help' (we ignore 'with' that was at the beginning). "), 
             tags$li("In the 4grams training set, we find 3 fourgrams starting with 'a little help': "), 
             "'a little help FROM' (count = 99)", br(), 
             "'a little help WITH' (count = 9) ", br(), 
             "'a little help IN' (count = 5)", br(), 
             tags$li("In the 3grams training set, we find 188 occurences of 'a little help' (it includes also occurences when 'a little help' is at the end of a line of text/sentence). "), 
             tags$li("We ignore the 'FROM' suggestion since we have it as a candidate already. "), 
             tags$li("We compute the scores for the 2 remaining options: "), 
             "'WITH - score = 0.4 * 9 / 188 = 0.019", br(), 
             "'IN' - score = 0.4 * 5 / 188 = 0.011", br(), 
             h4("Back off to next lower order ngram (search for 2gram input in 3gram dataset): "), 
             tags$li("We follow exactly the same principle. "), 
             tags$li("The score is this time 0.4 x 0.4 x count(3gram candidate) / count(2gram) "), 
             tags$li("For instance, 'little help ON' has a count of 5, the bigram 'little help' has a count of 221, the score is 0.4 x 0.4 x 5/221 = 0.0036 "), br(), 
             
             "We continue the process until we find our 3 (or more) suggestions.", br(), 
             "In this case, we find:", br(), 
             "from (score = 0.85)" , br(), 
             "with (score = 0.019)", br(), 
             "in (score = 0.011)", br(), 
             "on (score = 0.0036)" , br(), br(), 
             
             "To summarize, stupid backoff computes a score using MLE, starting with the highest order ngram. If we find enough suggestions, we stop here, if not, we back off one ngram order and apply a penalty of lambda (0.4 is the most commonly chosen value), and we continue the backoff process (multiplying by 0.4 for each lower order ngram we back off to), until we find enough suggestions/predictions.",    
             br(), br()
             
             
           )
           
           
           
           ),
  tabPanel("References",
           
           br(),
           h2("Project files"), 
           "All my project files can be found on Github:", tags$a(href = "https://github.com/xvalda/PredNext---Predict-Next-Words-with-language-models", "https://github.com/xvalda/PredNext---Predict-Next-Words-with-language-models"),
           tags$li("Markdown files with process explanation and code: 1. Milestone report, 2. Preparing full datasets, 3. Understanding, implementing and testing the algorithms"),
           tags$li("datasets and function files"), 
           tags$li("the shiny file"), 
           h2("References"), 
           h4("General"), 
           tags$li(tags$a(href="https://web.stanford.edu/~jurafsky/slp3/ed3book.pdf", "Speech and Language Processing, Jurafsky & Martin"),"28.08.2017, Chapter 4"),
           tags$li(tags$a(href="https://www.youtube.com/watch?v=s3kKlUBa3b0", "Language Modeling Course Videos - Stanford NLP - Professor Dan Jurafsky & Chris Manning")),
           tags$li(tags$a(href="https://nlp.stanford.edu/~wcmac/papers/20050421-smoothing-tutorial.pdf", "NLP Lunch Tutorial: Smoothing"), "Bill MacCartney, 21.04.2005"), 
           tags$li(tags$a(href="https://rpubs.com/pferriere/dscapreport", "Word Prediction Using Stupid Backoff With a 5-gram Language Model"), "Phil Ferriere, April 2016"),
           h4("Katz Backoff: "), 
           tags$li(tags$a(href="https://en.wikipedia.org/wiki/Katz%27s_back-off_model", "Katz's back-off model on Wikipedia")),
           tags$li(tags$a(href="http://l2r.cs.uiuc.edu/~danr/Teaching/CS546-09/Papers/Katz87.pdf", "Language Model Component of a Speech Recognizer"), "Slava M. Katz, 03.03.1987"),
           tags$li(tags$a(href="https://thachtranerc.wordpress.com/2016/04/12/katzs-backoff-model-implementation-in-r/", "Katz's Backoff Model Implementation in R"), "Thach-Ngoc TRAN, 12.04.2016"),
           h4("Good-Turing: "), 
           tags$li(tags$a(href="https://en.wikipedia.org/wiki/Good%E2%80%93Turing_frequency_estimation", "Good-Turing frequency estimation on Wikipedia")),
           h4("Stupid backoff: "), 
           tags$li(tags$a(href="http://www.aclweb.org/anthology/D07-1090.pdf", "Large language models in machine translation by Thorsten Brants et al, in EMNLP/CoNLL 2007")),
           tags$li(tags$a(href="https://rpubs.com/pferriere/dscapreport", "Word Prediction Using Stupid Backoff With a 5-gram Language Model"), "Phil Ferriere, April 2016"),
           h4("Kneser-Ney"), 
           tags$li(tags$a(href="https://web.stanford.edu/~jurafsky/slp3/ed3book.pdf", "Speech and Language Processing, Jurafsky & Martin"),"28.08.2017, Chapter 4"),
           tags$li(tags$a(href="https://rpubs.com/pferriere/dscapreport", "Word Prediction Using Stupid Backoff With a 5-gram Language Model"), "Phil Ferriere, April 2016"),
           h2("Contact"),
           tags$a(href = "https://www.linkedin.com/in/xavier-valdayron-9707231", "Xavier Valdayron - xavier@measuringsocial.com")
           )),
  width = 6
)#close main p\anel
)#close fluidpage
 
##########################################################


server <- function(input, output, session) {

  observe({

  wordsDF <- reactive({
  req(input$text)
  if(grepl("\\s$", input$text) == TRUE){
  input_text <- input$text
  res <- predict_sb(input_text)
  }
  })

  word1 <- reactive({
    word1 <- wordsDF()[1,1]
  })
    
  word2 <- reactive({
    word2 <- wordsDF()[2,1]
  })
  
  word3 <- reactive({
    word3 <- wordsDF()[3,1]
  })
  
  word4 <- reactive({
    word4 <- wordsDF()[4,1]
  })
  
  word5 <- reactive({
    word5 <- wordsDF()[5,1]
  })
  
  
  
  
  
  ######output  
  
  output$wordsDF <- DT::renderDataTable({
  DT::datatable(data = wordsDF(), 
      options = list(pageLength = 10), 
      rownames = FALSE)
  })
  
  output$word1 <- renderText(word1())
  output$word2 <- renderText(word2())
  output$word3 <- renderText(word3())
  output$word4 <- renderText(word4())
  output$word5 <- renderText(word5())

  ############
  output$word1 <- renderUI({
    actionButton("actionbutton1", label = word1(), width = '15%')
  })
  output$word2 <- renderUI({
    actionButton("actionbutton2", label = word2(), width = '15%')
  })
  output$word3 <- renderUI({
    actionButton("actionbutton3", label = word3(), width = '15%')
  })
  output$word4 <- renderUI({
    actionButton("actionbutton4", label = word4(), width = '15%')
  })
  output$word5 <- renderUI({
    actionButton("actionbutton5", label = word5(), width = '15%')
  })
  
  observeEvent(input$actionbutton1, {
    if(input$actionbutton1 == 1){
      updateTextInput(session, "text", value=paste0(input$text, word1(), " "))
    }
  })
  
  observeEvent(input$actionbutton2, {
    if(input$actionbutton2 == 1){
      updateTextInput(session, "text", value=paste0(input$text, word2(), " "))
    }
  })
  
  observeEvent(input$actionbutton3, {
    if(input$actionbutton3 == 1){
      updateTextInput(session, "text", value=paste0(input$text, word3(), " "))
    }
  })
  observeEvent(input$actionbutton4, {
    if(input$actionbutton4 == 1){
      updateTextInput(session, "text", value=paste0(input$text, word4(), " "))
    }
  })
  observeEvent(input$actionbutton5, {
    if(input$actionbutton5 == 1){
      updateTextInput(session, "text", value=paste0(input$text, word5(), " "))
    }
  })
  
  #############
  
  })#close observe
  
}


# Create Shiny app object
shinyApp(ui = ui, server = server)
