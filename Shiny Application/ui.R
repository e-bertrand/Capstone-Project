library(shiny)
library(shinydashboard)
library(DT)

# Building the Dashboard header
header  <-  dashboardHeader(
    title = "Predictive Text Apps"
)

# Building the Dashboard sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Applications", tabName = "apps", icon = icon("list-alt")),
        menuItem("General info", tabName = "info", icon = icon("info")),
        menuItem("Technical details", tabName = "tech", icon = icon("th")),
        menuItem("Process schema", tabName = "proc", icon = icon("share-alt"))
    )
)

# Building the Dashboard body
body <- dashboardBody(
    tabItems(
        
        # Applications tab
        tabItem(tabName = "apps", 
                
                # Application 1 + progress message
                fluidRow(
                column(width = 5, h3("Application 1 - Predicting the next word")),
                column(width = 4, offset = 3,
                       actionButton(inputId = "msg", 
                                    label = "Please wait while data is being loaded...",
                                    icon = icon("refresh")))
                ),
                tags$style(type='text/css', "#msg { width:100%; margin-top: 15px;}"),
                box(width = NULL,
                    p(strong("Type your phrase and click 'Next word'")),
                    fluidRow(
                        column(width = 5, textInput(inputId = "phrase", value = " ", label = "",
                                                    width = "100%")),
                        
                        column(width = 2, actionButton(inputId = "nword", 
                                                       label = "Next word",
                                                       style="color: #fff; 
                                                       background-color: #265570; 
                                                       border-color: #2e6da4;
                                                       font-size: 100%")),
                
                        column(width = 2, offset = 0, htmlOutput(outputId = "nw"))
                    ),
                    tags$style(type='text/css', "#nword { width:100%; margin-top: 20px;}"),
                    tags$style(type='text/css', "#nw { width:100%; margin-top: 25px;}")
                ),
                
                box(height = 2, width = NULL),
                
                # Application 2
                h3("Application 2 - Avoid typing: choose the next word"),
                box(width = NULL,
                    p(strong("Type your message")),
                    fluidRow(
                        column(width = 5,
                               textAreaInput(inputId = "text", value = "", label = "", 
                                             width = "430px", rows = 6)
                        ),
                        column(width = 3, 
                               br(), 
                               p(em("As soon as you type a whitespace or", 
                                    "a punctuation mark (. ; : ! ?) at the end", 
                                    "of your text, you will see next word",
                                    "suggestions. You can choose one of them",
                                    " (clicking it) or continue typing.")))
                    ),
                    
                    p(strong("Click a word for adding it to the message")),
                    
                    actionButton(inputId = "w1", label = "...",
                                 style="color: #fff; 
                                  background-color: #265570; 
                                  border-color: #2e6da4;
                                  font-size: 120%"),
                    
                    actionButton(inputId = "w2", label = "...",
                                 style="color: #fff; 
                                  background-color: #265570; 
                                  border-color: #2e6da4;
                                  font-size: 120%"),
                    
                    actionButton(inputId = "w3", label = "...",
                                 style="color: #fff; 
                                  background-color: #265570; 
                                  border-color: #2e6da4;
                                  font-size: 120%")
                )
        ),
        
        # General info tab
        tabItem(tabName = "info",
                fluidRow(column(width = 10,
                h4(strong("How to use these applications?")),
                p("First of all, you must wait some seconds before models",  
                  "are fully loaded and the applications are ready ", 
                  "(shown the in upper-right corner). "),
                p("In the first application,",
                   strong("[Predicting the next word], "), 
                  " simply type, or paste, a phrase", 
                  "and click the 'Next word' button. On the right, ", 
                  "in bold red, you can see the predicted next word."),
                p("In the second application, ",
                  strong('[Avoid typing: choose the next word], '), 
                  "we simulate a mobile app: you start typing ", 
                  "and immediately after you introduce a whitespace or a ", 
                  "punctuation mark (. ; : ! ?) you can see the three most ", 
                  "likely continuations. You can choose one of them ", 
                  "(clicking on it) or continue typing. Remember, ",
                  strong("always type a whitespace"),  
                  " (or a punctuation mark) after the word to get predictions."),
                br(),    
                h4(strong("How can we predict next words?")),
                p("Predicting the next word is based on statistical language",
                  "models, called N-grams, with which the conditional probability for a",  
                  "word given the previous sequence in a phrase can be estimated.",
                  "For the purpose of training and building these N-grams up, ",
                  "we have used a corpora of text from news, blogs, and twitter. "),
                p("Combining the results obtained for sequences of one, two, three ",
                  "or four words (unigrams, bigrams, trigrams, and four-grams)",
                  "we predict the most -or the three most- likely continuations",
                  "for any typed phrase."),
                p("For more information about how the models have been built ",
                  "and the estimated performance of the predictive method, please go to the ",
                  a("Technical details", onclick = "openTab('tech')"), 
                  tags$script(HTML("var openTab = function(tabName){
                                   $('a', $('.sidebar')).each(function() {
                                   if(this.getAttribute('data-value') == tabName) {
                                   this.click()};});}")),
                  "page.")
                
        ))),
        
        # Technical detailed info tab
        tabItem(tabName = "tech",
                fluidRow(column(width = 11,
                h4(strong("How the language models have been built?")),
                p("The N-grams models have been trained with data from three", 
                  "different text corpora: news (~75,000 element), blogs ", 
                  "(~900,000 elements) and twitter messages (~2,300,000 ", 
                  "elements). In order to be able to evaluate the performance ", 
                  "of the models, the corpora have been splitted in two subsets: ", 
                  "one for training itself (80%) and one for test (20%). At ", 
                  "the end of a process that includes cleaning corpora, training ", 
                  "models, and consolidating results, ", 
                  "we obtain a set of 4-grams, trigrams, bigrams ", 
                  "and unigrams. These tasks are supported by the R packages ", 
                  "'tm' and 'stylo'."),
                
                p("With the goal of reducing processing time and the loading ", 
                  "cost and data footprint of the models in the server, we ", 
                  "have curbed the N-grams applying three criteria:"), 
                tags$ul(
                    tags$li("Pruning the ngrams with total frequency below 3."), 
                    tags$li("Covering word sequences that were not present in", 
                            "the training corpora ('out of vocabulary' OOV ", 
                            "sequences) through the 'stupid backoff' algorithm: ", 
                            "if a sequence is not present in a higher-order ", 
                            "N-gram, we remove the first word and backoff to ", 
                            "a lower-order N-gram, ", 
                            "weighing the result by a fixed factor 'lambda' ", 
                            "(in our case lambda = 0.4, as recommended by ", 
                            "proponents of the algorithm)."),
                    tags$li("Reorganizing the N-grams selecting only the three ", 
                            "most likely continuations for each sequence of ", 
                            "words detected in the training corpora (after ", 
                            "scoring results with stupid backoff).")
                    ),
                p("This strategy sacrifices some accuracy in favor of ", 
                  "greater speed in prediction, which is one of the ", 
                  "requirements on mobile apps."),
                p("Here we can see a small sample of the prediction tables",  
                  "generated from the 4-grams and trigrams (likelihood ", 
                  "goes down from w_1 to w_3):"),
                fluidRow(
                    column(width = 6, DT::dataTableOutput('fourgrams')),
                    column(width = 5, offset = 1, DT::dataTableOutput('trigrams'))
                ),
                br(),
                h4(strong("Evaluating model's performance")),
                p("The simplest way to evaluate how our language model ", 
                  "performs is measuring the accuracy: the ratio between ", 
                  "number correct predicted words and total number of ", 
                  "predictions. We have measured the accuracy of our model ", 
                  "against a sample of 100,000 different sequences extracted ", 
                  "from the test subset, which was not used in the training ", 
                  "phase to avoid biased results."),
                p("The following table summarizes the results using N-grams ", 
                  "of increasing order (backed by the lower-order ones in ", 
                  "case of  OOV sequences) and considering as a correct ", 
                  "prediction both cases: only the most likely word ", 
                  "('accuracy_1w'); or anyone of the three most likely ", 
                  "('accuracy_3w'):"),
                p(strong("Accuracy (expressed as percentage of correct predictions)")),
                fluidRow(
                    column(width = 5, DT::dataTableOutput('accuracy'))
                ),
                br(),
                p("Although accuracy figures seem modest at first glance, ", 
                  "when you consider the number of different words in the ", 
                  "corpora vocabulary  that potentially can be selected as ", 
                  "next word (~675,000) and the relative simplicity of the ", 
                  "N-grams models, they are quite remarkable."),
                p("Unfortunately, with limited resources (2 cores CPU / 8 GB RAM) it is not ", 
                  "possible consider further improvements: building a 5-grams ", 
                  "model; avoiding pruning when you train the models; or ", 
                  "applying more powerful smoothing algorithms than 
                  'stupid backoff'.")
        ))),
        
        # Process schema tab
        tabItem(tabName = "proc",
                h4(strong("Schema of the whole process")),
                imageOutput("process")
        )
    )
)

# Putting all dashboard elements together
dashboardPage(skin = "yellow",
    header,
    sidebar,
    body
)
