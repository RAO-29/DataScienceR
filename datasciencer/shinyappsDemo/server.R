if(require("shiny")==FALSE) install.packages("shiny") 
## Load shiny
library("shiny")

if(require("tidyverse")==FALSE) install.packages("tidyverse") 
## Load tidyverse
library("tidyverse")

if(require("LDAvis")==FALSE) install.packages("LDAvis") 
## Load LDAvis
library("LDAvis")

if(require("RJSONIO")==FALSE) install.packages("RJSONIO") 
## Load RJSONIO
library("RJSONIO")

if(require("tsne")==FALSE) install.packages("tsne")
library("tsne")

## Reading from file
source("TwitterLDA.R")


shinyServer(function(input, output) {
  reactiveTwitterLDA <- reactive ({
  ## input-parameters have to be mentioned once outside of 
  ## call-context, so that Shiny 'registers' them as reactive
  # input$goButton
  # infoPopup <- isolate(input$infoPopup)
  
  # if (infoPopup) { 
    showModal(modalDialog(
    title = NULL,
    size = "s",
    "Fetching Tweets&Fitting Topic Model...",
    footer = NULL,
    easyClose = TRUE
    )) 
    # }
  
  # ## 'isolated' reactive components get only called when some other,
  # ## non-isolated reactive component like the 'goButton' gets called as well
  # streamtime <- isolate(input$streamtime)
  # if (is.null(streamtime)||!is.numeric(streamtime)) { streamtime <- 10 }
  
  # query <- isolate(input$query)
  # if (is.null(query)) { query <- "realdonaldtrump,maga,america" }
  # ## Remove all kinds ( [[:space:]] ) of whitespaces from query
  # query <- base::gsub("[[:space:]]","", query)
  
  minTopics <- isolate(input$Topics[1])
  maxTopics <- isolate(input$Topics[2])
  # batch_processing <- isolate(input$batch_processing)
  # bot_detection <- isolate(input$bot_detection)
  
  TwitterLDA(batch_processing = TRUE, 
             bot_detection = FALSE,
             minTopics = minTopics,
             maxTopics = maxTopics)
  
  ## Might be useful to have the tweets/documents on hand
  
  ## Checking for existance of output - output gets wiped
  ## each time before streaming, so any error in processing results in 
  ## a reference to NULL)
  # if(base::file.exists("output/filtered_tweets.json")) { 
    tweets <- jsonlite::fromJSON("output/filtered_tweets.json")
    # if (infoPopup) {
      noTweets <- base::NROW(tweets)
      showModal(modalDialog(
        title = NULL,
        size = "s",
        paste("Fetched",noTweets,
              "Tweets and fitted Topic Model. Rendering...",
              sep = " ", collapse = NULL),
        footer = NULL,
        easyClose = TRUE
      )) 
      # } } else { 
        # showModal(modalDialog(
        #   title = NULL,
        #   size = "s",
        #   "Looks like there are no recent Tweets for your query, sorry.",
        #   easyClose = TRUE
        # ))
        
      # }
}
)

runReactive <- reactive({
  reactiveTwitterLDA()
  
  no.terms <- input$Terms
  
  ## Phi matrix, with each row containing the distribution over terms 
  ## for a topic, with as many rows as there are topics in the model, and as 
  ## many columns as there are terms in the vocabulary; 
  ## Also check if Phi exists
  if(base::file.exists("output/LDAGibbsPerTopicPerWordProbabilities.csv")) {
    phi <- read.csv(file="output/LDAGibbsPerTopicPerWordProbabilities.csv", 
                    header=TRUE, sep=",")
    phi <- dplyr::select(phi, -X)
    phi <- as.matrix(phi)
    
    ## Theta-matrix, with each row containing the probability distribution
    ## over topics for a document, with as many rows as there are documents in the
    ## corpus, and as many columns as there are topics in the model.
    theta <- read.csv(file="output/LDAGibbsPerDocumentPerTopicProbabilities.csv", 
                      header=TRUE, sep=",")
    theta <- dplyr::select(theta, -X)
    theta <- as.matrix(theta)
    
    ##  vocab character vector of the terms in the vocabulary (in the same
    ## order as the columns of \code{phi}). Each term must have at least one
    ## character.    
    vocab <- read.csv(file="output/LDAGibbsWords.csv", 
                      header=TRUE, sep=",")
    vocab <- dplyr::select(vocab, -X)
    vocab <- t(vocab)
    vocab <- as.vector(vocab)
    
    ## doc.length integer vector containing the number of tokens in each
    ## document of the corpus.
    doc.length <- read.csv(file="output/DocumentLengths.csv", 
                           header=TRUE, sep=",")
    doc.length <- dplyr::select(doc.length, -X)
    doc.length <- t(doc.length)
    doc.length <- as.vector(doc.length)
    
    ## term.frequency integer vector containing the frequency of each term 
    ## in the vocabulary.
    term.frequency <- read.csv(file="output/TermFrequencies.csv", 
                               header=TRUE, sep=",")
    term.frequency <- dplyr::select(term.frequency, -X)
    term.frequency <- t(term.frequency)
    term.frequency <- as.vector(term.frequency)
    
    ## Principal Component Analysis is the actual default for scaling in the 
    ## LDAvis-package - given that there might be more than a hundred
    ## topics when building a topic model from tweets, this scaling method
    ## potentially produces a very 'crowded' bubble-diagram
    if (input$scaling=="PCA") {
      jsonLDAvis <- LDAvis::createJSON(phi = phi, theta = theta, 
                                       doc.length = doc.length, 
                                       vocab = vocab, 
                                       term.frequency = term.frequency, 
                                       R = no.terms, mds.method = jsPCA, 
                                       plot.opts = list(xlab = 
                                                          "PC1", ylab = "PC2"))
    }
    ## t-sne is the default case - the resulting even spacing lends itself
    ## to a first cursory glance
    else { 
      jsonLDAvis <- LDAvis::createJSON(phi = phi, theta = theta, 
                                       doc.length = doc.length, 
                                       vocab = vocab, 
                                       term.frequency = term.frequency, 
                                       R = no.terms, mds.method = svd_tsne,
                                       plot.opts = list(xlab="", ylab="") )
    }
  }
}) 
output$LDAvis <- LDAvis::renderVis({runReactive()}) })


## T-SNE (t-distributed stochastic neighbor embedding)
## can be used instead of PCA for getting the scaling of topics; Suggestion
## by author of LDAvis-package
svd_tsne <- function(x) tsne::tsne(svd(x)$u)