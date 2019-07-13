if(require("shiny")==FALSE) install.packages("shiny") 
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

  shinyUI(fluidPage(
    title = "StopMoVis", 
    ## fluidRow(column(4, offset = 4, titlePanel("LDAvis"))),
    br(),
      fluidRow(
        column(5, helpText("Note: Demo version with text dataset, streamed on 12th January '19 for 60 seconds and the query 'brexit,may,vote' with the actual interactive streaming component.")), 
      
      column(5, offset = 1, selectInput(inputId = "scaling", label = NULL, 
                                        choices = c(
                                          "Scaling Method: t-Distributed Stochastic Neighbor Embedding" = "tsne",
                                          "Scaling Method: Principal Component Analysis" = "PCA"), 
                                        selected = "tsne", 
                                        width = "100%"))),
    
    fluidRow(column(5, sliderInput(inputId = "Topics", 
                                   label = "Range of Topic Number:",
                                   min = 3, max = 103, step = 10,
                                   value = c(3,103),
                                   width = "100%")),
             column(5, offset = 1, sliderInput(inputId = "Terms", 
                                               label = "Number of Terms per Topic:",
                                               min = 10, max = 50, step = 10,
                                               value = 20,
                                               width = "100%"))),
    
    hr(),
    
    fluidRow(column(12, visOutput("LDAvis") )
    )
  )
  ) 