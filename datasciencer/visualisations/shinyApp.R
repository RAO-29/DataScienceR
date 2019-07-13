if(require("shiny")==FALSE) install.packages("shiny") 
## Load shiny
library("shiny")

if(require("ggplot2")==FALSE) install.packages("ggplot2") 
## Load ggplot2
library("ggplot2")

if(require("plotly")==FALSE) install.packages("plotly") 
## Load ggplot2
library("plotly")


if(require("dplyr")==FALSE) install.packages("dplyr") 
## Load plyr
library("dplyr")

if(require("grid")==FALSE) install.packages("grid") 
## Load grid
library("grid")

if(require("tidyverse")==FALSE) install.packages("tidyverse") 
## Load tidyverse
library("tidyverse")

if(require("zoom")==FALSE) install.packages("zoom") 
library(zoom)

# setwd <- ("..")
# parent <- getwd()
# setwd(parent)
# print(parent)


##create input list of files
ui <- fluidPage(
  selectInput("File", "Choose a File:",
              list(`Files` = c("Term Frequency","Topic - Term Probabilities"))
              
              
  ),
  
  dataTableOutput("contents"),
  mainPanel
  (
    plotOutput("plott", width = "2500px", height = "2500px")
  )
)


##render output based on selected input file
server <- function(input, output) {
  #TwitterLDA()
  output$contents <- renderDataTable({
    
    inFile <- input$File
    
    # if(inFile=="Term Frequency")
    # {
    #   
    #   data1 <- read.csv(file="output/TermFrequencies.csv", header=TRUE, sep=",")
    #   #data1 <- dplyr :: select(data1, -X)
    #   w<- read.csv(file = "output/LDAGibbsWords.csv", header = TRUE, sep = ",")
    #   
    #   data1 <- cbind(data1, w$x)
    #   
    #   
    #   
    # }
    #   
    #   
    #   else if (inFile=="PerTopicPerWordProbabilities")
    #   {
    #     data1 <- read.csv(file="output/LDAGibbsPerTopicPerWordProbabilities.csv", header=FALSE, sep=",")
    #     #data1 <- dplyr :: select(data1, -V1)
    #     data1 <- dplyr :: slice(data1, 2:length(data1))
    #     words <- read.csv(file="output/LDAGibbsWords.csv", header=TRUE, sep=",")
    #     ## extract the terms from csv file and place them as column names
    #     exractedWords <- words$x
    #     colnames(data1) <- paste(exractedWords)
    #     data1 
    #   }
    #  
    #   
  })
  
  output$plott<-renderPlot({
    
    
    inFile <- input$File
    
    
    if(inFile=="Term Frequency")
    {
      
      data1 <- read.csv(file="output/TermFrequencies.csv", header=TRUE, sep=",")
      # data1 <- dplyr :: select(data1, -X)
      w <- read.csv(file = "output/LDAGibbsWords.csv", header = TRUE, sep = ",")
      
      data1 <- cbind(data1, w$x)
      
      names(data1)[3]<-"terms"
      names(data1)[2]<-"frequency"
      names(data1)[1]<-"index"
      head(data1)
      # mydf<-as.data.frame(data1)
      
      labelNames = c(data1$terms)
      individual <- as.factor(data1$terms)
      #individual = paste(data1$terms)
      print(individual)
      label_data=data1
      number_of_bar=nrow(label_data)
      print(number_of_bar)
      #number_of_terms = nrow(data1)
      angle= 90 - 360 * (label_data$index-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
      
      label_data$hjust<-ifelse( angle < -90, 1, 0)
      label_data$angle<-ifelse(angle < -90, angle+180, angle)
      
      head(label_data)
      # Make the plot
      p = ggplot(label_data, aes(x=as.factor(label_data$index), y=label_data$frequency)) +     # Note that id is a factor. If x is numeric, there is some space between the first bar
        
        # This add the bars with a blue color
        geom_bar(stat="identity", fill=alpha("green", 0.6)) +
        
        # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
        ylim(-60, 200) +
        
        #Custom the theme: no axis title and no cartesian grid
        theme_minimal() +
        theme(
          #axis.text.x = element_text(angle = 15, hjust = 0.5),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-2,8), "cm")     # This remove unnecessary margin around plot
          # plot.margin = par(mar = c(1,1,1,1))
        ) +
        
        # This makes the coordinate polar instead of cartesian.
        coord_polar(start = 0) +
        
        geom_text(data=label_data,  aes(x=label_data$index, y=label_data$frequency, label = label_data$terms, hjust=hjust), color="black",check_overlap = TRUE, fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE)
      p
      #zm(type = "navigation", rp = NULL)
      #session.zoom()
    }
    
    
    
  })
  
  
}




shinyApp(ui, server)
