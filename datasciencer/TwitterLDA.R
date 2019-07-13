## Example call (repeating the defaults):
## Not run:
 # TwitterLDA(streamtime = 10,
 #           batch_processing = TRUE,
 #          file_name = "tweets.json",
 #          bot_detection = FALSE,
 #          query = "realdonaldtrump,maga,america",
 #          minTopics = 3,
 #          maxTopics = 103,
 #          token = NULL)
## End(Not run):

TwitterLDA <- 
  function(streamtime = 10, 
           batch_processing = TRUE, 
           file_name = "tweets.json", 
           bot_detection = FALSE,
           query = "realdonaldtrump,maga,america",
           minTopics = 3,
           maxTopics = 103,
           token = NULL) 
  {
    
    ## Install rtweet
    if(require("rtweet")==FALSE) install.packages("rtweet") 
    ## Load rtweet
    library("rtweet")

    ## Load tidyverse-collection, containing several packages, among
    ## them the package dplyr, and the package stringr with
    ## functions for cleaning text
    if(require("tidyverse")==FALSE) install.packages("tidyverse") 
    ## Load tidyverse
    library("tidyverse")
    
    ## install the {httpuv} package to authorize via web browser
    if(require("httpuv")==FALSE) install.packages("httpuv") 
    ## load httpuv package
    library("httpuv")
    
    if(require("devtools")==FALSE) install.packages("devtools") 
    library("devtools")
    
    if(require("readr")==FALSE) install.packages("readr") 
    library("readr")
    
    if(require("jsonlite")==FALSE) install.packages("jsonlite") 
    library("jsonlite")

    ## Package "tidytext" uses methods from packages NLP, tm, topicmodels
    ## -> Wraps needed function and is also 'tidy'!
    if(require("tidytext")==FALSE) install.packages("tidytext")
    library("tidytext")
    

    if(require("stringr")==FALSE) install.packages("stringr")
    library("stringr")
    
    ## Package tm contains functions for topic modelling utils
    if(require("tm")==FALSE) install.packages("tm")
    library("tm")
        
    # Package 'topicmodels' contains methods for doing general LDA-related task
    # and the actual topic modeling including fitting
    if(require("topicmodels")==FALSE) install.packages("topicmodels")
    library("topicmodels")
    
    if(require("NLP")==FALSE) install.packages("NLP")
    library("NLP")
    
    ## Package Rmpfr contains functions to handle a large number
    ## of decimal places, like they might occur in the representation of 
    ## the probabilities in a topic model
    if(require("Rmpfr")==FALSE) install.packages("Rmpfr")
    library("Rmpfr")
    
    ## 'slam'-package contains data structures and algorithms for sparse arrays 
    ## and matrices, based on index arrays and simple triplet representations, 
    ## respectively. The document-term-matrices constructed from Tweets are 
    ## extremely sparse, given that the documents are relatively short.
    if(require("slam")==FALSE) install.packages("slam")
    library("slam")

    ## Batch-Processing; This works by reading data from a specified file,
    ## that is the result of prior streaming; Default-value is 'tweets.json'
    if(batch_processing) {
      tweets <- rtweet::parse_stream(path = file_name, verbose = TRUE)
      users <- tweets
    } else {
      ## Verbosity might be of no use anymore; Language should be "en",
      ## others might occur for search-queries like "realdonaldtrump,wall,mexico" etc.
      ## To authenticate the user, an Oauth (Twitter API) token is needed - 
      ## in this case, the token from the global-environment is used;  
       tweets <- rtweet::stream_tweets(q = query, timeout = streamtime, 
                                  file_name = file_name, verbose = TRUE, 
                                  language = "en", token = token)
      users <- rtweet::users_data(tweets)
    }
    
    ## Package "tweetbotornot" still WIP and not on CRAN anymore; Caution is advised; 
    ## Demanding resource-wise; Prone to flooding and thus getting disconnected too 
    if(bot_detection) {
      
      # install remotes if not already
      if (!requireNamespace("remotes", quietly = TRUE)) {
        install.packages("remotes")
      }
      
      ## install textfeatures and the depending tweetbotornot from github
      if(require("textfeatures")==FALSE) devtools::install_github(
        "mkearney/textfeatures")
      library(textfeatures)
      
      if(require("tweetbotornot")==FALSE) devtools::install_github(
        "mkearney/tweetbotornot")
      library(tweetbotornot)
      
      ## Feed User_IDs to Tweetbotornot, get back User_IDs and prob of being bot
      data <- tweetbotornot::tweetbotornot(users$user_id, fast = TRUE)
      
      ## Threshold of a row/user being considered a bot
      threshold <- 0.50
      
      ## Filter which leaves only the users considered being a bot
      bots <- dplyr::filter(data, prob_bot > threshold)
      
      ## Delete bots from Dataset
      tweets <- dplyr::anti_join(tweets,bots,by="user_id")
      
    }
    
    ## If there are no Tweets for a query, just leave it
    if (!is.null(tweets)) {
      
      # Get path to working directory
      mainDir <- base::getwd()
      ## Create subfolder 'output' of working directory to store intermediary
      ## files like a snapshot of dtm and several parts of fitted topic model
      ## If there's already such a directory, just suppress the warning
      dir.create(file.path(mainDir, "output"), showWarnings = FALSE)
      ## Clear the folder, so that no obsolete data remains
      base::unlink("output/*")
      
      
      ## Just use the columns for text
      tweets <- dplyr::select(tweets, text)
      ##  Clean text to remove odd characters/emojs
      tweets$text <- base::sapply(tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
      ##  Replace hashtags with empty string
      tweets$text <- stringr::str_replace_all(tweets$text, "#[a-z,A-Z,0-9]*", "")
      ##  Replace user tags
      tweets$text <- stringr::str_replace_all(tweets$text, "@[a-z,A-Z,0-9]*", "")
      ## Replace URLs
      tweets$text <- stringr::str_replace_all(tweets$text, "http\\S+", "")
      ## Replace retweets 
      tweets$text <- stringr::str_replace_all(tweets$text, "RT @[a-z,A-Z,0-9]*", "")
      
      ## Keep non-empty rows/remove every row with an empty string
      tweets <- dplyr::filter(tweets, tweets$text != " ")
      
      ## The content of the tweets might be of interest later on 
      base::writeLines(jsonlite::toJSON(tweets, pretty = TRUE), 
                       "output/filtered_tweets.json")
      
      ## create Vector of the corpus
      tweetCorpus <- tm::VCorpus(VectorSource(tweets$text))
      
      ## 'Pull' results in a character-vector of extracted stopwords 
      ## from 'word'-col
      stop_words <- tidytext::get_stopwords(language = "en", source = "smart") %>% 
        dplyr::pull(word) 
      tweetCorpus <- tm::tm_map(tweetCorpus, removeWords, stop_words)
      
      ## Custom list of stopwords
      customStopwords <- c("can", "say","one","way","use",
                           "also","howev","tell","will",
                           "much","need","take","tend","even",
                           "like","particular","rather","said",
                           "get","well","make","ask","come","end",
                           "first","two","help","often","may",
                           "might","see","someth","thing","point",
                           "post","look","right","now","think","'ve",
                           "'re","anoth","put","set","new","good",
                           "want","sure","kind","larg","yes,","day","etc",
                           "quit","sinc","attempt","lack","seen","awar",
                           "littl","ever","moreov","though","found","abl",
                           "enough","far","earli","away","achiev","draw",
                           "last","never","brief","bit","entir","brief",
                           "great","lot","retweet")
      tweetCorpus <- tm::tm_map(tweetCorpus, removeWords, customStopwords)
      
      ## remove whitespaces
      tweetCorpus <- tm::tm_map(tweetCorpus, stripWhitespace)
      
      ## Do the remaining preprocesses and create DocumentTermMatrix,
      ## Only keep words of a length between 3 and 15 characters
      dtm <- tm::DocumentTermMatrix(tweetCorpus, control = list (
        tolower = TRUE, stopwords = TRUE, 
        removeNumbers = TRUE, removePunctuation = TRUE, 
        wordLengths = c (3, 15)))
      
      ## Tf-idf is a approach to filter out "unimportant" words from our text. This omit terms which have low frequency as well as those occurring in many documents.
      term_tfidf <- tapply(dtm$v/slam::row_sums(dtm)[dtm$i],
                           dtm$j, mean) * log2(nDocs(dtm)/slam::col_sums(dtm > 0))
      
      ## ensuring that the very frequent terms are omitted
      median_tfidf <- summary(term_tfidf)[3]
      dtm <- dtm[, term_tfidf >= median_tfidf]
      
      ## define a function for harmonic mean
      harmonicMeanCalc <- function(logLikelihoods, precision=2000L) {
        llMed <- Rmpfr::median(logLikelihoods)
        as.double(llMed - log(Rmpfr::mean(exp(-Rmpfr::mpfr(logLikelihoods,
                                                           prec = precision) + llMed))))
      }
      
      ## Find the sum of words in each Document
      rowTotals <- base::apply(dtm , 1, sum) 
      
      ## remove all docs without words
      dtm.new   <- dtm[rowTotals> 0, ]
      
      ## define the Values for Gibbs-Sampling
      burnin = 100
      iter = 100
      keep = 50
      
      ## Generate different values for k
      totalDocs <- as.integer(nrow(tweets))
      from <- minTopics
      interval <- 10
      to <- maxTopics
      if (totalDocs >= 1000) { 
        to <- as.integer(totalDocs/10)
      } 
      k_values <- base::seq(from, to, interval)
      

      
      ## run LDA for each of values of k
      fitted_many_models <- base::lapply(
        k_values, function(k) topicmodels::LDA(
          dtm.new, k = k, method = "Gibbs", control = list(
            burnin = burnin, iter = iter, keep = keep) ))
      
      ## extract loglikelihood from each topic
      logLiks_many <- base::lapply(fitted_many_models, 
                                   function(L) L@logLiks[-c(1:(burnin/keep))])
      
      # compute harmonic means
      hm_many <- base::sapply(logLiks_many, function(h) harmonicMeanCalc(h))
      
      #graphics.off()
      #par("mar")
      #plot(k_values, hm_many, type = "l")
      k <- k_values[which.max(hm_many)]
      
      ## run LDA using optimal value of k to have our final model
      seedNum <- as.integer(Sys.time())
      final_model <- topicmodels::LDA(dtm.new, k = k, method = "Gibbs", 
                                      control = list( alpha = 50/k,
                                                      burnin = burnin, iter = iter, keep = keep, seed=seedNum))
      
      #writing out results...
      
      print ("Prior-Number of topics(k): " + as.String(final_model@k))
      print ("Prior-Alpha: " + as.String(final_model@alpha  ))
      
      #write DocumentTermMatrix into csv
      utils::write.csv(as.matrix(dtm.new), 
                       file=paste("output/DocumentTermMatrix.csv"))
      
      #output: top n terms in each topic
      ldamodel.terms <- as.matrix(topicmodels::terms(final_model, 50))
      utils::write.csv(ldamodel.terms, 
                       file=paste("output/LDAGibbsTopicsToTermsMap.csv"))
      
      ## Note: Feeding the following parameters directly to LDAvis::createJSON
      ## and just exporting the .json might work just as well for the given usecase
      
      ## Constructing the data for the vizualisation from the fitted topic model
      ## Extension of the posterior-function of the topicmodels-package is needed 
      ## to get the probabilities of the posteriors (words/terms/tokens) 
      ## to be generated by the final model given the priors Alpha&Beta

      topicmodelVisualisation(final_model)
    }
  }

## Extract the relevant matrices from the Posteriors and write them to
## the 'output'-folder
topicmodelVisualisation <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop(
    "The model must contain at least 3 topics")
  
  ## Get the observations from the topic model
  mat <- x@wordassignments
  
  ## output: per-topic-per-word probabilities (beta)
  phi <- post[["terms"]]
  utils::write.csv(phi,file=paste(
    "output/LDAGibbsPerTopicPerWordProbabilities.csv"))
  
  ## output: per-document-per-topic probabilities
  theta <- post[["topics"]]
  utils::write.csv(theta, file=paste(
    "output/LDAGibbsPerDocumentPerTopicProbabilities.csv"))
  
  ## output: list of  words
  vocab <- colnames(post[["terms"]])
  utils::write.csv(vocab,
                   file=paste("output/LDAGibbsWords.csv"))
  
  ## Get the length of each 'De-stopworded' Tweet/Document
  doc.length <- slam::row_sums(mat, na.rm = TRUE)
  utils::write.csv(doc.length,
                   file=paste("output/DocumentLengths.csv"))
  
  ## Get the overall frequencies of all terms from DTM
  termFrequencies <- slam::col_sums(mat, na.rm = TRUE)
  utils::write.csv(termFrequencies,
                   file=paste("output/TermFrequencies.csv"))
}
