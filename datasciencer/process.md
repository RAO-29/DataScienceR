---
title: "The Process Notebook"
output:
  html_document:
    keep_md: true
---
##Project Title: Streaming Topic Model and Visualization for Twitter.


###Supervisor: Uli Niemann, KMD Research Group, OVGU 

###The Team:

 Daniel Bokelmann | Rajatha Nagaraja Rao | Mehedi Hasan
------------- | ------------  | ------------
Email: daniel.bokelmann@st.ovgu.de | rajatha1@st.ovgu.de | md.hasan@st.ovgu.de
MSc. Program: DKE  | MSc. Program: DKE  | MSc. Program: DKE

###Overview and motivation:
The topic model approach has been used widely in the field of computer science focusing on web and text-mining in recent years. The goal of topic modeling is to automatically discover the topics from a collection of documents. When a corpus of documents is large, how can a single person understand what’s going on in a collection of large number of documents? This is a growingly common problem: navigating through an organization’s documents, emails, tweets from twitter, understanding a decade worth of newspapers, or characterizing a scientific field’s research. Researchers seek for automated analysis and visualization tools to produce a high-level overview of a large dataset. Topic models are a statistical framework that help users understand large document collections: not just to find individual documents but to understand the general themes present in the collection. In addition, topic models could be effectively applied to solve traditional problems in the fields of information retrieval, visualization, statistical inference, multilingual modeling, reducing dimension and linguistic understanding. Thus, topic models unlock large text collections for explorative and qualitative analysis.

###Related Work:
The Latent Dirichlet allocation (LDA) is a topic modeling technique, proposed by [Blei et al., 2003] is a probabilistic generative model. LDA emphasizes on hidden (latent) topic structure(topics), per document topic distributions, and per topic word distributions. It represents each unit (e.g., a document) of a textual collection by an infinite mixture of probability over an underlying set of latent topics (concepts); each token (e.g., a word) of a document is associated with some of these latent topics. In 2011, [Grun et al., (2011)] published topicmodels paper with implementation in R, this package is known as topicmodels in CRAN. To visualize the fitted model [Sievert et al., (2014)] provided a way to visualize the topicmodel using web interface; known as LDAvis in CRAN. Nowadays, there are a growing number of probabilistic models that are based on state-of-the art LDA via combination with particular tasks; variations of LDA. [Rosen-Zvi et al., 2004] proposed the author-topic-model(ATM) which extends LDA to include authorship information, modelling the interests of authors. [Ramage et al., 2009] introduced Labeled-LDA for labeled social bookmarking websites data which is a topic model that constrains LDA by defining a one-to-one correspondence between LDA’s latent topics and user tags. [Zhao et al., 2011] published Twitter-LDA which is an abstraction of LDA to discover topics from short text. Nonetheless, LDA based topic models have initially been introduced in the text analysis community for unsupervised topic discovery in a corpus of docments, still serving as state of the art topic modelling technique.

Twitter is a popular micro-blogging service [Kwak et al., 2010], an important text analysis problem domain. In the last few years, twitter has become a growingly popular platform for internet users. Users share ideas and opinions with each other, respond to recent events, share breaking news via twitter messages (tweets). Tweet consists of maximum 140 characters [Boyd et al., 2010], recently that changed to 280. Although tweets are short but useful because tweets are compact and fast, can potentially contain much information [Zhao et al., 2011]. In the past years, several studies have been conducted on Twitter from different perspectives. [Kwak et al., 2010] studied the topological characteristics of tweets in work settings and concluded that useful informal conversations among colleagues could be done more intuitively via tweets. [Sakaki et al., 2010] considered tweets as social sensors of realtime events, each Twitter user as a sensor and detected earthquake based on user’s tweet. [Asur and Huberman, 2010] demonstrated that the social media content can be analyzed to predict realworld outcomes. The author used "chatter"" from Twitter.com to forecast box-office revenues for movies. [Tumasjan et al., 2010] predicted the German federal election of the national parliament which took place on September 27th, 2009. [Khan et al., 2014] proposed a hybrid approach for determining the sentiment of each tweet. [Alvarez-Melis and Saveski, 2016] proposed a topic modeling approach for Twitter by grouping tweets together that occurs in the same user-to-user conversation; tweets and their replies were aggregated into a single document and the users who posted them are considered co-authors. [Soleymani et al., 2017] analyzed in response to advertisement videos to predict their purchase.

###Intitial Questions:
 1. Researchers seek for single click interface for automated training, evaluation and generating intuitive visualization of the generated topics. No such open tool available out there.
 
 2. Automated estimation of hyper parameters in a data-driven-way because all topic modelling techniques are parametric including LDA.
 
###How Questions Evolved:
 1. What would be the lower and upper limit of k value?
 
 2. How to estimate the parameters that best approximates the underlying distribution (of words)?

 3. Which algorithm to use to generate the model? Variational Expectation Maximization(VEM), Gibbs Sampler.

 4. Which evaluation technique to use? Harmonic mean, Perplexity
 
 5. How to vizualise Topics after fitting a model? LDAvis, Termite, WordCloud, Starburst, Sankay.
 
###Data:
 As we wanted to do topic modelling for very recent dataset collected from Twitter.com, we wrote a twitter streamer using R to stream tweets. We provided a web interface so that user can write own query and stream tweets based on a number of keywords i.e. 'berlinwall','angelamerkel','union' etc. for configurable amount of time. Scraping is done by Twitter-API (with Dev-Account) and rtweet package. Pre-processing like removing numbers, special symbols, bots (at the outset), URLs, Retweets, stopwords and emojis are being done on the fly. Also, very infrequent and very frequent words. Final topicModel and other intermediate important artifacts are being stored as csv-files because csv files are easy to use. Tweetbotornot-package is still not stable. That is why bot tweet detection fails if we stream for more than 5 seconds. Its an unresolved issue.
 
###Exploratory data analysis:

As a topic modelling technique we used LDA. There are other topic modelling technique available for example: Hierarchical Dirichlet Process (HDP). But we choose LDA because LDA is proven to be good performant for tweets [Weng et al.,].

The initial algorithm to generate the topic model (for training) was VEM. Because of poor performance and recommendation from scientific articles we had to switch to Gibbs sampling. Gibbs sampling functions by performing a random walk in such a way that reflects the characteristics of a desired distribution. Because the starting point of the walk is chosen at random, it was necessary to discard the first few steps of the walk as these do not correctly reflect the properties of distribution. This is referred to as the burn-in period. We set the burn-in parameter to 100. Following the burn-in period, we perform 100 iterations. The reason we do this is to avoid correlations between samples. Finally we’ve set best to TRUE (actually a default setting), which instructs the algorithm to return results of the run with the highest posterior probability.

The number of topic (k) value should be at least 2 because topic modelling mostly likely to generate at least 2 topics and at most total number of documents in the collection. Regarding the general question of finding optimal topic numbers (k), we followed the example of [Martin Ponweiser](http://epub.wu.ac.at/3558/1/main.pdf) on Model Selection by cross validation (CV). We needed an evaluation metric for that. There are several metrics available to measure goodness-of-fit of the topic model, for example: harmonic mean, perplexity. The goodness-of-fit of topic model mostly depends on human understanding of the generated topics. To be able to evaluate the model perfectly, it has to be checked by human observer. As our purpose was only selecting best parameter values automatically and perplexity would not bring any special finding, so we used harmonic mean over perplexity as evaluation technique because it was straight forward to integrate to our codebase. We tried different values for k for the mentioned range and choose the best one which maximizes the harmonic mean value. The prior alpha was set to 50/k based on [Griffiths and Steyvers, 2004] whcih is a dafault setting of topicmodels package.

To visualize the final topic model we used LDAvis package which is available in CRAN, robust, well-documented and easy to integrate with other topic modelling as well as other helper CRAN packages. However, this package is sophisticated which means customizing UI components are not straight forward. It requires expertise in JavaScript reactive programming skill. As our goal was generating different views of the data so that users can interpret built topic model easily; using LDAvis was helpful for that.

We created an end-to-end tool which is an interactive web-based visualization of a topic model that has been fit to a corpus of tweet data using Latent Dirichlet Allocation (LDA). It computes various summary statistics as input to an interactive visualization built with D3.js that is accessed via a browser. User can view generated topic map and terms (words) it consists of along with their distributions. For example, we deployed the visualization of the tweets streamed based on the keywords "brexit,may,vote". We can view configurable number of most relevant terms for generated topics using a relevance setting of λ, the widths of the red and blue bars indicate that there is at least one other topic in which that term appears frequently. By hovering over a particular term, we can see that it also appears frequently in other topics.

https://stopmovisapp.shinyapps.io/datasciencer/#topic=0&lambda=0.5&term=

Also, we are able to compare topics. Comparing two topics, we can see the different main contexts of each topic.

###Final Analysis:

Topic modelling partitions documents into topics, which in turn have terms associated to varying degrees. However we observed that the models are very sensitive to the input data small changes to the pre-processing, learning algorithms can result in completely different topics; topics need to be manually categorised in order to be useful (often arbitrarily, as the topics often contain mixed content). And also topics are "unstable" in the sense that using same set of paramaters and same corpus, different runs yields completely different topics.

###Further Improvements:
#####We can further improve the scalability of the application by 
1. Scale-up utilizing multi-core processors for single node.
2. Scale-out using public/private/hybrid cloud kubernetes cluster

###References:
[Kwak et al., 2010] Kwak, H., Lee, C., Park, H., and Moon, S. (2010). What is Twitter, a SocialNetwork or a News Media? InProceedings of the 19th International Conference on World WideWeb, WWW ’10, pages 591–600, New York, NY, USA. ACM.

[Boyd et al., 2010] Boyd, D., Golder, S., and Lotan, G. (2010). Tweet, Tweet, Retweet: Conver-sational Aspects of Retweeting on Twitter. In2010 43rd Hawaii International Conference onSystem Sciences, pages 1–10.

[Zhao et al., 2011] Zhao, W. X., Jiang, J., Weng, J., He, J., Lim, E.-P., Yan, H., and Li, X.(2011). Comparing Twitter and Traditional Media Using Topic Models. In Clough, P., Foley,C., Gurrin, C., Jones, G. J. F., Kraaij, W., Lee, H., and Mudoch, V., editors,Advances inInformation Retrieval, pages 338–349, Berlin, Heidelberg. Springer Berlin Heidelberg.

[Rosen-Zvi et al., 2004] Rosen-Zvi, M., Griffiths, T., Steyvers, M., and Smyth, P. (2004).  TheAuthor-topic Model for Authors and Documents.  InProceedings of the 20th Conference onUncertainty in Artificial Intelligence, UAI ’04, pages 487–494, Arlington, Virginia, United States.AUAI Press.

[Sakaki et al., 2010] Sakaki, T., Okazaki, M., and Matsuo, Y. (2010). Earthquake Shakes TwitterUsers: Real-time Event Detection by Social Sensors. InProceedings of the 19th InternationalConference on World Wide Web, WWW ’10, pages 851–860, New York, NY, USA. ACM.

[Asur and Huberman, 2010] Asur, S. and Huberman, B. A. (2010). Predicting the Future withSocial Media. InProceedings of the 2010 IEEE/WIC/ACM International Conference on WebIntelligence and Intelligent Agent Technology - Volume 01, WI-IAT ’10, pages 492–499, Wash-ington, DC, USA. IEEE Computer Society.

[Tumasjan et al., 2010] Tumasjan, A., Sprenger, T. O., Sandner, P. G., and Welpe, I. M. (2010).Predicting Elections with Twitter: What 140 Characters Reveal about Political Sentiment. InICWSM.

[Khan et al., 2014] Khan, F. H., Bashir, S., and Qamar, U. (2014). TOM: Twitter opinion miningframework using hybrid classification scheme.Decision Support Systems, 57:245–257.

[Alvarez-Melis and Saveski, 2016] Alvarez-Melis, D. and Saveski, M. (2016). Topic Modeling inTwitter: Aggregating Tweets by Conversations. InICWSM.

[Soleymani et al., 2017] Soleymani, M., Schuller, B., and Chang, S.-F. (2017).  Guest editorial:Multimodal sentiment analysis and mining in the wild.Image and Vision Computing, 65:1–2.

[Blei et al., 2003] Blei, D. M., Ng, A. Y., and Jordan, M. I. (2003). Latent Dirichlet Allocation.J. Mach. Learn. Res., 3:993–1022.

[Ramage et al., 2009] Ramage, D., Hall, D., Nallapati, R., and Manning, C. D. (2009). LabeledLDA: A Supervised Topic Model for Credit Attribution in Multi-labeled Corpora. InProceedingsof the 2009 Conference on Empirical Methods in Natural Language Processing: Volume 1 -Volume 1, EMNLP ’09, pages 248–256, Stroudsburg, PA, USA. Association for ComputationalLinguistics.

[Grun et al., (2011)] topicmodels: An R package for ﬁtting topic models. Journal of Statistical Software, 40(13).

[Sievert et al., (2014)] LDAvis: A method for visualizing and interpreting topics, Proceedings of the Workshop on Interactive Language Learning, Visualization, and Interfaces, pages 63–70, Baltimore, Maryland, USA, June 27, 2014. c 2014 Association for Computational Linguistics

[Griffiths and Steyvers, 2004] “Finding Scientific Topics.” Proceedings of the National
Academy of Sciences of the United States of America, 101, 5228–5235.

[Weng et al.,] TwitterRank: Finding topic-sensitive influential twitterers. In Proc. Int. Conf. on Web Search and Data Mining, pages 261–270, 2010.
