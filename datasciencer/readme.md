###StopMoVis: Streaming Topic Model and Visualization for Twitter



-------------------------------------------
###Process Notebook: 

The [process.Rmd](https://gitlab.com/mehedicoder/datasciencer/blob/master/process.Rmd) describes the course of the project from the outset. This also includes deviations from the initially proposed structure and further information about the resulting prototypical application. 

###Website:

The website can be found at [https://sites.google.com/view/stopmovis/home](https://sites.google.com/view/stopmovis/home).

###Screencast: 

The screencast can be found embedded on the website mentioned above or can be accessed directly on [YouTube](https://youtu.be/ijLyTEbeHwU).

###Demo:

A restricted version of the final application can be found online at [StopMoVis on shinyapps.io](https://stopmovisapp.shinyapps.io/datasciencer/); This version only allows for the use of a test dataset.

###Repository on Gitlab 

The repository at [StopMoVis on GITLAB](https://gitlab.com/mehedicoder/datasciencer) is structured as follows:

####Main folder:

This folder contains, apart from this 'readme'-file and process notebook, the core files of the application:

* shinyLDAvis.R - This is, as the name implies, the combined 'ShinyApp' containing both the ui- and the server-component necessary for any Shiny-application. The parameters input into the GUI get submitted to the backend-component, which in turn generates the files needed for vizualisation by the GUI. 
* TwitterLDA.R -  This file represents the 'backend' of the application; It contains the core logic for streaming Tweets, cleaning and processing them, fitting the topic model and generating outputs that serve as a basis for vizualisation.  
* tweets.json - This file is optional; it contains the Tweets of the last streaming; A reference dataset gets delivered when the repository gets cloned 

####Supplementary Materials:

* shinyappsDemo: The source-files for the 'shinyapps.io'-version can be found in this folder. The files 'ui.R' and 'server.R' are based on the 'shinyLDAvis.R'-file.
* visualisations: This folder contains further source-files for creating some intermediary vizualistions.
* data: This folder contains the reference dataset 'tweets.json'; it contains 1044 tweets, and was streamed on the 12th January '19 using a streamtime of 60 seconds and the searchwords 'brexit,may,vote'.
* literature: This folder contains the most relevant papers in the context of this application.
* proposal: This folder contains the files of the initial proposal.
* output: This folder gets generated during the first run of the application and holds the output files of the respective  last run; This folder gets wiped.   

###Getting started: Installation:

To start the local, non-restricted version, the following steps are necessary: 

* Clone Gitlab-Repository [StopMoVis on GITLAB](https://gitlab.com/mehedicoder/datasciencer) to a local folder. Apart from the necessary source and optional auxilliary files, there's also a reference dataset in the main folder, 'tweets.json' coming along. The same dataset can also be found in the 'data' subfolder.
* Run 'shinyLDAvis.R' from the folder to start the shiny application.
* If no global Twitter-token is available, a browser-window should open to receive the credentials (a regular Twitter-account is sufficient); further information can be obtained by executing the following code 'vignette("auth", "rtweet")'
* During the first run, the example 'tweet.json'-file is used as per default. Be aware that this file gets overwritten during each time in case of streaming from Twitter.com instead of using the option 'Cached Tweets?' - the same goes for the temporary files in the 'output'-folder which gets generated if not already existing.   
* A short video that gives an overview of the application can also be accessed at [Overview](https://www.youtube.com/watch?v=U7yfVk1cJeI)
