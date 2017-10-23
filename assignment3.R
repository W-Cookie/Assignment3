#The tidy data was stored in csv formati and was loaded into R using "read_csv" command
library(readr)
GA_SNAPstores_2008_2016_GeoID <- read_csv("D:/Dropbox/course/INFO 8000/Assignment3/GA_SNAPstores_2008_2016_GeoID.csv")
#View teh first 5 rows of the data
head(GA_SNAPstores_2008_2016_GeoID,n=5)

#Map visualization
library(leaflet)


#The function itself just shows a blank map
#The %>% separator is often used to divide commands within functions
leaflet() %>% addTiles()


#You can even define and add popups
map_popup <- paste0("<strong>Store Name: </strong>", 
                    GA_SNAPstores_2008_2016_GeoID$NAME,
                    "<br/><strong>Store Type: </strong>",
                    GA_SNAPstores_2008_2016_GeoID$STTYPE)

meanX<-mean(GA_SNAPstores_2008_2016_GeoID$LONG)
meanY<-mean(GA_SNAPstores_2008_2016_GeoID$LAT)


#Map showing all SNAP authorized stores from 2008 to 2016,with markerCluster options
leaflet() %>% 
  addTiles() %>% 
  setView(meanX, meanY,zoom=7) %>%
  addCircleMarkers(GA_SNAPstores_2008_2016_GeoID$LONG,
                   GA_SNAPstores_2008_2016_GeoID$LAT,
                   radius = 0.5,
                   color = "#03F",
                   popup=map_popup,clusterOptions = markerClusterOptions())
  
  #enable maekerCluster 
  addCircleMarkers(GA_SNAPstores_2008_2016_GeoID$LONG,
                  GA_SNAPstores_2008_2016_GeoID$LAT,
                  radius = 0.5,
                  color = "#03F",
                  popup=map_popup,clusterOptions = markerClusterOptions())

  #word cloud
  library(tm)
  
  library(wordcloud)
  
  library(RColorBrewer)

  
  storename_corpus = Corpus(VectorSource(GA_SNAPstores_2008_2016_GeoID$NAME))
  
  tdm = TermDocumentMatrix(storename_corpus,control = list(removePunctuation = TRUE,
                                                      stopwords = c("supermarket","shop","food","foods","mart","grocery","store","stop","llc","market","inc", stopwords("english")),
                                                      removeNumbers = TRUE, toupper = TRUE))
  
  # define tdm as matrix
  
  m = as.matrix(tdm)
  
  # get word counts in decreasing order
  
  word_freqs = sort(rowSums(m), decreasing=TRUE) 
  
  # create a data frame with words and their frequencies
  
  dm = data.frame(word=names(word_freqs), freq=word_freqs)
  
  wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
  

  
  
  
library(ggplot2)
  ggplot(GA_SNAPstores_2008_2016_GeoID,aes(x=GA_SNAPstores_2008_2016_GeoID$STTYPE))+
    xlab("Store types")+
    ggtitle("Distribution of SNAP-authorized store types")+
    geom_bar(,stat = "count",width = 0.5)+coord_flip()+
    theme_bw()
  
  

  
