##Analysis functions

#The following functions take a file path as input and run analysis.
#The functions will install and run all the required packages
#TextAnalysis is designed for .txt files
#TwitterAnalysis is designed for Twitter data in .json format, it parses the tweets into data frame  

#The function exports into the working directory the following:
    #spreadsheet of the 25 most frequent words and their frequencies 
    #A graph word cloud of the top 150 most frequent words 
    #LDA key results (a spreadsheet of how frequently a word appears and the LDA topic it was assigned to)
    #N-grams (most frequent n-grams and a word cloud of them)
    #For Twitter data, the function outputs a time graph of when tweets were made in terms of time of day
    #It also outputs a sentiment map of tweets that are geo tagging enabled plotted onto a map with 
#sentiment represented by colour of point
    #A spreadsheet of text and sentiment is also outputted 

#Potential errors includes the following:
  #If the format is not right, it will be unable to run
  #file path needs to be specified if not working directors is set
  #Often on the word clouds a non-fatal error occurs as some of the words or phases may be to long to plot

#Example of how to run the function:
    #TextAnalysis("alldata.txt")
    #TwitterAnalysis("tweets4.json")

#For more explanation of the methods used for analysis, 
#see the following blog: https://data16blog.wordpress.com/


TwitterAnalysis <- function(FilePath){
  # install.packages("lda")
  # install.packages("tm")
  # install.packages("SnowballC")
  # install.packages("ggplot2")
  # install.packages("topicmodels")
  # install.packages("stringr")
  # install.packages("cluster")
  # install.packages("tokenizers")
  # install.packages("wordcloud")
  # install.packages("wordcloud2")
  # install.packages("slam")
  # install.packages("streamR")
  # install.packages("twitteR")
  # install.packages("RCurl")
  # install.packages("quanteda")
  # install.packages("mapdata")
  # install.packages("rworldmap")
  # install.packages("plyr")
  # install.packages("NLP")
  # install.packages("Matrix")
  # install.packages("MASS")
  # install.packages("syuzhet")
  # install.packages("RJSONIO")
  # install.packages("rjson")
  
  library(mapdata)
  library(rworldmap) 
  library(lda)
  library(SnowballC)
  library(tm)
  library(ggplot2)
  library(topicmodels)
  library(stringr)
  library(cluster)
  library(tokenizers)
  library(wordcloud)
  library(wordcloud2)
  library(slam)
  library(streamR)
  library(syuzhet)
  library(twitteR)
  library(RCurl)
  library(quanteda)
  library(plyr)
  library(NLP)
  library(Matrix)
  library(MASS)
  library(RJSONIO)
  library(rjson)
  
  ##Reads the file and assigns to file path, then makes it into a text corpus.
  tweetsN <- parseTweets(FilePath)
  text <- tweetsN$text
  
  ##Load the data as a corpus
  text  <- iconv(enc2utf8(text),sub="byte")
  text <- sapply(text, function(row) iconv(row, "latin1","ASCII", sub=""))
  docs <- Corpus(VectorSource(text))
  
  ##Are useful to see if the data has been read in properly without errors 
  inspect(docs)
  summary(docs)
  
  ##Text transformation; removes particular symbols, and changes them to a space instead. 
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  ##Clean text
  ##Convert the text to lower case (either command is fine)
  docs <- tm_map(docs, content_transformer(tolower))
  #docs <- tm_map(docs, tolower)
  
  ##Remove numbers
  docs <- tm_map(docs, removeNumbers)
  
  ##Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  ##Remove your own stop word
  ##specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("think", "t.co", "https", "thing", "get", "just", "can", "one", "lot", "got", "yeah", "bit", "say", "actual", "come", "year", "might", "probabl", "way", "done", "day", "kind", "will", "also", "back","you'r","you're")) 
  
  ##Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  
  ##Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  ##Text stemming
  docs <- tm_map(docs, stemDocument)
  
  ##Document term matrix, which describes the frequency of the words in the text
  tdm <- TermDocumentMatrix(docs)
  
  ##Sorts to give a list of most common words and their frequencies (mostly head displays this)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  MostFrequentWords <- as.data.frame(head(v, 25))
  colnames(MostFrequentWords) <- "Frequency"
  write.csv(MostFrequentWords, file="MostFrequentWords.csv")
  
  ##Makes word cloud
  #'min.freq' is miminum of words requires to be on word cloud
  #'max.words' is how many words at most wanted for the word cloud 
  #'#colours can also be changed on the 'colors=' parameter
  pdf("WordCloud.pdf") 
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=150, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
  dev.off()
  
  ##LDA with a Gibbs sampler 
  #Can change how many ittereations by adjusting 'iter'
  #Can change how many topics for the data to be explained by
  burnin <- 4000
  iter <- 2000
  thin <- 500
  seed <-list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE
  ##Number of topics
  k <- 10
  ##Run LDA using Gibbs sampling
  ldaOut <-LDA(tdm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  
  ##Write out results of LDA
  ##Gives output for which topic each word belongs to 
  ldaOut.topics <- as.matrix(topics(ldaOut))
  
  ##This sorts the words so they are grouped by topic rather than being plainly alphabetical 
  ldaOut.topics.sorted <- sort(ldaOut.topics[,1],decreasing=TRUE)
  
  lda.topics.sorted <- cbind(rowSums(m), ldaOut.topics)
  lda.topics.sorted <- as.data.frame(lda.topics.sorted)
  colnames(lda.topics.sorted) <- c("Frequency","Topic Number")
  write.csv(lda.topics.sorted, file="ldatopicssorted.csv")
  
  ##bi/tri-grams
  ##Toeknis with quanteda 
  ##Reads the file and assigns to file path
  ##replace the stop words part with the following to remove your own stop works...   ignoredFeatures=c("","",stopwords("english")) 
  
  token2dfm <- dfm(text, toLower=TRUE, removeNumbers=TRUE, removePunct = TRUE, removeSeparators=TRUE, removeTwitter=TRUE, stem=TRUE, ignoredFeatures=c("t.co","rt","https",stopwords("english")), ngrams=2)
  t2 <- topfeatures(token2dfm,20)
  t2 <- as.data.frame(t2)
  colnames(t2) <- "Frequency"
  write.csv(t2, file="2gramsTOP20.csv")
  pdf("2-grams.pdf")
  plot(token2dfm, max.words=100, colors=brewer.pal(8, "Dark2"))
  dev.off()
  
  token3dfm <- dfm(text, toLower=TRUE, removeNumbers=TRUE, removePunct = TRUE, removeSeparators=TRUE, removeTwitter=TRUE, stem=TRUE, ignoredFeatures=c("t.co","rt","https",stopwords("english")), ngrams=3)
  t3 <- topfeatures(token3dfm,20)
  t3 <- as.data.frame(t3)
  colnames(t3) <- "Frequency"
  write.csv(t3, file="3gramsTOP20.csv") 
  pdf("3-grams.pdf")
  plot(token3dfm, max.words=100, colors=brewer.pal(8, "Dark2"))
  dev.off()
  
  token4dfm <- dfm(text, toLower=TRUE, removeNumbers=TRUE, removePunct = TRUE, removeSeparators=TRUE, removeTwitter=TRUE, stem=TRUE, ignoredFeatures=c("t.co","rt","https",stopwords("english")), ngrams=4)
  t4 <- topfeatures(token4dfm,20)
  t4 <- as.data.frame(t4)
  colnames(t4) <- "Frequency"
  write.csv(t4, file="4gramsTOP20.csv") 
  pdf("4-grams.pdf")
  plot(token4dfm, max.words=100, colors=brewer.pal(8, "Dark2"))
  dev.off()
  
  ##Put the chosen word into context by displaying where it appears in the text 
  #kwic(text, "")
  
  ##Time Plots
  
  a <- tweetsN$created_at
  b <- substr(a, 0, 3) #day
  c <- as.numeric(substr(a, 9, 10)) #date
  d <- as.numeric(paste(substr(a, 12,13),substr(a, 15,16), sep=""))#time
  hist(d) #Use to find most popular time to tweet
  
  e <- as.data.frame(table(d,c))
  pdf("TimeTweets.pdf")
  plot(table(d), type="l", main="Most popular time to tweet", xlab="Time", ylab="Frequency") #Use to find what time tweets are made on ech day
  dev.off() 
  #plot.ts(e$Freq)
  
  ##Sentiment 
  #sen <- get_sentiment(text, method="syuzhet")
  #sen <- get_sentiment(text, method="bing")
  sen <- get_sentiment(text, method="afinn") #Developed for Twitter tweets
  #sen <- get_sentiment(text, method="nrc")
  
  sen <- as.data.frame(sen)
  sen <- 5*sen/(max(max(sen),abs(min(sen))))
  
  SenAndText <- cbind(sen,text)
  SenAndText <- as.data.frame(SenAndText)
  colnames(SenAndText) <- c("Sentiment","Text")
  write.csv(SenAndText,"SenAndText.csv")
  
  ##Geo mapping
  place <- tweetsN[37:38]
  newmap <- getMap(resolution="high")
  
  ##Map with points for sentiment
  pdf("SenMap.pdf")
  #plot(newmap, xlim=c(-6,0), ylim=c(50,60), asp=1) #UK map
  plot(newmap, xlim=c(-10,10), ylim=c(30,70), asp=1) #Europe ish
  #plot(newmap, xlim=c(,), ylim=c(,), asp=1) #World map
  
  ##Plots points onto map
  points(place$place_lon, place$place_lat, col=heat.colors(sen$sen+6), pch=20)
  legend("topright",title="Sentiment",legend=c("Negative", "Neutral","Positive"),fill=c("red","orange","yellow"))
  dev.off()
}

TextAnalysis <- function(FilePath){
  install.packages("lda") 
  install.packages("tm")
  install.packages("SnowballC") 
  install.packages("ggplot2")
  install.packages("topicmodels")
  install.packages("stringr") 
  install.packages("cluster")
  install.packages("tokenizers") 
  install.packages("wordcloud") 
  install.packages("wordcloud2") 
  install.packages("slam") 
  install.packages("quanteda")
  install.packages("plyr")
  install.packages("NLP")
  install.packages("Matrix") 
  install.packages("MASS")
  install.packages("syuzhet")

  library(lda)
  library(SnowballC)
  library(tm)
  library(ggplot2)
  library(topicmodels)
  library(stringr)
  library(cluster)
  library(tokenizers)
  library(wordcloud)
  library(wordcloud2)
  library(slam)
  library(syuzhet)
  library(quanteda)
  library(plyr)
  library(NLP)
  library(Matrix)
  library(MASS)

  ##Reads the file and assigns to file path, then makes it into a text corpus.
  #tweets <- parseTweets(filePath)
  #text <- tweets$text
  
  text <- readLines(FilePath)
  ##Load the data as a corpus
  text  <- iconv(enc2utf8(text),sub="byte")
  text <- sapply(text, function(row) iconv(row, "latin1","ASCII", sub=""))
  docs <- Corpus(VectorSource(text))
  
  ##Are useful to see if the data has been read in properly without errors 
  inspect(docs)
  summary(docs)
  
  ##Text transformation; removes particular symbols, and chages them to a space instead. 
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  ##Clean text
  ##Convert the text to lower case (either command is fine)
  docs <- tm_map(docs, content_transformer(tolower))
  #docs <- tm_map(docs, tolower)
  
  ##Remove numbers
  docs <- tm_map(docs, removeNumbers)
  
  ##Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  ##Remove your own stop word
  ##specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("think", "t.co", "https", "thing", "get", "just", "can", "one", "lot", "got", "yeah", "bit", "say", "actual", "come", "year", "might", "probabl", "way", "done", "day", "kind", "will", "also", "back","you'r","you're")) 
  
  ##Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  
  ##Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  ##Text stemming
  docs <- tm_map(docs, stemDocument)
  
  ##Document term matrix, which describes the frequency of the words in the text
  tdm <- TermDocumentMatrix(docs)
  
  ##Sorts to give a list of most common words and their frequencies (mostly head displays this)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  MostFrequentWords <- as.data.frame(head(v, 25))
  colnames(MostFrequentWords) <- "Frequency"
  write.csv(MostFrequentWords, file="MostFrequentWords.csv")
  
  ##Makes word cloud
  #'min.freq' is miminum of words requires to be on word cloud
  #'max.words' is how many words at most wanted for the word cloud 
  #'#colours can also be changed on the 'colors=' parameter
  pdf("WordCloud.pdf") 
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=150, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
  dev.off()
  
  ##LDA with a Gibbs sampler 
  #Can change how many ittereations by adjusting 'iter'
  #Can change how many topics for the data to be explained by
  burnin <- 4000
  iter <- 2000
  thin <- 500
  seed <-list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE
  ##Number of topics
  k <- 10
  ##Run LDA using Gibbs sampling
  ldaOut <-LDA(tdm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  
  ##Write out results of LDA
  ##Gives output for which topic each word belongs to 
  ldaOut.topics <- as.matrix(topics(ldaOut))
  
  ##This sorts the words so they are grouped by topic rather than being plainly alphabetical 
  ldaOut.topics.sorted <- sort(ldaOut.topics[,1],decreasing=TRUE)
  
  lda.topics.sorted <- cbind(rowSums(m), ldaOut.topics)
  lda.topics.sorted <- as.data.frame(lda.topics.sorted)
  colnames(lda.topics.sorted) <- c("Frequency","Topic Number")
  write.csv(lda.topics.sorted, file="ldatopicssorted.csv")
  
  ##bi/tri-grams
  ##Toeknis with quanteda 
  ##Reads the file and assigns to file path
  ##replace the stop words part with the following to remove your own stop works...   ignoredFeatures=c("","",stopwords("english")) 
  
  token2dfm <- dfm(text, toLower=TRUE, removeNumbers=TRUE, removePunct = TRUE, removeSeparators=TRUE, removeTwitter=TRUE, stem=TRUE, ignoredFeatures=c("t.co","rt","https",stopwords("english")), ngrams=2)
  t2 <- topfeatures(token2dfm,20)
  t2 <- as.data.frame(t2)
  colnames(t2) <- "Frequency"
  write.csv(t2, file="2gramsTOP20.csv")
  pdf("2-grams.pdf")
  plot(token2dfm, max.words=100, colors=brewer.pal(8, "Dark2"))
  dev.off()
  
  token3dfm <- dfm(text, toLower=TRUE, removeNumbers=TRUE, removePunct = TRUE, removeSeparators=TRUE, removeTwitter=TRUE, stem=TRUE, ignoredFeatures=c("t.co","rt","https",stopwords("english")), ngrams=3)
  t3 <- topfeatures(token3dfm,20)
  t3 <- as.data.frame(t3)
  colnames(t3) <- "Frequency"
  write.csv(t3, file="3gramsTOP20.csv") 
  pdf("3-grams.pdf")
  plot(token3dfm, max.words=100, colors=brewer.pal(8, "Dark2"))
  dev.off()
  
  token4dfm <- dfm(text, toLower=TRUE, removeNumbers=TRUE, removePunct = TRUE, removeSeparators=TRUE, removeTwitter=TRUE, stem=TRUE, ignoredFeatures=c("t.co","rt","https",stopwords("english")), ngrams=4)
  t4 <- topfeatures(token4dfm,20)
  t4 <- as.data.frame(t4)
  colnames(t4) <- "Frequency"
  write.csv(t4, file="4gramsTOP20.csv") 
  pdf("4-grams.pdf")
  plot(token4dfm, max.words=100, colors=brewer.pal(8, "Dark2"))
  dev.off()
  
  ##Put the chosen word into context by displaying where it appears in the text 
  #kwic(text, "")
  
  ##Sentiment 
  #sen <- get_sentiment(text, method="syuzhet")
  #sen <- get_sentiment(text, method="bing")
  sen <- get_sentiment(text, method="afinn") #Developed for Twitter tweets
  #sen <- get_sentiment(text, method="nrc")
  
  SenAndText <- cbind(sen,text)
  SenAndText <- as.data.frame(SenAndText)
  colnames(SenAndText) <- c("Sentiment","Text")
  write.csv(SenAndText,"SenAndText.csv")
}