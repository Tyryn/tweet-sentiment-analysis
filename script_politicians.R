library(tidyverse)
library(rtweet)
library(ggplot2)
library(tidytext)
library(gridExtra)
library(wordcloud)
library(tm)
library(webshot)
library(syuzhet)
library(wordcloud2)


# Function to pre-process text ####
clean.text = function(x) {
  # convert to lower case
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # some other cleaning text
  x = gsub('https://','',x)
  x = gsub('http://','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}


# Get all Tweets that mention the following politicians
# Helen Zille, Ramaphosa, Malema

politicians <- c("Zille", "Ramaphosa", "Malema")

tweet_list <- lapply(politicians, function(x) search_tweets(x,
  retryonratelimit = TRUE, n=18000
))


tweet_list_preprocess <- lapply(tweet_list, function(x){
  df <- x %>%
    mutate(text = clean.text(text))
  return(df)
})
# Name the list elements
names(tweet_list_preprocess) <- politicians

# Number of Tweets per hour ####
tweet_list_time <- lapply(tweet_list_preprocess, function(x){
  df <- x %>%
    mutate(Created_at_round = round.POSIXt(created_at, units = "hours"))
  return(df)
})

# Below to create single dataframe
tweet_df <- Map(cbind, tweet_list_time, Politician = names(tweet_list_time)) %>% 
  bind_rows(.) 

# Save dataframes so I don't have to do this again
saveRDS(tweet_list_preprocess, file="tweet_list_preprocess.RData")
saveRDS(tweet_list_time, file="tweet_list_time.Rdata")

# Below for the graph
tweet_df_hour <- tweet_df %>% 
  group_by(Politician) %>% 
  dplyr::count(Created_at_round)

tweets_per_hour <- lapply(politicians, function(x){
  df <- tweet_df_hour %>% 
    filter(Politician==x) %>% 
    mutate(total_hours = nrow(.)) %>% 
    mutate(total_tweets = sum(n)) %>% 
    mutate(mean_tweets = round(total_tweets/total_hours)) %>% 
    slice(1) %>% 
    select(mean_tweets)
  return(df[1,2])
})



# Plot time of tweets
plot <- ggplot(tweet_df_hour, aes(x=Created_at_round, y=n)) +
  theme_light() +
  xlab("Time") + ylab(NULL) +
  ggtitle("Number of Twitter mentions per hour") +
  geom_line(aes(x=Created_at_round, y=n, color=Politician))

plot

# Sentiment scoring function ####
score.sentiment = function(sentences, pos.words, neg.words, .progress='none'){
  require(plyr)
  require(stringr)
  
  # we are giving vector of sentences as input. 
  # plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub() function:
    sentence = gsub('https://','',sentence)
    sentence = gsub('http://','',sentence)
    sentence = gsub('[^[:graph:]]', ' ',sentence)
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = str_replace_all(sentence,"[^[:graph:]]", " ")
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}



# Get the sentiment scores #### 
# Using the Opinion Lexicon from Hu and Liu

# Create positive and negative score list of words
pos.words <- sentiments[sentiments$sentiment=="positive",1][[1]]
neg.words <- sentiments[sentiments$sentiment=="negative",1][[1]]

sentiment_df <- score.sentiment(tweet_df$text, pos.words, neg.words)


# Split by politician
sentiment_zille <- score.sentiment(tweet_df[tweet_df$Politician=="Zille",]$text, pos.words, neg.words)
sentiment_ramaphosa <- score.sentiment(tweet_df[tweet_df$Politician=="Ramaphosa",]$text, pos.words, neg.words)
sentiment_malema <- score.sentiment(tweet_df[tweet_df$Politician=="Malema",]$text, pos.words, neg.words)


# Graph the sentiment scores
sentiment_graph_ramaphosa <- sentiment_ramaphosa %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "#FFCC00")+ 
  scale_x_continuous(breaks = seq(-8, 8, 2)) +
  ylab("Number of tweets") + 
  xlab("Sentiment score") +
  ggtitle("Ramaphosa") +
  theme_light() 

sentiment_graph_zille <- sentiment_zille %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "#015A9C")+ 
  scale_x_continuous(breaks = seq(-8, 8, 2)) +
  ylab("Number of tweets") + 
  xlab("Sentiment score") +
  ggtitle("Zille") +
  theme_light()

sentiment_graph_malema <- sentiment_malema %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "#FF0000")+ 
  scale_x_continuous(breaks = seq(-8, 8, 2)) +
  ylab("Number of tweets") + 
  xlab("Sentiment score") +
  ggtitle("Malema") +
  theme_light()

sentiment_graph <- grid.arrange(sentiment_graph_malema, sentiment_graph_ramaphosa,
             sentiment_graph_zille,nrow = 1,
             top=grid::textGrob("Distribution of sentiment scores", 
                                x = 0, hjust = 0))
sentiment_graph



# NLP using Syuuzhet ####
syuzhet_graph_list <- lapply(politicians, function(x){
  tweets_vector <- tweet_df[tweet_df$Politician==x,5]
  tweet_df <- tweet_df[1:50,]
  syuzhet_sentiment <- data.frame("Politician"=tweet_df[,"Politician"],"score"=get_sentiment(tweet_df[,5], method="syuzhet"))

  syuzhet_graph <- syuzhet_sentiment %>%
    ggplot(aes(x=score)) + 
    scale_x_continuous(breaks = seq(-8, 8, 2), lim = c(-8,8)) +
    ylab("Number of tweets") + 
    xlab("Sentiment score") +
    ggtitle(x) +
    theme_light()
  
  if(x=="Malema"){
    syuzhet_graph <- syuzhet_graph +
      geom_histogram(binwidth = 1, fill = "#FF0000") 
  } else if(x=="Ramaphosa") {
    syuzhet_graph <- syuzhet_graph +
      geom_histogram(binwidth = 1, fill = "#FFCC00") 
  } else {
    syuzhet_graph <- syuzhet_graph +
      geom_histogram(binwidth = 1, fill = "#015A9C") 
  }
  return(syuzhet_graph)
})

syuzhet_graph <- grid.arrange(grobs=syuzhet_graph_list,nrow = 1,
                                top=grid::textGrob("Distribution of sentiment scores", 
                                                   x = 0, hjust = 0))


# Get the emotions using the NRC Emotion lexicon
emotion_list <- lapply(seq_along(politicians), function(i){
    tweets_vector <- tweet_df[tweet_df$Politician==politicians[i],5]
    
    emotion_df <- get_nrc_sentiment(tweets_vector) %>% 
    gather(emotion, value, "anger":"positive") %>% 
    group_by(emotion) %>% 
    dplyr::summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(total_tweets = sum(value)) %>% 
    mutate(emotion_proportion = round((value/total_tweets)*100)) %>% 
    mutate(Politician = politicians[i]) 
  

})

emotion_politician <- bind_rows(emotion_list)


# Create the grouped bar graph (horizontal)
emotion_plot <- ggplot(emotion_politician, aes(fill=Politician, x=emotion, y=emotion_proportion)) +
  geom_bar(position="dodge", stat="identity") + coord_flip() +
  theme_light()
    

emotion_plot

# Word cloud ####
sentiment_malema_vector <- score.sentiment(tweet_df[tweet_df$Politician=="Malema",]$text, pos.words, neg.words)[[2]]
sentiment_ramaphosa_vector <- score.sentiment(tweet_df[tweet_df$Politician=="Ramaphosa",]$text, pos.words, neg.words)[[2]]
sentiment_zille_vector <- score.sentiment(tweet_df[tweet_df$Politician=="Zille",]$text, pos.words, neg.words)[[2]]



sentiment_list <- list("malema"=sentiment_malema_vector, "ramaphosa"=sentiment_ramaphosa_vector,
                       "zille"=sentiment_zille_vector)

wordcloud_list <- lapply(seq_along(sentiment_list), function(i){
  text_corpus <- Corpus(VectorSource(sentiment_list[i]))
  text_corpus <- tm_map(text_corpus, content_transformer(tolower))
  text_corpus <- tm_map(text_corpus, function(x)removeWords(x,stopwords("english")))
  if(i==1){
    text_corpus <- tm_map(text_corpus, removeWords, c("malema","julius"))
  }
  if(i==2){
    text_corpus <- tm_map(text_corpus, removeWords, c("cyril","ramaphosa", "ramaphosas"))
  }
  else {
    text_corpus <- tm_map(text_corpus, removeWords, c("zille","helen"))
  }
  tdm <- TermDocumentMatrix(text_corpus)
  tdm <- as.matrix(tdm)
  tdm <- sort(rowSums(tdm), decreasing = TRUE)
  tdm <- data.frame(word = names(tdm), freq = tdm)
  set.seed(123)
  
  return(wordcloud(text_corpus, min.freq = 1, max.words = 50, scale = c(2.2,1),
            colors=brewer.pal(8, "Dark2"), random.color = T, random.order = F))
  
})


sentiment_average <- syuzhet_sentiment %>% 
  
  summarise(mean= mean(score))

