library(tidyverse)
library(rtweet)
library(ggplot2)

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


# Get Twitter data in Cape Town, Johannesburg, London  ####

# cities <- c("cape town", "johannesburg", "london")
# 
# tweet_list <- lapply(cities, function(x) search_tweets(
#   "lang:en", geocode = lookup_coords(x),  retryonratelimit = TRUE, n=18000
# ))


tweets_london <- search_tweets(
  "lang:en", geocode = lookup_coords("london"),  retryonratelimit = TRUE, n=18000
)
Sys.sleep(904)
tweets_cape_town <- search_tweets(
  "lang:en", geocode = lookup_coords("cape town"),  retryonratelimit = TRUE, n=18000
)
Sys.sleep(904)
tweets_johannesburg <- search_tweets(
  "lang:en", geocode = lookup_coords("johannesburg"),  retryonratelimit = TRUE, n=18000
)



tweet_list <- list("cape town"=tweets_cape_town, "johannesburg"=tweets_johannesburg,
                   "london"=tweets_london)


tweet_list_preprocess <- lapply(tweet_list, function(x){
 df <- x %>%
   mutate(text = clean.text(text))
 return(df)
})
# Name the list elements
names(tweet_list_preprocess) <- cities

# Number of Tweets per hour ####
tweet_list_time <- lapply(tweet_list_preprocess, function(x){
  df <- x %>%
    mutate(Created_at_round = round.POSIXt(created_at, units = "mins"))
  return(df)
})

# Below to create single dataframe
tweet_df_city <- Map(cbind, tweet_list_time, City = names(tweet_list_time)) %>% 
  bind_rows(.) %>% 
  group_by(City) %>% 
  count(Created_at_round)

tweet_df <- Map(cbind, tweet_list_time, City = names(tweet_list_time)) %>% 
  bind_rows(.) %>% 
  count(Created_at_round, name = "total") %>% 
  bind_rows(tweet_df_city) %>% 
  drop_na(City)




# Plot time of tweets
plot <- ggplot(tweet_df, aes(x=Created_at_round, y=total)) +
  geom_line() +
  theme_light() +
  xlab("Time") + ylab(NULL) +
  ggtitle(paste0("Number of Tweets per minute", ": ", as.character(Sys.Date()))) +
  geom_line(aes(x=Created_at_round, y=n, color=City))

plot



# Get Sentiment scores ####





