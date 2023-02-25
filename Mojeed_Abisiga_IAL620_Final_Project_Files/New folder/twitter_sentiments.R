library(tidytext)
# install.packages("textdata")
library(textdata)
library(ggplot2)
library(tidyr)
# install.packages("wordcloud")
library(wordcloud)
library(reshape2)
library(lubridate)

library(readr)
# a collection of package for data wrangling.
library(tidyverse)
# package for text processing
library(tidytext)
# collection of packages for modeling and L 
# install.packages("tidymodels")
library(tidymodels)
library(scales)
# R package for managing and analyzing textual data
library(quanteda)
# An R package with word stemming algorithm
# collapsing words to a common root to aid comparison of vocabular. 
library(SnowballC)
# library for topic models (LDA)
library(topicmodels)
# text recipe
library(textrecipes)
# dealing with imbalance data using `step_downsample or upsample`.
library(themis)
# https://github.com/tidymodels/discrim
library(discrim)
# framework for constructing variable importance plots from ML models
library(vip)

library(igraph)
library(ggraph)


get_sentiments("bing")

# read in csv file as tibble/data frame
sentiment.data <- read.csv(file='sentiment_tweets.csv', stringsAsFactors=FALSE)

sentiment.data2 <- as_tibble(sentiment.data)


tweetsTMDF <-read.csv("sentiment_tweets.csv", encoding = "UTF-8")

### UDF to remove the URLs from the tweets
removeURLs <- function(tweet) {
  return(gsub("http\\S+", "", tweet))
}

#"^RT @[a-z,A-Z]*[0-9]*[a-z,A-Z]*[0-9]*: "

### UDF to remove RT from the tweets
removeUsernamesWithRT <- function(tweet) {
  return(gsub("(RT|via)((?:\\b\\W*@\\w+)+):","", tweet))
}
### UDF to remove the usernames or callouts from the tweets
removeUsernames <- function(tweet) {
  return(gsub("@[a-z,A-Z]*[0-9]*[a-z,A-Z]*[0-9]*", "", tweet))
}
### remove the hashtag # from the tweets
removeHashtagSignOnly <- function(tweet) {
  return(gsub("#", "", tweet))
}

# pre-processing using regex
tweetsTMDF$processed_tweet <- apply(tweetsTMDF['message'], 2, 
                                    removeURLs) 
tweetsTMDF$processed_tweet <- apply(tweetsTMDF['processed_tweet'],2, 
                                    removeUsernamesWithRT) 
tweetsTMDF$processed_tweet <- apply(tweetsTMDF['processed_tweet'],2, 
                                    removeUsernames)
tweetsTMDF$processed_tweet <- apply(tweetsTMDF['processed_tweet'],2, 
                                    removeHashtagSignOnly)

# pre-processing tokenization, stopword, stemming
text_tmdf <- tweetsTMDF %>%
  unnest_tokens(word, processed_tweet)%>%
  anti_join(stop_words[stop_words$lexicon == "snowball",], by = "word")%>%
  mutate(stem = wordStem(word))


clean.data_c <- clean.data$Company.Name
full_stop_words <- c(lapply(list(clean.data_c), tolower), list(c("cloud", "7", "2", "3", "hsbc", "dell", "bp", "dte", "aig", "duke", "amazon", "exxon", "hess", "petrochina", "sachs", "lloyds", "bcs", "cm", "nokia", "atlassian", "autodesk", "vmware", "slb", "shell", "schlumberger", "ecopetrol", "barclays", "exxonmobil", "adobe", "apple", "broadcom", "chevron", "s.a", "mro", "shopify", "mastercard", "mobil")))

custom_stop_words <- bind_rows(tibble(word = unlist(full_stop_words),  
                                      lexicon = c("custom")), 
                               stop_words)


#  we can tokenize text into consecutive sequences of words, called n-grams
text_bigrams<-tweetsTMDF %>%
  filter(sentiment == "negative") %>% 
  filter(sector == "Energy") %>% 
  unnest_tokens(bigram, processed_tweet, token="ngrams", n=2)

text_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- text_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united %>%
  count(sector, bigram) %>%
  arrange(desc(n)) %>% 
  top_n(8) %>% 
  ggplot(aes(y = fct_reorder(bigram, n), x = n)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Counts",
       y = "Bigram",
       title = "Most Frequent Bigrams in Tweets", 
  )+
  theme(plot.title = element_text(hjust = 0.5))

grouped <- tweetsTMDF %>%
  group_by(Day = as.integer(day(date)),
           sector,
           sentiment 
  ) %>% 
  summarize(
    average_sentiments = mean(score)
  )

tweetsTMDF %>%
  group_by(Day = as.integer(day(date)),
           sector,
           sentiment 
           ) %>% 
  summarize(
    average_sentiments = mean(score)
  ) %>% 
  ggplot(., aes(Day, average_sentiments))+
  scale_color_brewer(palette = "Paired")+
  geom_smooth(aes(color=sector), se = FALSE)+
  labs(y="Average Sentiments", x="Day",
       title = "Sentiment Trend Per Day For Each Sector", 
  )+
  facet_wrap(sentiment~sector, nrow(3), scales = "free_y")
  theme(plot.title = element_text(hjust = 0.5))

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  graph_from_data_frame()

set.seed(2022)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2023)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
