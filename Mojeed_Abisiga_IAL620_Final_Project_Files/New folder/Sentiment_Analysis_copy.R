library(tidytext)
# install.packages("textdata")
library(textdata)
library(tidyverse)
library(ggplot2)
library(tidyr)
# install.packages("wordcloud")
library(wordcloud)
library(reshape2)


get_sentiments("bing")


# read in csv file as tibble/data frame
scrape.data <- read.csv(file='stockdata.csv', stringsAsFactors=FALSE)

clean.data <- as_tibble(scrape.data)

# transform table into one-word-per-line tidytext format
clean.data2 <- clean.data %>%
  unnest_tokens(word, Top.News)

clean.data_c <- clean.data$Company.Name
full_stop_words <- c(lapply(list(clean.data_c), tolower), list(c("cloud", "7", "2", "3", "hsbc", "dell", "bp", "dte", "aig", "duke", "amazon", "exxon", "hess", "petrochina", "sachs", "lloyds", "bcs", "cm", "nokia", "atlassian", "autodesk", "vmware", "slb", "shell", "schlumberger", "ecopetrol", "barclays", "exxonmobil", "adobe", "apple", "broadcom", "chevron", "s.a", "mro", "shopify", "mastercard", "mobil")))

custom_stop_words <- bind_rows(tibble(word = unlist(full_stop_words),  
                                      lexicon = c("custom")), 
                               stop_words)

# remove stop words
data(stop_words)
clean.data2 <- clean.data2 %>%
  anti_join(custom_stop_words)


company_sentiment <- clean.data2 %>%
  inner_join(get_sentiments("bing"))

company_sentiment2 <- company_sentiment %>% 
  count(Sector, sentiment)

company_sentiment3 <- company_sentiment2 %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(total_sentiment = positive - negative)

ggplot(company_sentiment3, aes(Sector, total_sentiment, fill = Sector)) +
  geom_col(show.legend = FALSE)

ggplot(company_sentiment3, aes(Sector, negative, fill = Sector)) +
  geom_col(show.legend = FALSE)

ggplot(company_sentiment3, aes(Sector, positive, fill = Sector)) +
  geom_col(show.legend = FALSE)


company_sentiment2 %>% 
  group_by(sentiment, Sector) %>% 
  summarise(total_sentiment = sum(n)) %>% 
  arrange(desc(total_sentiment)) %>% 
  ggplot(.,aes(x = sentiment, y = total_sentiment, fill=Sector)) +
  geom_col(aes(fill=Sector), position = "dodge", stats="identity", show.legend = TRUE)+
  theme_bw() + 
  labs(
    title = "News Sentiment By Companies Sector", 
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(. ~ sentiment, ncol = 2, scales = "free_x")


company_sentiment4 <- company_sentiment %>% 
  count(Sector, Company.Name, sentiment)

company_sentiment5 <- company_sentiment4 %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)


company_sentiment4 %>% 
  filter(sentiment == "negative") %>% 
  group_by(Sector, Company.Name, sentiment) %>% 
  summarise(total_sentiment = sum(n)) %>% 
  arrange(Sector, desc(total_sentiment))

company_sentiment4 %>% 
  filter(sentiment == "negative") %>% 
  group_by(Sector, Company.Name, sentiment) %>% 
  summarise(total_sentiment = sum(n)) %>% 
  arrange(Sector, desc(total_sentiment)) %>% 
  view()

company_sentiment4 %>% 
  filter(sentiment == "positive") %>% 
  group_by(Sector, Company.Name, sentiment) %>% 
  summarise(total_sentiment = sum(n)) %>% 
  arrange(Sector, desc(total_sentiment)) %>% 
  view()

############################################################################
# Companies with the most Negative Sentiments per Sector
############################################################################
# Energy => DTE Energy Company
# Financial Services => Barclays
# Technology => Atlassian
############################################################################

############################################################################
# Companies with the most Positive Sentiments per Sector
############################################################################
# Energy => Duke Energy
# Financial Services => Bank of America
# Technology => VMware
############################################################################


# Top Negative Sentiments
bing_word_counts1 <- clean.data2 %>% 
  filter(Company.Name %in% c("DTE Energy Company", "Barclays", "Atlassian")) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(Company.Name, word, sentiment, sort = TRUE) %>%
  ungroup()
  
view(bing_word_counts1)


bing_word_counts2 <- clean.data2 %>% 
  inner_join(get_sentiments("bing")) %>%
  count(Company.Name, word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts1 %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(Company.Name~sentiment, ncol = 2, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL,
       title = "Most Frequent words For Top Companies Based on Sentiments", 
  )+
  theme(plot.title = element_text(hjust = 0.5))


# Top Positive Sentiments
clean.data2 %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


clean <- clean.data2 %>%
  filter(Company.Name == "Adobe")
clean$word


clean.data2 %>%
  filter(Company.Name == "Adobe") %>% 
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


clean.data2 %>%
  filter(Company.Name %in% c("Duke Energy", "Bank of America", "VMware")) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100,
                   fixed.asp=TRUE)

energy <- clean.data2 %>%
  filter(Sector %in% c("Energy")) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100,
                   fixed.asp=TRUE)

fin_serv <- clean.data2 %>%
  filter(Sector %in% c("Financial Services")) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100,
                   fixed.asp=TRUE)

technology <- clean.data2 %>%
  filter(Sector %in% c("Technology")) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100,
                   fixed.asp=TRUE)

