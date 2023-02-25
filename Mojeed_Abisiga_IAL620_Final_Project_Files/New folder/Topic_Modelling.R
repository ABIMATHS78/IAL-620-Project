library(topicmodels)
library(stringr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(tidyr)
library(forcats)

# read in csv file as tibble/data frame
scrape.data <- read.csv(file='stockdata.csv', stringsAsFactors=FALSE)

clean.data <- as_tibble(scrape.data)


# split into words
clean.data_word <- clean.data %>%
  unnest_tokens(word, Top.News)

clean.data2 <- clean.data$Company.Name
full_stop_words <- c(lapply(list(clean.data_c), tolower), list(c("cloud", "7", "2", "3", "hsbc", "dell", "bp", "dte", "aig", "duke", "amazon", "exxon", "hess", "petrochina", "sachs", "lloyds", "bcs", "cm", "nokia", "atlassian", "autodesk", "vmware", "slb", "shell", "schlumberger", "ecopetrol", "barclays", "exxonmobil", "adobe", "apple", "broadcom", "chevron", "s.a", "mro", "shopify", "mastercard", "mobil", "goldman", "5")))
unlist(full_stop_words)

custom_stop_words <- bind_rows(tibble(word = unlist(full_stop_words),  
                                      lexicon = c("custom")), 
                               stop_words)

custom_stop_words

clean.data_word <- clean.data_word %>%
  inner_join(get_sentiments("bing"))


# find company-word counts
word_counts <- clean.data_word %>%
  filter(sentiment == "negative") %>% 
  anti_join(custom_stop_words) %>%
  count(Company.Name, word, sort = TRUE)


the_dtm <- word_counts %>%
  cast_dtm(Company.Name, word, n)

# set a seed so that the output of the model is predictable
the_lda <- LDA(the_dtm, k = 6, control = list(seed = 1234))
the_lda
# A LDA_VEM topic model with 3 topics.

the_topics <- tidy(the_lda, matrix = "beta")
the_topics


top_terms <- the_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 3) %>% 
  ungroup() %>%
  arrange(topic, -beta)


top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 2, scales = "free") +
  scale_y_reordered()


the_gamma <- tidy(the_lda, matrix = "gamma")
the_gamma


sector <- clean.data$Sector

full_data <- merge(the_gamma, clean.data, by.x = "document", by.y = "Company.Name", all.x = TRUE)

# reorder titles in order of topic 1, topic 2, etc before plotting
full_data %>%
  mutate(title = reorder(Sector, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ Sector) +
  labs(x = "topic", y = expression(gamma))

