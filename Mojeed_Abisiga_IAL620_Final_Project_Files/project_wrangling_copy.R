library(tidyverse)
library(tidytext)
library(ggplot2)
library(tidyr)
library(forcats)
# install.packages("SemNetCleaner")
# library(SemNetCleaner)

# read in csv file as tibble/data frame
scrape.data <- read.csv(file='stockdata.csv', stringsAsFactors=FALSE)

clean.data <- as_tibble(scrape.data)

# transform table into one-word-per-line tidytext format
clean.data <- clean.data %>%
  unnest_tokens(word, Top.News)


# most frequent words
clean.data %>%
  count(word, sort = TRUE)


clean.data2 <- clean.data$Company.Name
full_stop_words <- c(lapply(list(clean.data2), tolower), list(c("cloud", "7", "2", "3", "hsbc", "dell", "bp", "dte", "aig", "duke", "amazon", "exxon", "hess", "petrochina", "sachs", "lloyds", "bcs", "cm", "nokia", "atlassian", "autodesk", "vmware", "slb", "shell", "schlumberger", "ecopetrol", "barclays", "exxonmobil", "adobe", "apple", "broadcom", "chevron", "s.a", "mro", "shopify", "mastercard", "mobil")))

custom_stop_words <- bind_rows(tibble(word = unlist(full_stop_words),  
                                      lexicon = c("custom")), 
                               stop_words)

# remove stop words
data(stop_words)
clean.data <- clean.data %>%
  anti_join(custom_stop_words)

# check result of stop word removal
clean.data %>%
  count(word, sort = TRUE)


# remove numbers
nums <- clean.data %>% 
  filter(str_detect(word, "^[0-9]")) %>% 
  select(word) %>% 
  unique()

clean.data <- clean.data %>%
  anti_join(nums, by = "word")

# check result of numbers removal
clean.data %>%
  count(word, sort = TRUE)

str(clean.data)

# remove other words
uni_sw <- data.frame(word = sapply(scrape.data$Sector, tolower))

# sapply(scrape.data$Company.Name, singularize(dictionary = TRUE)))


clean.data <- clean.data %>%
  anti_join(uni_sw, by = "word")


# check result of other words removal
clean.data %>%
  count(word, sort = TRUE)

# visualize top words in corpus (For all News)
clean.data %>%
  count(word, sort = TRUE) %>%
  filter(n >= 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# visualize top words in corpus (For News Per Sector)
clean.data %>%
  count(Sector, word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  group_by(Sector) %>% 
  top_n(5) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~Sector, ncol = 1, scales="free")


library(tidyr)

clean.data <- clean.data %>% 
  filter(Sector %in% c("Financial Services", "Technology", "Energy"))

frequency <- clean.data %>% 
  count(Sector, word) %>%
  group_by(Sector) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Sector, values_from = proportion) 

frequency <- frequency %>% 
  pivot_longer(`Financial Services`:`Energy`,
               names_to = "Sector", values_to = "proportion")



library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Technology`, 
                      color = abs(`Technology` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~Sector, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Technology", x = NULL)


frequency <- clean.data %>% 
  count(Sector, word) %>%
  group_by(Sector) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = Sector, values_from = proportion) 

frequency <- frequency %>% 
  pivot_longer(`Financial Services`:`Technology`,
               names_to = "Sector", values_to = "proportion")



library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Energy`, 
                      color = abs(`Energy` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~Sector, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Energy", 
       x = "Proportion of Word",
       title = "Comparing the Word Frequencies of 3 Different Sectors")+
  theme(plot.title = element_text(hjust = 0.5))


cor.test(data = frequency[frequency$Sector == "Financial Services",],
         ~ proportion + `Energy`)

cor.test(data = frequency[frequency$Sector == "Technology",],
         ~ proportion + `Energy`)


clean.data2 <- as_tibble(scrape.data)



clean.data2 <- clean.data2 %>%
  unnest_tokens(word, Top.News) %>%
  count(Sector, word, sort = TRUE)

# remove stop words
data(stop_words)
clean.data2 <- clean.data2 %>%
  anti_join(custom_stop_words)


total_words <- clean.data2 %>% 
  group_by(Sector) %>% 
  summarize(total = sum(n))


company_words <- left_join(clean.data2, total_words)

ggplot(company_words, aes(n/total, fill = Sector)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.09) +
  facet_wrap(~Sector, ncol = 3, scales = "free_y")


freq_by_rank <- company_words %>% 
  group_by(Sector) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()


freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = Sector)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()



company_tf_idf <- company_words %>%
  bind_tf_idf(word, Sector, n)


company_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))



company_tf_idf %>%
  group_by(Sector) %>%
  slice_max(tf_idf, n = 8) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Sector)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(.~Sector, ncol = 2, scales = "free") +
  labs(x = "tf-idf", 
       y = "word",
       title = "Important Words in News Sentiments For Each Sector")+
  theme(plot.title = element_text(hjust = 0.5))

