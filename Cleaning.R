#' topic_modelling.R
#' 
#' contributors: 
#'
#' What this file does"
#' - Explores the stm package for topic modelling as applied to tidied tweets from Trump in early 2020
#'

# --- Library --- #
library(readr)
library(dplyr)
library(tibble)
library(tidytext)
library(textstem)
library(stm)
library(ggplot2)

# --- load the data --- # 
tidy_trump <- read_csv('data/tidy_trump.csv')

# --- Data Cleaning --- #
# Need stop word removal
# And lemmatize

tidy_trump <- 
  tidy_trump %>% 
  anti_join(stop_words) %>% 
  mutate(word_lemma = lemmatize_words(word))

# --- Establish a Vocab List --- #
# Keep any word that appears > 5 times
# Note: this is a little ad-hoc
# Should explore sensitivity to choice or use TF-IDF 

# set up what my vocab list will be
word_count <- 
  tidy_trump %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n > 5)

# Keep only those words in data
tidy_trump <- 
  tidy_trump %>% 
  filter(word_lemma %in% word_count$word_lemma)

# only keep words from my vocab list


# --- Create Doc-Term-Matrix --- #
# wordcount for eacht tweet
tweet_word_counts <- 
  tidy_trump %>% 
  group_by(id) %>% 
  count(word_lemma) %>% 
  ungroup()

# cast this to a matrix
trump_dtm <-
  tweet_word_counts %>% 
  cast_sparse(id, word_lemma, n)

# --- Model! --- #
# model
trump_topics <- 
  stm(trump_dtm,
      K = 16, 
      seed = 42
  )

# --- Explore Output --- #
labelTopics(trump_topics)

# top 10 words per topic visualized
td_beta <- tidy(tru)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

# Suppose we want to assign human readable labels to topics:
td_beta <- 
  td_beta %>%
  mutate(topic_name = case_when(
    topic == 1 ~ "topic 1", # ie. name it something meaningful,
    topic == 2 ~ "topic 2",
    topic == 3 ~ "topic 3",
    topic == 4 ~ "topic 4",
    topic == 5 ~ "topic 5",
    topic == 6 ~ "topic 6",
    topic == 7 ~ "topic 7",
    topic == 8 ~ "topic 8",
    topic == 9 ~ "topic 9",
    topic == 10 ~ "topic 10",
    topic == 11 ~ "topic 11",
    TRUE ~ "topic 12"
  )
  )

# regraph!

# --- Assigning Topics to Tweets --- #
td_gamma <- 
  tidy(trump_topics,
       matrix = "gamma",
       document_names = rownames(trump_dtm)) %>% 
  arrange(document, gamma, desc(gamma))

# give each tweet its most probably topic..
tweets_gamma <- 
  td_gamma %>%
  rename(id = document) %>%
  mutate(id = as.numeric(id)) %>%
  group_by(id) %>%
  slice_max(gamma) %>%
  select(-gamma)

tweets <- 
  read_csv("data/tweets_cleaned.csv") %>% 
  inner_join(tweets_gamma, by = "id")

# --- Topic Trends --- #
