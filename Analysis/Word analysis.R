library(tidytext)
library(tidyverse)
library(ggplot2)
library(ggpubr)

  rename("word"="lemma")
#### Term frequency all data
word_freq <- worddata %>%
  count(word, sort=TRUE)

total_words <- word_freq %>% summarize(total = sum(n))


### Plot rating
rating_words <- worddata %>%
  count(Rating, word, sort = TRUE)
  
rank1 <- subset(rating_words, Rating == 1)
rank2 <- subset(rating_words, Rating == 2)
rank3 <- subset(rating_words, Rating == 3)
rank4 <- subset(rating_words, Rating == 4)
rank5 <- subset(rating_words, Rating == 5)

rank1 <- rank1 %>% 
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = NULL, title = '1 star rating \n') +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")

rank2 <- rank2 %>% 
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = NULL, title = '2 star rating \n') +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")

rank3 <- rank3 %>% 
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = NULL, title = '3 star rating \n') +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")

rank4 <- rank4 %>% 
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = NULL, title = '4 star rating \n') +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")

rank5 <- rank5 %>% 
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = '5 star rating \n') +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme()

library(ggpubr)

rank_figure <- ggarrange(rank1, rank2, rank3, rank4, rank5,
                         ncol = 2, nrow = 3)
annotate_figure(rank_figure, top = text_grob("Most frequently used words per restaurant rating",
                                             color = "black", face = 'bold', size = 14))
rank_figure
ggsave("rank_figure.pdf")
