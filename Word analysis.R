library(tidytext)
library(tidyverse)
library(ggplot2)
library(ggpubr)

city_2 = subset(city_1, select = -c(word))

city_2 <- city_2 %>%
  rename("word"="lemma")

#### Term frequency (group by rating / filter per rating can also prove informative)
word_freq <- city_2.2 %>%
  count(city, word, sort=TRUE)

total_words <- word_freq %>% group_by(city) %>% summarize(total = sum(n))

word_freq2 <- left_join(word_freq, total_words)

freq_rank <- word_freq3 %>%
  group_by(city) %>%
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

write.csv(freq_rank, "freq_rank.csv")

rating_words <- city_2 %>%
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

rank5 <- subset(rating_words, Rating == 5)

rank5 <- rank5 %>% 
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = '5 star rating \n') +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme()

rank_figure <- ggarrange(rank1, rank2, rank3, rank4, rank5,
                         ncol = 2, nrow = 3)
annotate_figure(rank_figure, top = text_grob("Most frequent used words per restaurant rating",
                                             color = "black", face = 'bold', size = 14))
ggsave("rank_figure.pdf")




























#### tf-idf
restaurant_tf_idf <- word_freq2 %>%
  bind_tf_idf(word, city, n)

restaurant_tf_idf2 <- restaurant_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))


### tf-idf per star rating
rating_words <- city_2 %>%
  count(Rating, word, sort = TRUE)

plot_rating <- rating_words %>%
  bind_tf_idf(word, Rating, n) %>%
  mutate(Rating = factor(Rating, levels = c('1', '2', '3', '4', '5')))

plot_rating %>%
  arrange(desc(tf_idf))

plot_rating_freq <- plot_rating %>% 
  group_by(Rating) %>% 
  slice_max(tf_idf, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = Rating)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~Rating, ncol = 2, scales = "free")
plot_rating_tf
