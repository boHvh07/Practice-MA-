library(tidytext)
library(tidyverse)

city_2.2 = subset(city_1, select = -c(word))

city_2.2 <- city_2.2 %>%
  rename("word"="lemma")

#### Term frequency (group by rating / filter per rating can also prove informative)
word_freq2 <- city_2.2 %>%
  count(city, word, sort=TRUE)

total_words2 <- word_freq %>% group_by(city) %>% summarize(total = sum(n))

word_freq3 <- left_join(word_freq2, total_words2)

freq_rank <- word_freq3 %>%
  group_by(city) %>%
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

write.csv(freq_rank, "freq_rank.csv")

#### tf-idf
restaurant_tf_idf <- word_freq3 %>%
  bind_tf_idf(word, city, n)

restaurant_tf_idf2 <- restaurant_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

### tf-idf per star rating
rating_words <- city_2.2 %>%
  count(Rating, word, sort = TRUE)

plot_rating <- rating_words %>%
  bind_tf_idf(word, Rating, n) %>%
  mutate(Rating = factor(Rating, levels = c('1', '2', '3', '4', '5')))

plot_rating %>%
  arrange(desc(tf_idf))

plot_rating_tf <- plot_rating %>% 
  group_by(Rating) %>% 
  slice_max(tf_idf, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = Rating)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~Rating, ncol = 2, scales = "free")
plot_rating_tf
