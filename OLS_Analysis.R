library(tidytext)
library(tidyverse)
https://www.tidytextmining.com/tfidf.html

city_2 = subset(city_word2, select = -c(word))

city_2 <- city_2 %>%
  rename("word"="lemma")

#### Term frequency (group by rating / filter per rating can also prove informative)
word_freq <- city_2 %>%
  count(city, word, sort=TRUE)

total_words <- word_freq %>% group_by(city) %>% summarize(total = sum(n))

word_freq <- left_join(word_freq, total_words)

freq_rank <- word_freq %>%
  group_by(city) %>%
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
    ungroup()
#### tf-idf
restaurant_tf_idf <- word_freq %>%
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

### OLS regression
library(olsrr)

summary(cityana)

ols_regress(Rating ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang, data = cityana)
ols_regress(Rating ~ ant + rating_seen + rating_seen*ant + review_seen + review_seen*ant, data = cityana)
ols_regress(Rating ~ joy + rating_seen + rating_seen*joy + review_seen + review_seen*joy, data = cityana)
ols_regress(Rating ~ sad + rating_seen + rating_seen*sad + review_seen + review_seen*sad, data = cityana)
ols_regress(Rating ~ dis + rating_seen + rating_seen*dis + review_seen + review_seen*dis, data = cityana)
ols_regress(Rating ~ fear + rating_seen + rating_seen*fear + review_seen + review_seen*fear, data = cityana)
ols_regress(Rating ~ trust + rating_seen + rating_seen*trust + review_seen + review_seen*trust, data = cityana)
ols_regress(Rating ~ surp + rating_seen + rating_seen*surp + review_seen + review_seen*surp, data = cityana)

olsdata <- ols_regress(Rating ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang +
              ant + rating_seen*ant + review_seen*ant + joy + rating_seen*joy + review_seen*joy +
              sad + rating_seen*sad + review_seen*sad + dis + rating_seen*dis + review_seen*dis +
              fear + rating_seen*fear + review_seen*fear + trust + rating_seen*trust + review_seen*trust +
              surp + rating_seen*surp + review_seen*surp
            , data = cityana_without_outliers)

summary(olsdata)
ols_aic(citylm2)
#### Ordinal Regression (Use polr command)
library(MASS)
polrdata <- polr(as.factor(Rating) ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang +
                    ant + rating_seen*ant + review_seen*ant + joy + rating_seen*joy + review_seen*joy +
                    sad + rating_seen*sad + review_seen*sad + dis + rating_seen*dis + review_seen*dis +
                    fear + rating_seen*fear + review_seen*fear + trust + rating_seen*trust + review_seen*trust +
                    surp + rating_seen*surp + review_seen*surp
                  , data = cityana_without_outliers, Hess = TRUE, method = c('logistic'), na.rm=T)

summary(polrdata)


polrdata2 <- polr(as.factor(Rating) ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang +
                   joy + rating_seen*joy + review_seen*joy +
                   sad + rating_seen*sad + review_seen*sad + dis + rating_seen*dis + review_seen*dis +
                   fear + rating_seen*fear + review_seen*fear + trust + rating_seen*trust + review_seen*trust +
                   surp + rating_seen*surp + review_seen*surp
                 , data = cityana_without_outliers, Hess = TRUE, method = c('logistic'))
summary(polrdata2)
