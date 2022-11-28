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

#### Ordinal Regression (Use polr command)
library(MASS)
polrdata <- polr(as.factor(Rating) ~ avgang + avgant + avgdis + avgjoy + avgsad
                 + avgfear + avgtrust + avgsurp + review_seen + rating_seen + avgang*review_seen + avgant*review_seen + avgdis*review_seen +
                   avgjoy*review_seen + avgsad*review_seen + avgfear*review_seen + avgtrust*review_seen + avgsurp*review_seen +
                   avgang*rating_seen + avgant*rating_seen + avgdis*rating_seen + avgjoy*rating_seen + avgsad*rating_seen + 
                   avgfear*rating_seen + avgtrust*rating_seen + avgsurp*rating_seen,
                 data = cityana_without_outliers, Hess = TRUE, method = c('logistic'))

summary(polrdata)

ols_aic(citylm2)

##### partial proportional odds model
ppo.model <- vglm(Rating ~ avgang + avgant + avgdis + avgjoy + avgsad
                                      + avgfear + avgtrust + avgsurp + review_seen + rating_seen 
                                      ,data = cityana_without_outliers, family = cumulative(parallel = F~avgant))
head(fitted(ppo.model))

summary(ppo.model)

ppo.model2 <- vglm(Rating ~ avgang + avgant + avgdis + avgjoy + avgsad
                  + avgfear + avgtrust + avgsurp + review_seen + rating_seen + avgang*review_seen + avgant*review_seen + avgdis*review_seen +
                    avgjoy*review_seen + avgsad*review_seen + avgfear*review_seen + avgtrust*review_seen + avgsurp*review_seen +
                    avgang*rating_seen + avgant*rating_seen + avgdis*rating_seen +
                    avgjoy*rating_seen + avgsad*rating_seen + avgfear*rating_seen + avgtrust*rating_seen + avgsurp*rating_seen
                  ,data = cityana_without_outliers, family = cumulative(parallel = T~avgant))

summary(ppo.model2)

ppo.model3 <- vglm(Rating ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang
                  ,data = cityana_without_outliers, family = cumulative(parallel = F))

summary(ppo.model3)

ctable <- coef(summary(ppo.model))

ci <- confint(ppo.model)
ci 

exp(coef(ppo.model))

exp(cbind(OR = coef(ppo.model), ci))

coef(ppo.model(matrix = TRUE))


#### partial nonproportional odds model
ppo.model4 <- vglm(Rating ~ avgang + avgant + avgdis + avgjoy + avgsad
                   + avgfear + avgtrust + avgsurp + review_seen + rating_seen + avgang*review_seen + avgant*review_seen + avgdis*review_seen +
                     avgjoy*review_seen + avgsad*review_seen + avgfear*review_seen + avgtrust*review_seen + avgsurp*review_seen +
                     avgang*rating_seen + avgant*rating_seen + avgdis*rating_seen +
                     avgjoy*rating_seen + avgsad*rating_seen + avgfear*rating_seen + avgtrust*rating_seen + avgsurp*rating_seen
                   ,data = cityana_without_outliers, family = propodds)

summary(ppo.model4)
coef(ppo.model4, matrix = TRUE)
confint(ppo.model4, matrix = TRUE)
exp(coef(ppo.model4, matrix = TRUE))
exp(confint(ppo.model4, matrix = TRUE))
cbind(exp(coef(ppo.model4)), exp(confint(ppo.model4)))





####Marginal effects table after model summary
summary(cityana_without_outliers)
