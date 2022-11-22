library(tidytext)
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


### OLS regression


### Moderation analysis using Lavaan
Model 4: First-Stage Moderation
#-------------------------------------------------------------------------------\n")
library(haven)      # Read and write Stata, SPSS, or SAS data file 
library(lavaan)     # LAtent VAriable ANalysis 
library(semPlot)

model.4 <- "                                   
m1 ~ a1*x + a2*w + a3*xw  
y  ~ b1*m1 + b2*w + b3*xw + cp*x   
a1b1           := a1*b1               # Indirect effect of x on y via m
a3b1           := a3*b1               # Conditional indirect effect of xw on y 
cprime         := cp                  # Conditional direct effect of x on y
total          := a1*b1 + a3*b1 + cp  # Total effect of x on y
"
moderation.1 <- sem(model.4, data=data, se="bootstrap", bootstrap = 1000)  
summary(moderation.1, ci=T, standardized=T, rsquare=T, fit.measures=F) 

cat(

semPaths(mediation.1,              # Diagram of mediation.1
         whatLabels = "name",      # "name" of the path. Other options "est" (estimates) or "stand" (standardized est)
         rotation = 2,             # Rotation = 2 locates X on the left of the diagram
         asize = 5,                # Arrow-head size
         sizeMan=10,               # Size of boxes
         edge.label.position=.65,  # Relative position of label on arrow. Default = .5
         edge.label.cex=1.5,       # Size of labels on the paths (= edges,
         residuals = FALSE)        # Hide residuals to reduce visual clutter
         
semPaths(moderation.1,whatLabels="est",rotation=2,asize=5,sizeMan=10,
         edge.label.position=.65, edge.label.cex=1.5, residuals = FALSE) 

### Post test?