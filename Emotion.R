###NRC (via base)
NRC.Emotion.Intensity.Lexicon.v1 <- read.delim("~/Downloads/NRC-Emotion-Intensity-Lexicon/NRC-Emotion-Intensity-Lexicon/NRC-Emotion-Intensity-Lexicon-v1.txt", header=FALSE)
RC.Emotion.Lexicon.Wordlevel.v0.92 <- read.delim("~/Downloads/NRC-Emotion-Lexicon/NRC-Emotion-Lexicon/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt", header=FALSE)

### Preparing data for join()
library(tidyverse)

emolex <- NRC.Emotion.Lexicon.Wordlevel.v0.92 %>%
  rename("word"="V1", "emotion"="V2", "score"="V3")
nrceil <- NRC.Emotion.Intensity.Lexicon.v1 %>%
  rename("word"="V1", "emotion"="V2", "score"="V3")

nrceil$anger = NA
nrceil$anticipation = NA
nrceil$joy = NA
nrceil$sadness = NA
nrceil$disgust = NA
nrceil$fear = NA
nrceil$trust = NA
nrceil$surprise = NA

library(dplyr)
nrceil2 <- nrceil %>%
  mutate(anger = ifelse(nrceil$emotion == "anger", nrceil$score, NA)) %>%
  mutate(anticipation = ifelse(nrceil$emotion == "anticipation", nrceil$score, NA)) %>%
  mutate(joy = ifelse(nrceil$emotion == "joy", nrceil$score, NA)) %>%
  mutate(sadness = ifelse(nrceil$emotion == "sadness", nrceil$score, NA)) %>%
  mutate(disgust = ifelse(nrceil$emotion == "disgust", nrceil$score, NA)) %>%
  mutate(fear = ifelse(nrceil$emotion == "fear", nrceil$score, NA)) %>%
  mutate(trust = ifelse(nrceil$emotion == "trust", nrceil$score, NA)) %>%
  mutate(surprise = ifelse(nrceil$emotion == "surprise", nrceil$score, NA))




###Emolex (unnecesary)
boston_sent2 <- boston_final%>%
  inner_join(emolex, by = "word") %>% # inner join with our lexicon to get the polarity score
  group_by(review_num) #group by for sentence polarity


###NRCEIL

boston_sent4 <- boston_final %>%
  inner_join(nrceil2, by = "word") 

boston_sent5 <- boston_sent4 %>%
  group_by(review_num) %>%
  summarise(avgang = mean(anger, na.rm=T), avgant = mean(anticipation, na.rm=T), avgjoy = mean(joy, na.rm=T), avgsad = mean(sadness, na.rm=T),
            avgdis = mean(disgust, na.rm=T), avgfear = mean(fear, na.rm=T), avgtrust = mean(trust, na.rm=T), avgsurp = mean(surprise, na.rm=T))


### Join with original dataset for analysis
bostontest2 <- boston_test %>%
  rename("review_num"="...1")
bostonjoin = subset(bostontest2, select = -c(Review, review, review2))

Bostonana <- bostonjoin %>%
  inner_join(boston_sent5, by = "review_num")
