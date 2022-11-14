###NRC (via base)
NRC.Emotion.Intensity.Lexicon.v1 <- read.delim("~/Downloads/NRC-Emotion-Intensity-Lexicon/NRC-Emotion-Intensity-Lexicon/NRC-Emotion-Intensity-Lexicon-v1.txt", header=FALSE)
RC.Emotion.Lexicon.Wordlevel.v0.92 <- read.delim("~/Downloads/NRC-Emotion-Lexicon/NRC-Emotion-Lexicon/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt", header=FALSE)

### Preparing data for join()
library(tidyverse)

emolex <- NRC.Emotion.Lexicon.Wordlevel.v0.92 %>%
  rename("word"="V1", "emotion"="V2", "score"="V3")
nrceil <- NRC.Emotion.Intensity.Lexicon.v1 %>%
  rename("word"="V1", "emotion"="V2", "score"="V3")

boston_final2 <- boston_final
boston_final2$anger = NA
boston_final2$anticipation = NA
boston_final2$joy = NA
boston_final2$sadness = NA
boston_final2$disgust = NA
boston_final2$fear = NA
boston_final2$trust = NA
boston_final2$surprise = NA

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




###Emolex
boston_sent2 <- boston_final%>%
  inner_join(emolex, by = "word") %>% # inner join with our lexicon to get the polarity score
  group_by(review_num) #group by for sentence polarity

  
  
###NRCEIL
boston_sent3 <- boston_final%>%
  inner_join(nrceil)%>% # inner join with our lexicon to get the intensity score
  group_by(review_num) # group by for review emotion intensity

boston_sent4 <- boston_sent3 %>%
  separate(boston_sent3$emotion, c("anticipation", "anger", "joy", "surprise", "fear", "trust", "disgust", "sadness")))
