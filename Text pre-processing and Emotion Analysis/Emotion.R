###NRC (via base)
NRC.Emotion.Intensity.Lexicon.v1 <- read.delim("~/Downloads/NRC-Emotion-Intensity-Lexicon/NRC-Emotion-Intensity-Lexicon/NRC-Emotion-Intensity-Lexicon-v1.txt", header=FALSE)

### Preparing data for join()
library(tidyverse)
library(dplyr)

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

nrceil2 <- nrceil %>%
  mutate(anger = ifelse(nrceil$emotion == "anger", nrceil$score, NA)) %>%
  mutate(anticipation = ifelse(nrceil$emotion == "anticipation", nrceil$score, NA)) %>%
  mutate(joy = ifelse(nrceil$emotion == "joy", nrceil$score, NA)) %>%
  mutate(sadness = ifelse(nrceil$emotion == "sadness", nrceil$score, NA)) %>%
  mutate(disgust = ifelse(nrceil$emotion == "disgust", nrceil$score, NA)) %>%
  mutate(fear = ifelse(nrceil$emotion == "fear", nrceil$score, NA)) %>%
  mutate(trust = ifelse(nrceil$emotion == "trust", nrceil$score, NA)) %>%
  mutate(surprise = ifelse(nrceil$emotion == "surprise", nrceil$score, NA))


###NRCEIL

city_1 <- city_final %>%
  inner_join(nrceil2, by = 'word') 

city_perc <- city_1 %>%
  group_by(review_id) %>%
  summarise(sumang = sum(anger, na.rm=T), sumant = sum(anticipation, na.rm=T), sumjoy = sum(joy, na.rm=T), sumsad = sum(sadness, na.rm=T),
            sumdis = sum(disgust, na.rm=T), sumfear = sum(fear, na.rm=T), sumtrust = sum(trust, na.rm=T), sumsurp = sum(surprise, na.rm=T))

### Join with original dataset for analysis
cityjoin = subset(city, select = -c(Review, review))

cityana <- cityjoin %>% #change name per city
  inner_join(city_perc2, by = "review_id")

summary(cityana)
