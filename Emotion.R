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
boston_emo <- boston_final%>%
  inner_join(emolex, by = "word") %>% # inner join with our lexicon to get the polarity score
  group_by(review_num) #group by for sentence polarity


###NRCEIL

boston_sent1 <- boston_final %>%
  inner_join(nrceil2, by = "word") 

boston_perc <- boston_sent1 %>%
  group_by(review_num) %>%
  summarise(avgang = sum(anger, na.rm=T), avgant = sum(anticipation, na.rm=T), avgjoy = sum(joy, na.rm=T), avgsad = sum(sadness, na.rm=T),
            avgdis = sum(disgust, na.rm=T), avgfear = sum(fear, na.rm=T), avgtrust = sum(trust, na.rm=T), avgsurp = sum(surprise, na.rm=T))

boston_perc$sum = rowSums(boston_perc[,c(2:9)]) 

library(dplyr)
boston_perc2 <- boston_perc %>% rowwise() %>% mutate(ang = avgang/sum, ant = avgant/sum, joy = avgjoy/sum, sad = avgsad/sum, 
                                                     dis = avgdis/sum, fear = avgfear/sum, trust = avgtrust/sum, surp = avgsurp/sum)   


### Join with original dataset for analysis
bostonjoin = subset(bostontest, select = -c(Review, review, review2))

Bostonana <- bostonjoin %>%
  inner_join(boston_perc, by = "review_num")
