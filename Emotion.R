####NRC (sentiment via tidyverse)
boston_sent <- Boston_final %>%
  inner_join(get_sentiments("nrc"))

###NRC (via gettext)
NRC.Emotion.Intensity.Lexicon.v1 <- read.delim("~/Downloads/NRC-Emotion-Intensity-Lexicon/NRC-Emotion-Intensity-Lexicon/NRC-Emotion-Intensity-Lexicon-v1.txt", header=FALSE)
RC.Emotion.Lexicon.Wordlevel.v0.92 <- read.delim("~/Downloads/NRC-Emotion-Lexicon/NRC-Emotion-Lexicon/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt", header=FALSE)

library(tidyverse)

emolex <- NRC.Emotion.Lexicon.Wordlevel.v0.92 %>%
  rename("word"="V1", "emotion"="V2", "score"="V3")
nrceil <- NRC.Emotion.Intensity.Lexicon.v1 %>%
  rename("word"="V1", "emotion"="V2", "score"="V3")


group_by(review_num) %>% #group by for sentence polarity
  summarise(sentiment = sum(score)) # final sentence polarity from words

boston_sent3 <- boston_final%>%
  inner_join(nrceil)%>% # inner join with our lexicon to get the intensity score
  group_by(review_num) %>%
  


boston_sent2 <- boston_final%>%
  inner_join(emolex) %>% # inner join with our lexicon to get the polarity score
  group_by(review_num) %>% #group by for sentence polarity
  summarise(sentiment = sum(score)) # final sentence polarity from words