####NRC (sentiment via tidyverse)
boston_sent <- Boston_final %>%
  inner_join(get_sentiments("nrc"))

###NRC (via package install.)
install.packages("devtools")
install.packages("C:/Users/Bovan/Downloads/NRC-Emotion-Intensity-Lexicon.zip", repos=NULL, type='source')
install.packages("C:/Users/Bovan/Downloads/NRC-Emotion-Lexicon.zip", repos=NULL, type='source')
