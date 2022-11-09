###Step 1 Tokenization and Normalization

library(tidytext)
library(tidyverse)

#Remove numbers from text
boston_test$review <- removeNumbers(boston_test$Review)

#Tokenize the words (unnest also removes punctuation and lowercases all words)
boston_word <- boston_test %>%
  unnest_tokens("word", "review2")

###Step 3 Stop Words
library(dplyr)

boston_word2 <- boston_word %>%
  anti_join(stop_words)

### Step 4 Lemmatization
library(textstem)

boston_word3$lemma <- lemmatize_words(boston_word3$word)

Boston_final = subset(boston_word3, select = -c(Review, review))
