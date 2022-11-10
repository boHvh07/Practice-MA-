####NRC (sentiment via tidyverse)
boston_sent <- Boston_final %>%
  inner_join(get_sentiments("nrc"))

###NRC (via gettext)
library(textdata)

emolex_lexicon <- lexicon_nrc()
nrceil_lexicon <- lexicon_nrc_eil()

###NRC(via install package)
