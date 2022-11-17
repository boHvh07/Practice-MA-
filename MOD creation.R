###1 Reviews seen
library(tidyverse)
bostonMod1 <- boston1 %>%
  rename("review_seen"="X")


###2 Rating seen
library(dplyr)

bostonMod1$Rating2 = NA

bostonMod1$Rating2 = c(bostonMod1$Rating[-1], NA)

