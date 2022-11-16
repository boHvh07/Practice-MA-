boston_perc <- boston_sent4 %>%
  group_by(review_num) %>%
  summarise(avgang = sum(anger, na.rm=T), avgant = sum(anticipation, na.rm=T), avgjoy = sum(joy, na.rm=T), avgsad = sum(sadness, na.rm=T),
            avgdis = sum(disgust, na.rm=T), avgfear = sum(fear, na.rm=T), avgtrust = sum(trust, na.rm=T), avgsurp = sum(surprise, na.rm=T))


Bostonana <- bostonjoin %>%
  inner_join(boston_perc, by = "review_num")

#changed all of them???/
#boston_perc$avgang[is.nan(boston_perc$avgang)]<- NA

boston_perc$sum = rowSums(boston_perc[,c(2:9)]) 

#data fixed to show proportion of each emotion per review
library(dplyr)
boston_perc2 <- boston_perc %>% rowwise() %>% mutate(ang = avgang/sum, ant = avgant/sum, joy = avgjoy/sum, sad = avgsad/sum, 
                                                     dis = avgdis/sum, fear = avgfear/sum, trust = avgtrust/sum, surp = avgsurp/sum)   
                                                     
  