library(dplyr)
### visualize emotions
ang_r <- aggregate(citydata_out$sumang, list(citydata_out$Rating), FUN=mean)
ant_r <- aggregate(citydata_out$sumant, list(citydata_out$Rating), FUN=mean)
dis_r <- aggregate(citydata_out$sumdis, list(citydata_out$Rating), FUN=mean)
joy_r <- aggregate(citydata_out$sumjoy, list(citydata_out$Rating), FUN=mean)
sad_r <- aggregate(citydata_out$sumsad, list(citydata_out$Rating), FUN=mean)
fear_r <- aggregate(citydata_out$sumfear, list(citydata_out$Rating), FUN=mean)
trust_r <- aggregate(citydata_out$sumtrust, list(citydata_out$Rating), FUN=mean)
surp_r <- aggregate(citydata_out$sumsurp, list(citydata_out$Rating), FUN=mean)

emo_r <- ang_r %>% inner_join(ant_r,by = 'Group.1')
emo_r <- emo_r %>% inner_join(dis_r,by = 'Group.1')
emo_r <- emo_r %>% inner_join(joy_r,by = 'Group.1')
emo_r <- emo_r %>% inner_join(sad_r,by = 'Group.1')
emo_r <- emo_r %>% inner_join(fear_r,by = 'Group.1')
emo_r <- emo_r %>% inner_join(trust_r,by = 'Group.1')
emo_r <- emo_r %>% inner_join(surp_r,by = 'Group.1')

emo_r <- emo_r %>%
  rename("Anger"="x.x", "Anticipation"="x.y", "Disgust"="x.x.x", "Joy"="x.y.y", "Sadness"="x.x.x.x", "Fear"="x.y.y.y", 
         "Trust"="x.x.x.x.x", "Surprise"="x.y.y.y.y", "Rating"="Group.1")

#Plot



### visualize interaction rating_seen



### visualize interaction reviews_seen

