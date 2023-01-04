library(dplyr)
### visualize emotions
ang_r <- aggregate(citydata$sumang, list(citydata$Rating), FUN=mean)
ant_r <- aggregate(citydata$sumant, list(citydata$Rating), FUN=mean)
dis_r <- aggregate(citydata$sumdis, list(citydata$Rating), FUN=mean)
joy_r <- aggregate(citydata$sumjoy, list(citydata$Rating), FUN=mean)
sad_r <- aggregate(citydata$sumsad, list(citydata$Rating), FUN=mean)
fear_r <- aggregate(citydata$sumfear, list(citydata$Rating), FUN=mean)
trust_r <- aggregate(citydata$sumtrust, list(citydata$Rating), FUN=mean)
surp_r <- aggregate(citydata$sumsurp, list(citydata$Rating), FUN=mean)

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
emo_r
write.csv(emo_r, "C:\\Users\\Bovan\\OneDrive\\Documents\\MA Thesis\\Thesis\\emo_r.csv")

