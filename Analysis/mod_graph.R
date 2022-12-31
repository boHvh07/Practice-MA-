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

### visualize interaction rating_seen
### visualize emotions
citydata_out$sumangrev = citydata_out$sumang*citydata_out$review_seen
citydata_out$sumantrev = citydata_out$sumant*citydata_out$review_seen
citydata_out$sumdisrev = citydata_out$sumdis*citydata_out$review_seen
citydata_out$sumjoyrev = citydata_out$sumjoy*citydata_out$review_seen
citydata_out$sumsadrev = citydata_out$sumsad*citydata_out$review_seen
citydata_out$sumfearrev = citydata_out$sumfear*citydata_out$review_seen
citydata_out$sumtrustrev = citydata_out$sumtrust*citydata_out$review_seen
citydata_out$sumsurprev = citydata_out$sumsurp*citydata_out$review_seen

ang_rev <- aggregate(citydata_out$sumangrev, list(citydata_out$Rating), FUN=mean)
ant_rev <- aggregate(citydata_out$sumantrev, list(citydata_out$Rating), FUN=mean)
dis_rev <- aggregate(citydata_out$sumdisrev, list(citydata_out$Rating), FUN=mean)
joy_rev <- aggregate(citydata_out$sumjoyrev, list(citydata_out$Rating), FUN=mean)
sad_rev <- aggregate(citydata_out$sumsadrev, list(citydata_out$Rating), FUN=mean)
fear_rev <- aggregate(citydata_out$sumfearrev, list(citydata_out$Rating), FUN=mean)
trust_rev <- aggregate(citydata_out$sumtrustrev, list(citydata_out$Rating), FUN=mean)
surp_rev <- aggregate(citydata_out$sumsurprev, list(citydata_out$Rating), FUN=mean)

emo_rev <- ang_rev %>% inner_join(ant_rev,by = 'Group.1')
emo_rev <- emo_rev %>% inner_join(dis_rev,by = 'Group.1')
emo_rev <- emo_rev %>% inner_join(joy_rev,by = 'Group.1')
emo_rev <- emo_rev %>% inner_join(sad_rev,by = 'Group.1')
emo_rev <- emo_rev %>% inner_join(fear_rev,by = 'Group.1')
emo_rev <- emo_rev %>% inner_join(trust_rev,by = 'Group.1')
emo_rev <- emo_rev %>% inner_join(surp_rev,by = 'Group.1')

emo_rev <- emo_rev %>%
  rename("Anger"="x.x", "Anticipation"="x.y", "Disgust"="x.x.x", "Joy"="x.y.y", "Sadness"="x.x.x.x", "Fear"="x.y.y.y", 
         "Trust"="x.x.x.x.x", "Surprise"="x.y.y.y.y", "Rating"="Group.1")
emo_rev

write.csv(emo_rev, "C:\\Users\\Bovan\\OneDrive\\Documents\\MA Thesis\\Thesis\\emo_rev.csv")

### visualize interaction reviews_seen
citydata_out$sumangrat = citydata_out$sumang*citydata_out$rating_seen
citydata_out$sumantrat = citydata_out$sumant*citydata_out$rating_seen
citydata_out$sumdisrat = citydata_out$sumdis*citydata_out$rating_seen
citydata_out$sumjoyrat = citydata_out$sumjoy*citydata_out$rating_seen
citydata_out$sumsadrat = citydata_out$sumsad*citydata_out$rating_seen
citydata_out$sumfearrat = citydata_out$sumfear*citydata_out$rating_seen
citydata_out$sumtrustrat = citydata_out$sumtrust*citydata_out$rating_seen
citydata_out$sumsurprat = citydata_out$sumsurp*citydata_out$rating_seen

ang_rat <- aggregate(citydata_out$sumangrat, list(citydata_out$Rating), FUN=mean, na.rm=T)
ant_rat <- aggregate(citydata_out$sumantrat, list(citydata_out$Rating), FUN=mean, na.rm=T)
dis_rat <- aggregate(citydata_out$sumdisrat, list(citydata_out$Rating), FUN=mean, na.rm=T)
joy_rat <- aggregate(citydata_out$sumjoyrat, list(citydata_out$Rating), FUN=mean, na.rm=T)
sad_rat <- aggregate(citydata_out$sumsadrat, list(citydata_out$Rating), FUN=mean, na.rm=T)
fear_rat <- aggregate(citydata_out$sumfearrat, list(citydata_out$Rating), FUN=mean, na.rm=T)
trust_rat <- aggregate(citydata_out$sumtrustrat, list(citydata_out$Rating), FUN=mean, na.rm=T)
surp_rat <- aggregate(citydata_out$sumsurprat, list(citydata_out$Rating), FUN=mean, na.rm=T)

emo_rat <- ang_rat %>% inner_join(ant_rat,by = 'Group.1')
emo_rat <- emo_rat %>% inner_join(dis_rat,by = 'Group.1')
emo_rat <- emo_rat %>% inner_join(joy_rat,by = 'Group.1')
emo_rat <- emo_rat %>% inner_join(sad_rat,by = 'Group.1')
emo_rat <- emo_rat %>% inner_join(fear_rat,by = 'Group.1')
emo_rat <- emo_rat %>% inner_join(trust_rat,by = 'Group.1')
emo_rat <- emo_rat %>% inner_join(surp_rat,by = 'Group.1')

emo_rat <- emo_rat %>%
  rename("Anger"="x.x", "Anticipation"="x.y", "Disgust"="x.x.x", "Joy"="x.y.y", "Sadness"="x.x.x.x", "Fear"="x.y.y.y", 
         "Trust"="x.x.x.x.x", "Surprise"="x.y.y.y.y", "Rating"="Group.1")
emo_rat

write.csv(emo_rat, "C:\\Users\\Bovan\\OneDrive\\Documents\\MA Thesis\\Thesis\\emo_rat.csv")
