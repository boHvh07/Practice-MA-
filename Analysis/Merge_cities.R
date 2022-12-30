worddata <- rbind(city_chi, city_Hou, city_la, city_ny)
worddata <- worddata[!(worddata$review_seen > 200),]

citydata <- rbind(hou_ana, chi_ana, la_ana, ny_ana)
citydata <- citydata[!(citydata$review_seen > 200),]
