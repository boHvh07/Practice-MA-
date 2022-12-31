
library(sandwich)
library(lmtest)
library(MASS)
library(gridExtra)
library(dplyr)
### Full model OLS regression
summary(citydata_out)

olsfull <- lm(Rating ~ sumang + rating_seen + review_seen + rating_seen*sumang + review_seen*sumang +
                sumant + rating_seen*sumant + review_seen*sumant + sumjoy + rating_seen*sumjoy + review_seen*sumjoy +
                sumsad + rating_seen*sumsad + review_seen*sumsad + sumdis + rating_seen*sumdis + review_seen*sumdis +
                sumfear + rating_seen*sumfear + review_seen*sumfear + sumtrust + rating_seen*sumtrust + review_seen*sumtrust +
                sumsurp + rating_seen*sumsurp + review_seen*sumsurp
              , data = citydata_out)

summary(olsfull)

olsfull <- coeftest(olsfull, vcov. = sandwich)
olsfull <- round(olsfull, digits = 4)
olsfull

write.csv(olsfull, "C:\\Users\\Bovan\\OneDrive\\Documents\\MA Thesis\\Thesis\\olsfull.csv")

### Simple model OLS regression 
olssimp <- lm(Rating ~ sumang + sumant + sumjoy + sumsad +  sumdis + sumfear
              + sumtrust + sumsurp,
               data = citydata_out)

olssimp <- coeftest(olssimp, vcov. = sandwich)
olssimp <- round(olssimp, digits = 4)
olssimp

write.csv(olssimp, "C:\\Users\\Bovan\\OneDrive\\Documents\\MA Thesis\\Thesis\\olssimp.csv")

### OLS Fixed effects
citydata_out <- citydata_out %>%
  rename("Restname"="Restaurant Name")

olsfix <- lm(Rating ~ sumang + rating_seen + rating_seen*sumang + review_seen + review_seen*sumang +
               sumant + rating_seen*sumant + review_seen*sumant + sumjoy + rating_seen*sumjoy + review_seen*sumjoy +
               sumsad + rating_seen*sumsad + review_seen*sumsad + sumdis + rating_seen*sumdis + review_seen*sumdis +
               sumfear + rating_seen*sumfear + review_seen*sumfear + sumtrust + rating_seen*sumtrust + review_seen*sumtrust +
               sumsurp + rating_seen*sumsurp + review_seen*sumsurp + as.factor(Restname) + as.factor(city)
             , data = citydata_out)

summary(olsfix)

olsfix <- coeftest(olsfix, vcov. = sandwich)
olsfix <- round(olsfix, digits = 4)
olsfix 

write.csv(olsfix, "C:\\Users\\Bovan\\OneDrive\\Documents\\MA Thesis\\Thesis\\olsfix.csv")
