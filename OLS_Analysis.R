
library(sandwich)
library(lmtest)
library(MASS)
### Full model OLS regression
summary(citydata_out)

olsfull <- lm(Rating ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang +
              ant + rating_seen*ant + review_seen*ant + joy + rating_seen*joy + review_seen*joy +
              sad + rating_seen*sad + review_seen*sad + dis + rating_seen*dis + review_seen*dis +
              fear + rating_seen*fear + review_seen*fear + trust + rating_seen*trust + review_seen*trust +
              surp + rating_seen*surp + review_seen*surp
            , data = citydata_out)

olsfull <- lm(Rating ~ sumang + rating_seen + rating_seen*sumang + review_seen + review_seen*sumang +
                sumant + rating_seen*sumant + review_seen*sumant + sumjoy + rating_seen*sumjoy + review_seen*sumjoy +
                sumsad + rating_seen*sumsad + review_seen*sumsad + sumdis + rating_seen*sumdis + review_seen*sumdis +
                sumfear + rating_seen*sumfear + review_seen*sumfear + sumtrust + rating_seen*sumtrust + review_seen*sumtrust +
                sumsurp + rating_seen*sumsurp + review_seen*sumsurp
              , data = citydata_out)

summary(olsfull)

olsfull <- coeftest(olsfull, vcov. = sandwich)
olsfull <- round(olsfull, digits = 4)
olsfull

logLik(olsfull)
AIC(olsfull)
BIC(olsfull)

write.csv(olsfull, "C:\\Users\\Bovan\\OneDrive\\Documents\\MA Thesis\\Thesis\\olsfull.csv")
### Simple model OLS regression 
olssimp <- lm(Rating ~ sumang + rating_seen + review_seen + sumant + sumjoy + sumsad +  sumdis + sumfear
              + sumtrust + sumsurp,
               data = citydata_out)

olssimp <- coeftest(olssimp, vcov. = sandwich)
olssimp <- round(olssimp, digits = 4)
olssimp

logLik(olssimp)
AIC(olssimp)
BIC(olssimp)

write.csv(olssimp, "C:\\Users\\Bovan\\OneDrive\\Documents\\MA Thesis\\Thesis\\olssimp.csv")
### Fixed effects model (restaurant) OLS regression 
citydata_out <- citydata_out %>%
  rename("Restname"="Restaurant Name")

olsrest <- lm(Rating ~ sumang + rating_seen + review_seen + sumant + sumjoy + sumsad +  sumdis + sumfear
                       + sumtrust + sumsurp + as.factor(Restname)
                       , data = citydata_out)

summary(olsrest)

olsrest <- coeftest(olsrest, vcov. = sandwich)
olsrest <- round(olsrest, digits = 4)
olsrest 

logLik(olsrest)
AIC(olsrest)
BIC(olsrest)

write.csv(olsrest, "C:\\Users\\Bovan\\OneDrive\\Documents\\MA Thesis\\Thesis\\olsrest.csv")
### Fixed effects model (cities) OLS regression  
olscity <- lm(Rating ~ sumang + rating_seen + review_seen + sumant + sumjoy + sumsad +  sumdis + sumfear
                       + sumtrust + sumsurp + as.factor(city)
                       , data = citydata_out)
summary(olscity)

olscity <- coeftest(olscity, vcov. = sandwich)
olscity <- round(olscity, digits = 4)
olscity

logLik(olscity)
AIC(olscity)
BIC(olscity)

write.csv(olscity, "C:\\Users\\Bovan\\OneDrive\\Documents\\MA Thesis\\Thesis\\olscity.csv")
