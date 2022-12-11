
#### OLS regression indiv. Tryout

ols_regress(Rating ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang, data = cityana)
ols_regress(Rating ~ ant + rating_seen + rating_seen*ant + review_seen + review_seen*ant, data = cityana)
ols_regress(Rating ~ joy + rating_seen + rating_seen*joy + review_seen + review_seen*joy, data = cityana)
ols_regress(Rating ~ sad + rating_seen + rating_seen*sad + review_seen + review_seen*sad, data = cityana)
ols_regress(Rating ~ dis + rating_seen + rating_seen*dis + review_seen + review_seen*dis, data = cityana)
ols_regress(Rating ~ fear + rating_seen + rating_seen*fear + review_seen + review_seen*fear, data = cityana)
ols_regress(Rating ~ trust + rating_seen + rating_seen*trust + review_seen + review_seen*trust, data = cityana)
ols_regress(Rating ~ surp + rating_seen + rating_seen*surp + review_seen + review_seen*surp, data = cityana)



library(sandwich)
library(lmtest)
### Full model OLS regression
summary(cityana_without_outliers)

olsfull <- lm(Rating ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang +
              ant + rating_seen*ant + review_seen*ant + joy + rating_seen*joy + review_seen*joy +
              sad + rating_seen*sad + review_seen*sad + dis + rating_seen*dis + review_seen*dis +
              fear + rating_seen*fear + review_seen*fear + trust + rating_seen*trust + review_seen*trust +
              surp + rating_seen*surp + review_seen*surp
            , data = cityana_without_outliers)

summary(olsfull)

coeftest(olsfull, vcov. = sandwich)

### Simple model OLS regression 
olssimp <- lm(Rating ~ sumang + rating_seen + review_seen + sumant + sumjoy + sumsad +  sumdis + sumfear + sumtrust + sumsurp
               , data = cityana_without_outliers)

summary(olssimp)

olssimp2 <- coeftest(olssimp, vcov. = sandwich)

anova(olssimp)

### Fixed effects model (restaurant) OLS regression 
cityana_without_outliers <- cityana_without_outliers %>%
  rename("Restname"="Restaurant Name")

olsrest <- lm(Rating ~ sumang + rating_seen + review_seen + sumant + sumjoy + sumsad +  sumdis + sumfear
                       + sumtrust + sumsurp + as.factor(Restname)
                       , data = cityana_without_outliers)

summary(olsrest)

coeftest(olsrest, vcov. = sandwich)

### Fixed effects model (cities) OLS regression  
olscity <- lm(Rating ~ sumang + rating_seen + review_seen + sumant + sumjoy + sumsad +  sumdis + sumfear
                       + sumtrust + sumsurp + as.factor(city)
                       , data = cityana_without_outliers)
olscity

coeftest(olscity, vcov. = sandwich)
