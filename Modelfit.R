###### Ordinal
library(VGAM)
library(MASS)
polrdata <- polr(as.factor(Rating) ~ sumang + sumant + sumdis + sumjoy + sumsad
                 + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
                   sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
                   sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen + sumjoy*rating_seen + sumsad*rating_seen + 
                   sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen,
                 data = cityana_without_outliers, Hess = TRUE, method = c('logistic'))

summary(polrdata)


#### Multinomial
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

multinom(Rating ~ sumang +sumant + sumdis + sumjoy + sumsad
         + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
           sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
           sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen +
           sumjoy*rating_seen + sumsad*rating_seen + sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen
         ,data = cityana_without_outliers)


#### PPO model    (probably wrong but gives no error message)
ppo.model3 <- vglm(Rating ~ sumang + sumant + sumdis + sumjoy + sumsad
                   + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
                     sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
                     sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen +
                     sumjoy*rating_seen + sumsad*rating_seen + sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen
                   ,data = cityana_without_outliers, family = cumulative(parallel = F~sumant, reverse = F))

summary(ppo.model3)