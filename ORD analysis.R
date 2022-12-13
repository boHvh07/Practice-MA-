
#### Ordinal Regression (Use polr command)
library(VGAM)
library(MASS)
polrdata <- polr(as.factor(Rating) ~ sumang + sumant + sumdis + sumjoy + sumsad
                 + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
                   sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
                   sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen + sumjoy*rating_seen + sumsad*rating_seen + 
                   sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen,
                 data = cityana_without_outliers, Hess = TRUE, method = c('logistic'))

summary(polrdata)

ols_aic(citylm2)

##### partial proportional odds model
ppo.model <- vglm(Rating ~ avgang + avgant + avgdis + avgjoy + avgsad
                                      + avgfear + avgtrust + avgsurp + review_seen + rating_seen 
                                      ,data = cityana_without_outliers, family = cumulative(parallel = F~avgant))
head(fitted(ppo.model))

summary(ppo.model)

ppo.model3 <- vglm(Rating ~ sumang + sumant + sumdis + sumjoy + sumsad
                  + sumfear + sumtrust + sumsurp + review_seen + rating_seen,
                  data = cityana_without_outliers, family = cumulative(parallel = F))

summary(ppo.model3)

ppo.model3 <- vglm(Rating ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang
                  ,data = cityana_without_outliers, family = multinomial)

summary(ppo.model3)

ctable <- coef(summary(ppo.model2))

ci <- confint(ppo.model2)
ci 

exp(coef(ppo.model2))

exp(cbind(OR = coef(ppo.model2), ci))

coef(ppo.model2(matrix = TRUE))

ppo.model4 <- vglm(as.factor(Rating) ~ sumang + sumant + sumdis + sumjoy + sumsad
                   + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
                     sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
                     sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen +
                     sumjoy*rating_seen + sumsad*rating_seen + sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen
                   ,data = cityana_without_outliers, family = cumulative(parallel = F))

summary(ppo.model4)
coef(ppo.model4, matrix = TRUE)
confint(ppo.model4, matrix = TRUE)
exp(coef(ppo.model4, matrix = TRUE))
exp(confint(ppo.model4, matrix = TRUE))
cbind(exp(coef(ppo.model4)), exp(confint(ppo.model4)))

AIC(ppo.model4)




ppo.model5 <- vglm(Rating ~ sumang + sumant + sumdis + sumjoy + sumsad
                   + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
                     sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
                     sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen +
                     sumjoy*rating_seen + sumsad*rating_seen + sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen
                   ,data = cityana_without_outliers, family = cumulative(parallel = T~sumant, reverse = T))


ppo.model6 <- vglm(Rating ~ sumang + sumant + sumdis + sumjoy + sumsad
                   + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
                     sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
                     sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen +
                     sumjoy*rating_seen + sumsad*rating_seen + sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen
                   ,data = cityana_without_outliers, family = cumulative(parallel = F~ sumjoy, reverse = T))

summary(ppo.model6)


ppo.model7 <- vglm(Rating ~ sumang + review_seen + rating_seen + sumang*review_seen + sumang*rating_seen 
                   ,data = cityana_without_outliers, family = cumulative(parallel = F~ sumang + review_seen + rating_seen, reverse = T))

summary(ppo.model7)



####Marginal effects table after model summary
summary(cityana_without_outliers)

library(stats)
logLik(polrdata)
logLik(ppo.model4)
logLik(polrdata2)



polrdata2 <- polr(as.factor(Rating) ~ ang + dis + joy + sad
                 + fear + trust + surp + review_seen + rating_seen + ang*review_seen + dis*review_seen +
                   joy*review_seen + sad*review_seen + fear*review_seen + trust*review_seen + surp*review_seen +
                   ang*rating_seen + dis*rating_seen + joy*rating_seen + sad*rating_seen + 
                   fear*rating_seen + trust*rating_seen + surp*rating_seen,
                 data = cityana_without_outliers, Hess = TRUE, method = c('logistic'))

summary(polrdata2)
