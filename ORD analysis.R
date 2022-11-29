
#### Ordinal Regression (Use polr command)
library(MASS)
polrdata <- polr(as.factor(Rating) ~ avgang + avgant + avgdis + avgjoy + avgsad
                 + avgfear + avgtrust + avgsurp + review_seen + rating_seen + avgang*review_seen + avgant*review_seen + avgdis*review_seen +
                   avgjoy*review_seen + avgsad*review_seen + avgfear*review_seen + avgtrust*review_seen + avgsurp*review_seen +
                   avgang*rating_seen + avgant*rating_seen + avgdis*rating_seen + avgjoy*rating_seen + avgsad*rating_seen + 
                   avgfear*rating_seen + avgtrust*rating_seen + avgsurp*rating_seen,
                 data = cityana_without_outliers, Hess = TRUE, method = c('logistic'))

summary(polrdata)

ols_aic(citylm2)

##### partial proportional odds model
ppo.model <- vglm(Rating ~ avgang + avgant + avgdis + avgjoy + avgsad
                                      + avgfear + avgtrust + avgsurp + review_seen + rating_seen 
                                      ,data = cityana_without_outliers, family = cumulative(parallel = F~avgant))
head(fitted(ppo.model))

summary(ppo.model)

ppo.model2 <- vglm(Rating ~ avgang + avgant + avgdis + avgjoy + avgsad
                  + avgfear + avgtrust + avgsurp + review_seen + rating_seen + avgang*review_seen + avgant*review_seen + avgdis*review_seen +
                    avgjoy*review_seen + avgsad*review_seen + avgfear*review_seen + avgtrust*review_seen + avgsurp*review_seen +
                    avgang*rating_seen + avgant*rating_seen + avgdis*rating_seen +
                    avgjoy*rating_seen + avgsad*rating_seen + avgfear*rating_seen + avgtrust*rating_seen + avgsurp*rating_seen
                  ,data = cityana_without_outliers, family = cumulative(parallel = T~avgant))

summary(ppo.model2)

ppo.model3 <- vglm(Rating ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang
                  ,data = cityana_without_outliers, family = cumulative(parallel = F))

summary(ppo.model3)

ctable <- coef(summary(ppo.model))

ci <- confint(ppo.model)
ci 

exp(coef(ppo.model))

exp(cbind(OR = coef(ppo.model), ci))

coef(ppo.model(matrix = TRUE))


#### partial nonproportional odds model
ppo.model4 <- vglm(Rating ~ avgang + avgant + avgdis + avgjoy + avgsad
                   + avgfear + avgtrust + avgsurp + review_seen + rating_seen + avgang*review_seen + avgant*review_seen + avgdis*review_seen +
                     avgjoy*review_seen + avgsad*review_seen + avgfear*review_seen + avgtrust*review_seen + avgsurp*review_seen +
                     avgang*rating_seen + avgant*rating_seen + avgdis*rating_seen +
                     avgjoy*rating_seen + avgsad*rating_seen + avgfear*rating_seen + avgtrust*rating_seen + avgsurp*rating_seen
                   ,data = cityana_without_outliers, family = cumulative(parallel = F~avgant, reverse = T))

summary(ppo.model4)
coef(ppo.model4, matrix = TRUE)
confint(ppo.model4, matrix = TRUE)
exp(coef(ppo.model4, matrix = TRUE))
exp(confint(ppo.model4, matrix = TRUE))
cbind(exp(coef(ppo.model4)), exp(confint(ppo.model4)))

AIC(ppo.model4)




####Marginal effects table after model summary
summary(cityana_without_outliers)

library(stats)
logLik(polrdata)
logLik(ppo.model4)
logLik(polrdata2)
