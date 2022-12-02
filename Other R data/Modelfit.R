###### Ordinal
library(VGAM)
library(MASS)
library(ggeffects)
library(car)
library(margins)
library(gtsummary)

polrdata2 <- polr(as.factor(Rating) ~ sumang + sumant + sumdis + sumjoy + sumsad
                 + sumfear + sumtrust + sumsurp + review_seen + rating_seen,
                 data = cityana_without_outliers, Hess = TRUE, method = c('logistic'))


polrdata <- polr(as.factor(Rating) ~ sumang + sumant + sumdis + sumjoy + sumsad
                 + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
                   sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
                   sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen + sumjoy*rating_seen + sumsad*rating_seen + 
                   sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen,
                 data = cityana_without_outliers, Hess = TRUE, method = c('logistic'))

poTest(polrdata2)

summary(polrdata)

tbl_regression(polrdata, exp = TRUE)

margins(polrdata, variables = "sumang")

ctable <- coef(summary(polrdata))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

ci <- confint(polrdata)

ggpredict(polrdata, terms = "Rating") %>%
  plot()

exp(coef(polrdata))
exp(cbind(OR = coef(polrdata), ci))

#### Multinomial
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

multinom <- multinom(Rating ~ sumang +sumant + sumdis + sumjoy + sumsad
         + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
           sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
           sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen +
           sumjoy*rating_seen + sumsad*rating_seen + sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen
         ,data = cityana_without_outliers)
multinom


coefs <- coef(multinom)
coefs
exp(coefs)
trans_test <- (exp(coefs)-1)*100
round(trans_test, digits = 2)

z <- summary(multinom)$coefficients/summary(multinom)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
round(p, digits = 4)


#prediction of categories???
library(caret)
library(nnet)
test <- nnet::multinom(Rating ~ sumang +sumant + sumdis + sumjoy + sumsad
                 + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
                   sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
                   sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen +
                   sumjoy*rating_seen + sumsad*rating_seen + sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen
                 ,data = cityana_without_outliers)

summary(test)



#### PPO model    (probably wrong but gives no error message)
ppo.model1 <- vglm(Rating ~ sumang + sumant + sumdis + sumjoy + sumsad
                   + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
                     sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
                     sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen +
                     sumjoy*rating_seen + sumsad*rating_seen + sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen
                   ,data = cityana_without_outliers, family = cumulative(parallel = T~sumant, reverse = T))

summary(ppo.model1)

ppo.model2 <- vglm(Rating ~ sumang + sumant + sumdis + sumjoy + sumsad
                   + sumfear + sumtrust + sumsurp + review_seen + rating_seen
                   ,data = cityana_without_outliers, family = cumulative(parallel = T~sumant, reverse = T))

summary(ppo.model2)

ppo.model3 <- vglm(Rating ~ sumant + review_seen + rating_seen + sumant*review_seen + sumant*rating_seen 
                   ,data = cityana_without_outliers, family = cumulative(parallel = T ~ sumant, reverse = T))

summary(ppo.model3)

####NonPO model
npo.model1 <- vglm(Rating ~ sumang + sumdis + sumjoy + sumsad
                   + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumdis*review_seen +
                     sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
                     sumang*rating_seen + sumdis*rating_seen +
                     sumjoy*rating_seen + sumsad*rating_seen + sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen
                   ,data = cityana_without_outliers, family = cumulative)
summary(npo.model1)

npo.model2 <- vglm(Rating ~ sumang + sumdis + sumjoy + sumsad + sumfear + sumtrust +
                     sumsurp + review_seen + rating_seen ,data = cityana_without_outliers, family = cumulative(parallel = F))
summary(npo.model2)

library(stats)
logLik(ppo.model1)
logLik(npo.model2)
AICqrrvglm(ppo.model1)
AICqrrvglm(npo.model1)
