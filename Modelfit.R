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

test <- multinom(Rating ~ sumang +sumant + sumdis + sumjoy + sumsad
         + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
           sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
           sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen +
           sumjoy*rating_seen + sumsad*rating_seen + sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen
         ,data = cityana_without_outliers)
test


coefs <- coef(test)
coefs
exp(coefs)
trans_test <- (exp(coefs)-1)*100
round(trans_test, digits = 2)

z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
round(p, digits = 4)


prediction of categories

#### PPO model    (probably wrong but gives no error message)
ppo.model3 <- vglm(Rating ~ sumang + sumant + sumdis + sumjoy + sumsad
                   + sumfear + sumtrust + sumsurp + review_seen + rating_seen + sumang*review_seen + sumant*review_seen + sumdis*review_seen +
                     sumjoy*review_seen + sumsad*review_seen + sumfear*review_seen + sumtrust*review_seen + sumsurp*review_seen +
                     sumang*rating_seen + sumant*rating_seen + sumdis*rating_seen +
                     sumjoy*rating_seen + sumsad*rating_seen + sumfear*rating_seen + sumtrust*rating_seen + sumsurp*rating_seen
                   ,data = cityana_without_outliers, family = cumulative(parallel = F~sumant, reverse = F))

summary(ppo.model3)