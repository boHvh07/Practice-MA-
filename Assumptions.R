library(ggplot2)
library(stargazer)
library(dplyr)

citylmnon <- lm(cityana$Rating ~ cityana$avgang + cityana$avgant + cityana$avgdis + cityana$avgjoy + cityana$avgsad
               + cityana$avgfear + cityana$avgtrust + cityana$avgsurp + cityana$review_seen + cityana$rating_seen, 
               data = cityana)

citylm <- lm(Rating ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang +
               ant + rating_seen*ant + review_seen*ant + joy + rating_seen*joy + review_seen*joy +
               sad + rating_seen*sad + review_seen*sad + dis + rating_seen*dis + review_seen*dis +
               fear + rating_seen*fear + review_seen*fear + trust + rating_seen*trust + review_seen*trust +
               surp + rating_seen*surp + review_seen*surp
             , data = cityana)
citylm
stargazer(citylm)
summary(citylm)
### REMOVE influential cases (cook's D)
ols_plot_cooksd_chart(citylm, print_plot = T)

cooksD <- cooks.distance(citylm)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

names_of_influential <- names(influential)
outliers <- cityana[names_of_influential,]

cityana_without_outliers <- cityana %>% anti_join(outliers)

citylm2 <- lm(Rating ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang +
                   ant + rating_seen*ant + review_seen*ant + joy + rating_seen*joy + review_seen*joy +
                   sad + rating_seen*sad + review_seen*sad + dis + rating_seen*dis + review_seen*dis +
                   fear + rating_seen*fear + review_seen*fear + trust + rating_seen*trust + review_seen*trust +
                   surp + rating_seen*surp + review_seen*surp
                 , data = cityana_without_outliers)
ols_plot_cooksd_chart(citylm2, print_plot = T)
summary(citylm2)
###ASSUMPTION 1: Linearity
plot(citylm, which = 1)
cor(cityana_without_outliers)

###ASSUMPTION 2: independence of residual values (Durbin-Watson test)


###ASSUMPTION 3: Conditional mean should be zero


###ASSUMPTION 4: No Multicollinearity (variance of inflation (VIF))
library(car)
vif(citylmnon)

vif_values <- vif(citylmnon)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)

cor(cityana_without_outliers)

###ASSUMPTION 5: Homoskedacity and no Autocorrelation (plot variance of residuals)
plot(citylm2, which = 1)
plot(citylm2, which = 3)

### Normality of residuals??? (kolmogorov-Smirnov and Shapiro-Wilk)
hist(citylm2$residuals)
plot(citylm2, which = 2)

ks.test(cityana_without_outliers, 'pnorm')

library(olsrr)
ols_test_normality(citylmnon)





plot(citylm2, which = 1)
plot(citylm2, which = 2)
plot(citylm2, which = 3)
plot(citylm2, which = 5)

par(mfrow=c(2,2))
plot(citylm2)