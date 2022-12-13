#### remove outliers
citylm <- lm(Rating ~ ang + rating_seen + rating_seen*ang + review_seen + review_seen*ang +
               ant + rating_seen*ant + review_seen*ant + joy + rating_seen*joy + review_seen*joy +
               sad + rating_seen*sad + review_seen*sad + dis + rating_seen*dis + review_seen*dis +
               fear + rating_seen*fear + review_seen*fear + trust + rating_seen*trust + review_seen*trust +
               surp + rating_seen*surp + review_seen*surp
             , data = cityana)

library(olsrr)
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


#### Assumption 1: DV is ordinal

### Assumption 2: IV are continuous, ordinal or interval

### Assumption 3: No multi-collinearity (VIF and correlation)
citylmnon <- lm(Rating ~ ang + ant + dis + joy + sad
                + fear + trust + surp + review_seen + rating_seen, 
                data = cityana_without_outliers)

library(car)
vif()

vif_values <- vif(citylmnon)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)


citycor <- cityana_without_outliers[ ,c('Rating', 'ang', 'ant', 'dis', 'joy', 'sad'
                                        , 'fear', 'trust', 'surp', 'review_seen', 'rating_seen')]
cor(citycor, use = 'complete.obs')


### Assumption 4: Proportional odds (Brant's test)
library(MASS)

polrdata2 <- polr(as.factor(Rating) ~ sumang + sumant + sumdis + sumjoy + sumsad
                  + sumfear + sumtrust + sumsurp + review_seen + rating_seen,
                  data = cityana_without_outliers, Hess = TRUE, method = c('logistic'))

poTest(polrdata2, digits = 6)

understanding 
Assumption is violated -> partial proportional odds models