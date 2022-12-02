#### remove outliers
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
citylmnon <- lm(cityana$Rating ~ cityana$avgang + cityana$avgant + cityana$avgdis + cityana$avgjoy + cityana$avgsad
                + cityana$avgfear + cityana$avgtrust + cityana$avgsurp + cityana$review_seen + cityana$rating_seen, 
                data = cityana)

library(car)
vif(citylmnon)

vif_values <- vif(citylmnon)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)


citycor <- cityana_without_outliers[ ,c('Rating', 'avgang', 'avgant', 'avgdis', 'avgjoy', 'avgsad'
                                        , 'avgfear', 'avgtrust', 'avgsurp', 'review_seen', 'rating_seen')]
cor(citycor, use = 'complete.obs')


### Assumption 4: Proportional odds (Brant's test)
cityord <- polr(as.factor(Rating) ~ avgang + avgant + avgdis + avgjoy + avgsad
                + avgfear + avgtrust + avgsurp + review_seen + rating_seen, 
                data = cityana_without_outliers, Hess = TRUE, method = c('logistic'))

library(brant)
brant(cityord)

Assumption is violated -> partial proportional odds models