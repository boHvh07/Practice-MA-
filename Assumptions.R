library(ggplot2)
library(stargazer)

citylm <- lm(cityana$Rating ~ cityana$avgang + cityana$avgant + cityana$avgdis + cityana$avgjoy + cityana$avgsad
               + cityana$avgfear + cityana$avgtrust + cityana$avgsurp + cityana$reviews_seen + cityana$rating_seen)
stargazer(bostonlm)
###ASSUMPTION 1: Linearity
plot(citylm, which = 1)
plot(citylm, which = 2)
plot(citylm, which = 3)
plot(citylm, which = 5)

par(mfrow=c(2,2))
plot(citylm)
###ASSUMPTION 2: random sampling of observations


###ASSUMPTION 3: Conditional mean should be zero


###ASSUMPTION 4: No Multicollinearity



###ASSUMPTION 5: Homoskedacity and no Autocorrelation 

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


### Assumption 1: Error term = 0

### Assumption 2: Homoskedacity

##3 Assumption 3: Normal distribution of residuals

### Assumption 4: Independence of residuals

### Assumption 5: