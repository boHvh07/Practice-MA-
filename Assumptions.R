library(ggplot2)
library(stargazer)

bostonlm <- lm(Bostonana$Rating ~ Bostonana$avgang + Bostonana$avgant + Bostonana$avgdis + Bostonana$avgjoy + Bostonana$avgsad
               + Bostonana$avgfear + Bostonana$avgtrust + Bostonana$avgsurp)
stargazer(bostonlm)
###ASSUMPTION 1: Linearity
plot(bostonlm, which = 1)
plot(bostonlm, which = 2)
plot(bostonlm, which = 3)
plot(bostonlm, which = 5)

par(mfrow=c(2,2))
plot(bostonlm)
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