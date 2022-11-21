#### Term frequency


### Word clouds
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)


### OLS regression


### Moderation analysis using Lavaan
Model 4: First-Stage Moderation
#-------------------------------------------------------------------------------\n")
library(haven)      # Read and write Stata, SPSS, or SAS data file 
library(lavaan)     # LAtent VAriable ANalysis 
library(semPlot)

model.4 <- "                                   
m1 ~ a1*x + a2*w + a3*xw  
y  ~ b1*m1 + b2*w + b3*xw + cp*x   
a1b1           := a1*b1               # Indirect effect of x on y via m
a3b1           := a3*b1               # Conditional indirect effect of xw on y 
cprime         := cp                  # Conditional direct effect of x on y
total          := a1*b1 + a3*b1 + cp  # Total effect of x on y
"
moderation.1 <- sem(model.4, data=data, se="bootstrap", bootstrap = 1000)  
summary(moderation.1, ci=T, standardized=T, rsquare=T, fit.measures=F) 

cat("

semPaths(mediation.1,              # Diagram of mediation.1
         whatLabels = "name",      # "name" of the path. Other options "est" (estimates) or "stand" (standardized est)
         rotation = 2,             # Rotation = 2 locates X on the left of the diagram
         asize = 5,                # Arrow-head size
         sizeMan=10,               # Size of boxes
         edge.label.position=.65,  # Relative position of label on arrow. Default = .5
         edge.label.cex=1.5,       # Size of labels on the paths (= edges,
         residuals = FALSE)        # Hide residuals to reduce visual clutter
         
semPaths(moderation.1,whatLabels="est",rotation=2,asize=5,sizeMan=10,
         edge.label.position=.65, edge.label.cex=1.5, residuals = FALSE) 

### Post test?