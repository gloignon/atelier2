# demo_sem.R
#
# Démonstrations de modélisation par équation structurelles
# 
# Compilation d'exemples utilisant le package lavaan et production de graphes
# avec le package semPlot.
#
# En prime: le dernier exemple permet de tester l'intégration de R et Mplus
#
# Adapté par G. Loignon
# Mars 2019
# guillaume.loignon@umontreal.ca

library(lavaan) #pour l'essentiel de notre démo
library(semPlot) #on veut aussi des graphiques

#---- Démo 1 ----
# Adapté depuis https://quantdev.ssri.psu.edu/tutorials/structural-equation-modeling-r-using-lavaan

#0. simulation
demo.model <- '
y ~ .5*f  #strength of regression with external criterion

f =~ .8*x1 + .8*x2 + .8*x3 + .8*x4 + .8*x5  #definition of factor f with loadings on 5 items

x1 ~~ (1-.8^2)*x1 #residual variances. Note that by using 1-squared loading, we achieve a total variability of 1.0 in each indicator (standardized)
x2 ~~ (1-.8^2)*x2
x3 ~~ (1-.8^2)*x3
x4 ~~ (1-.8^2)*x4
x5 ~~ (1-.8^2)*x5
'

# generate data; note, standardized lv is default
simData <- simulateData(demo.model, sample.nobs=200)

#look at the data
head(simData)

#1. spécification du modèle
tofit.model <- '
y ~ f # "~ is regressed on"
f =~ x1+ x2 + x3 + x4 + x5 # "=~ is measured by"
x1 ~~ x1 # variance
x2 ~~ x2 #variance
x3~~x3 #variance
x4~~x4 #variance
x5~~x5 #variance
#x4~~x5 would be an example of covariance
'

#2. fit et vérification
tofit.model_m <- sem(tofit.model, simData)
summary(tofit.model_m, fit.measures = TRUE)
inspect(tofit.model_m)

#3. plot (requiert package semPlot)
semPaths(tofit.model_m)

#--- Démo 2 ----
# Reproduit depuis la documentation du package semPlot
#  voir https://cran.r-project.org/web/packages/semPlot/semPlot.pdf

# A silly dataset:
X <- rnorm(100)
Y <- rnorm(100)
Z <- rnorm(1)*X + rnorm(1)*Y + rnorm(1)*X*Y
DF <- data.frame(X,Y,Z)
# Regression including interaction:
res <- lm(Z ~ X*Y, data = DF)
# Path diagram:
semPaths(res, intAtSide=TRUE)

# Standardized estimates:
semPaths(res,"std","hide", intAtSide=TRUE)

#--- Démo 3 ----
## The famous Holzinger and Swineford (1939) example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data=HolzingerSwineford1939)
summary(fit, fit.measures=TRUE)
semPaths(fit, 'std', 'est', curveAdjacent = TRUE, style = "lisrel")

#---- Démo 4 ----
# Example 5.8 from mplus user guide:
Data <- read.table("http://www.statmodel.com/usersguide/chap5/ex5.2.dat")
names(Data) <- c("u1","u2","u3","u4","u5","u6")
Data <- as.data.frame(lapply(Data, ordered))
# Lavaan model:
model <- ' f1 =~ u1 + u2 + u3; f2 =~ u4 + u5 + u6 '
# Run Lavaan:
fit <- lavaan::cfa(model, data=Data)
# Plot path diagram:
semPaths(fit,intercepts=FALSE) #dessine le graphe, requiert package semPlot

## Mplus: requiert package MplusAutomation et logiciel MPlus
# Same model, now using mplus output:
outfile <- tempfile(fileext=".out")
download.file("http://www.statmodel.com/usersguide/chap5/ex5.2.out",outfile)
# Plot model:
semPaths(outfile) #requiert MPlus et MPlusAutomation
