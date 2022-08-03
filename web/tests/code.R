# Comparison of different R packages imputing iris dataset
# ===========================================================
rm(list = ls())
library(mlim)
library(mice)
library(missForest)
library(missRanger)
data(iris)

# Add artifitial missing data
# ===========================================================
irisNA <- missRanger::generateNA(iris, p = 0.1, seed = 2022)

library(VIM)
kNN <- kNN(irisNA, imp_var=FALSE)
(kNNerror <- mixError(kNN, irisNA, iris))

horDeck <- hotdeck(irisNA, imp_var=FALSE)
(kNNerror <- mixError(horDeck, irisNA, iris))

IRMI <- irmi(irisNA, imp_var=FALSE, trace = FALSE)
(IRMIerror <- mixError(IRMI, irisNA, iris))

View(kNN)

dim(iris)
dim(kNN)
