# Comparison of different R packages imputing iris dataset
# ===========================================================
rm(list = ls())
library(mlim)
library(mice)
library(missForest)
library(missRanger)
library(VIM)

# Add artifitial missing data
# ===========================================================
irisNA <- missRanger::generateNA(iris, p = 0.1, seed = 2022)

# GBM Imputation with mlim (nearly 40 minutes of runtime)
# ===========================================================
mlimGBM <- mlim(irisNA, init = TRUE, miniter = 1, maxiter = 1,
             include_algos = "GBM", preimpute = "knn",
             report = "mlimGBM.log", verbosity = "debug",
             max_models = 100, min_mem_size = "120G", nthreads = 21,
             iteration_stopping_tolerance = .01,
             seed = 2022)
(mlimGBMerror <- mixError(mlimGBM, irisNA, iris))

# ELNET Imputation with mlim
# ===========================================================
mlimELNET <- mlim(irisNA, init = TRUE, maxiter = 1,
                include_algos = "ELNET", preimpute = "knn",
                report = "mlimGBM.log", verbosity = "debug",
                max_models = 1, min_mem_size = "120G", nthreads = 21,
                seed = 2022)
(mlimELNETerror <- mixError(mlimELNET, irisNA, iris))

# kNN Imputation with VIM
# ===========================================================
kNN <- kNN(irisNA, imp_var=FALSE)
(kNNerror <- mixError(kNN, irisNA, iris))

# irmi Imputation with VIM
# ===========================================================
IRMI <- irmi(irisNA, imp_var=FALSE)
IRMIerror <- mixError(IRMI, irisNA, iris)

# MICE Imputation with mice (50 datasets)
# ===========================================================
mc <- mice(irisNA, m=50, maxit = 50, method = 'pmm', seed = 500)
MCerror <- NULL
for (i in 1:50) MCerror <- c(MCerror, mixError(complete(mc,i), irisNA, iris)[1])
MCerror <- mean(MCerror)

# Random Forest Imputation with missForest
# ===========================================================
RF <- missForest(irisNA)
(RFerror <- mixError(RF$ximp, irisNA, iris))

rngr <- missRanger(irisNA, seed = 2021)
(missRanger <- mixError(rngr, irisNA, iris))





