library(mlim)
data(iris)

# Add artifitial missing data
# ===========================================================
irisNA <- iris
irisNA <- mlim.na(iris, p = 0.1, seed = 2022)


h2o::h2o.shutdown(F)
# ELNET Imputation with mlim
# ===========================================================
ELNET <- mlim(irisNA, init = TRUE, maxiter = 10, seed = 2022,
              impute = "AUTO", postimpute=NULL,
              doublecheck = T)
(mlimELNETerror <- mlim.error(ELNET, irisNA, iris))

ELNET2 <- mlim(irisNA, init = TRUE, maxiter = 10, seed = 2022,
               impute = "ELNET", postimpute="DRF",
               doublecheck = T)
(mlimELNETerror2 <- mlim.error(ELNET2, irisNA, iris))

ELNET3 <- mlim(irisNA, init = TRUE, maxiter = 10, seed = 2022,
               impute = "ELNET", postimpute=c("DRF", "GBM"),
               doublecheck = T)
(mlimELNETerror3 <- mlim.error(ELNET3, irisNA, iris))

met <- attributes(ELNET)$metrics
View(met)

set.seed(2022)
RF <- missForest::missForest(irisNA)
(RFerror <- mlim.error(RF$ximp, irisNA, iris))


rngr <- missRanger::missRanger(irisNA, num.trees=500,
                               seed = 2022, mtry=1,
                               verbose = 0, returnOOB=TRUE,
                               maxiter = 10)
#attributes(rngr)$oob
(missRanger <- mlim.error(rngr, irisNA, iris))


