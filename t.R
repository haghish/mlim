library(mlim)
data(iris)

# Add artifitial missing data
# ===========================================================
irisNA <- iris
irisNA <- mlim.na(iris, p = 0.1, seed = 2022)


h2o::h2o.shutdown(F)
# ELNET Imputation with mlim
# ===========================================================
ELNET <- mlim(irisNA, m=5, init = TRUE, maxiter=10, seed = 2022,
              tuning_time = 60, doublecheck = T)

(mlimELNETerror <- mlim.error(ELNET, irisNA, iris))
#View(cbind(irisNA[1], ELNET[[1]][1], ELNET[[2]][1])) #check the imps

View(met <- attributes(ELNET[[2]])$metrics)


ELNET2 <- mlim(irisNA, init = TRUE, maxiter = 10, seed = 2022,
               impute = "ELNET",
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



library(miss)
data(orange)
resMI <- MIPCA(orange, ncp=2, nboot = 1)
resMI$res.MI
resMI$res.imputePCA
resMI$call



data(ozone)
res <-MIPCA(ozone[,1:11], ncp=2, nboot = 5)
res$res.MI #gives array of imputed datasets
res$res.imputePCA #gives 1 dataset
res$call

require(mice)
imp<-missMDA::prelim(res.mi=res, X=ozone[,1:11]) #creating a mids object from missMDA
fit <- with(data=imp, exp=lm(maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v))#analysis
res.pool<-pool(fit)
summary(res.pool)#pooling

a <- res
a$res.imputePCA <- NULL
a$call <- NULL
b<- missMDA::prelim(res.mi=a, X=ozone[,1:11]) #creating a mids object from missMDA
fit <- with(data=b, exp=lm(maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v))#analysis
res.pool<-pool(fit)
summary(res.pool) #pooling

## Diagnostics
res.over<-Overimpute(res.BayesMIPCA)

a <- list()
a[[1]] <- 10
a[[2]] <- 20
a
