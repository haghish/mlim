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
irisNA <- missRanger::generateNA(iris, p = 0.5, seed = 2022)

# ELNET Imputation with mlim
# ===========================================================
mlimELNET <- mlim(irisNA, init = TRUE, maxiter = 10,
                  include_algos = "ELNET", preimpute = "knn",
                  report = "mlimELNET.log", verbosity = "debug",
                  max_models = 1, min_mem_size = "6G", nthreads = 1,
                  max_mem_size = "8G", iteration_stopping_tolerance = .01,
                  shutdown = TRUE, flush=FALSE, seed = 2022)
(mlimELNETerror <- mixError(mlimELNET, irisNA, iris))

h2o::h2o.shutdown(F)
ELNETmtch <- mlim(irisNA, init = TRUE, maxiter = 10, matching = TRUE,
                  include_algos = "ELNET", preimpute = "knn",
                  report = "mlimELNET.log", verbosity = "debug",
                  max_models = 1, min_mem_size = "6G", nthreads = 1,
                  max_mem_size = "8G", iteration_stopping_tolerance = .01,
                  shutdown = TRUE, flush=FALSE, seed = 2022)
(ELNETmtchError <- mixError(ELNETmtch, irisNA, iris))

# kNN Imputation with VIM
# ===========================================================
kNN <- kNN(irisNA, imp_var=FALSE)
(kNNerror <- mixError(kNN, irisNA, iris))

# MICE Imputation with mice (10 datasets)
# ===========================================================
m <- 10
mc <- mice(irisNA, m=m, maxit = 50, method = 'pmm', seed = 500)
MCerror <- NULL
for (i in 1:m) MCerror <- c(MCerror, mixError(complete(mc,i), irisNA, iris)[1])
(MCerror <- mean(MCerror))

# Random Forest Imputation with missForest
# ===========================================================
set.seed(2022)
RF <- missForest(irisNA)
(RFerror <- mixError(RF$ximp, irisNA, iris))

rngr <- missRanger(irisNA, num.trees=100, seed = 2022)
(missRanger <- mixError(rngr, irisNA, iris))

# Create the plot
# ===========================================================
plotdata <- data.frame(Error = c(MCerror, RFerror[1], missRanger[1],
                                 kNNerror[1], mlimELNETerror[1]),
                       Algorithms = c("MICE", "missForest", "missRanger",
                                      "kNN", "MLIM ELNET"))
library(ggplot2)
ggplot(plotdata, aes(x=Algorithms, y=Error, fill=Algorithms)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  ggtitle("Comparison of ML Imputation Algorithms") +
  theme_minimal() +
  ylab("NRMSE") +
  theme(legend.position = "none")



