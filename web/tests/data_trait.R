# Comparison of different R packages imputing iris dataset
# ===========================================================
rm(list = ls())
library(mlim)
library(mice)
library(missForest)
library(missRanger)
library(VIM)

# make the trait dataset ordinal
# ===========================================================
trait <- readstata13::read.dta13("./inst/trait.dta")
for (i in 1:ncol(trait)) {
  trait[,i] <- as.ordered(trait[,i])
}

facmem <- function(x) {
  lev <- levels(x)
  print(lev)
}


# Add artifitial missing data
# ===========================================================
traitNA <- missRanger::generateNA(trait, p = 0.25, seed = 2022)

# ELNET Imputation with mlim
# ===========================================================
mlimELNET <- mlim(traitNA, init = TRUE, maxiter = 10,
                  include_algos = "ELNET", preimpute = "knn",
                  report = "mlimELNET.log", verbosity = "debug",
                  max_models = 1, min_mem_size = "6G", nthreads = 1,
                  iteration_stopping_tolerance = .01,
                  shutdown = TRUE, flush=FALSE, seed = 2022)
(mlimELNETerror <- mixError(mlimELNET, traitNA, trait))

# ELNET Imputation with mlim
# ===========================================================
asInteger <- mlim(traitNA, init = TRUE, maxiter = 10,
                  ordinal_as_integer = TRUE, matching = FALSE,
                  include_algos = "ELNET", preimpute = "knn",
                  report = "mlimELNET.log", verbosity = "debug",
                  max_models = 1, min_mem_size = "6G", nthreads = 1,
                  iteration_stopping_tolerance = .01,
                  shutdown = TRUE, flush=FALSE, seed = 2022)


mem <- factmem(trait)
rev <- revert(round(asInteger), mem)
(asIntegererror <- mixError(rev, traitNA, trait))

# kNN Imputation with VIM
# ===========================================================
kNN <- kNN(traitNA, imp_var=FALSE)
(kNNerror <- mixError(kNN, traitNA, trait))

# MICE Imputation with mice (10 datasets)
# ===========================================================
m <- 10
mc <- mice(traitNA, m=m, maxit = 50, method = 'pmm', seed = 500)
MCerror <- NULL
for (i in 1:m) MCerror <- c(MCerror, mixError(complete(mc,i), traitNA, trait)[1])
(MCerror <- mean(MCerror))

# Random Forest Imputation with missForest
# ===========================================================
set.seed(2022)
RF <- missForest(traitNA)
(RFerror <- mixError(RF$ximp, traitNA, trait))

rngr <- missRanger(traitNA, num.trees=100, seed = 2022)
(missRanger <- mixError(rngr, traitNA, trait))

# Create the plot
# ===========================================================
plotdata <- data.frame(Error = c(MCerror, RFerror[1], missRanger[1],
                                 kNNerror[1], asIntegererror[1]),
                       Algorithms = c("MICE", "missForest", "missRanger",
                                      "kNN", "MLIM ELNET"))
library(ggplot2)
ggplot(plotdata, aes(x=Algorithms, y=Error, fill=Algorithms)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  ggtitle("Comparison of ML Imputation Algorithms") +
  theme_minimal() +
  ylab("False classification") +
  ggtitle("Imputing ordinal variables") +
  theme(legend.position = "none")



