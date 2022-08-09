rm(list = ls())
library(mlim)
library(mice)
library(missForest)
library(missRanger)
library(VIM)
library(psych)
library(tictoc)
data("sat.act")

# Prepare dataset
# ===========================================================
df <- na.omit(sat.act)
df$gender <- factor(df$gender, levels = 1:2, labels = c("Male","Female"))
df$education <- factor(df$education, levels = 1:5,
                       labels = c("HS","finished HS", "some college",
                                  "college graduate","graduate degree"),
                       ordered = TRUE)

# Add artifitial missing data to ORDINAL variables
# ===========================================================
dfNA <- df
dfNA <- missRanger::generateNA(df, p = 0.1, seed = 2022)

# ELNET Imputation with mlim
# ===========================================================
ELNET <- mlim(dfNA, init = TRUE, maxiter = 10,
                  include_algos = "ELNET",
                  report = "mlimELNET.log", verbosity = "debug",
                  max_models = 1, min_mem_size = "15G", max_mem_size = "17G",
                  #nthreads = 1,
                  iteration_stopping_tolerance = .01,
                  shutdown = TRUE, flush=FALSE, seed = 2022)

(ELNETerr <-mlim.error(ELNET, dfNA, df))

# DRF Imputation with mlim
# ===========================================================
DRF <- mlim(dfNA, init = TRUE, maxiter = 10,
                   include_algos = "DRF", preimpute = "ranger",
                   report = "mlimDRF.log", verbosity = "debug",
                   min_mem_size = "15G", max_mem_size = "17G",
                   #nthreads = 1,
                   iteration_stopping_tolerance = .01,
                   shutdown = TRUE, flush=FALSE, seed = 2022)
(DRFerr <-mlim.error(DRF, dfNA, df))

# kNN Imputation with VIM
# ===========================================================
set.seed(2022)
tic()
kNN <- kNN(dfNA, imp_var=FALSE)
toc()
#View(cbind(kNN$A1[index], df$A1[index]))
(kNNerror <- mlim.error(kNN, dfNA, df))
(kNNerror <- mlim.error(kNN, dfNA, df, ignore.rank = TRUE))


# MICE Imputation with mice (10 datasets)
# ===========================================================
m <- 5
mc <- mice(dfNA, m=m, maxit = 50, method = 'pmm', seed = 2022)
nrmse <- NULL
missclass <- NULL
missrank <- NULL
for (i in 1:m) nrmse <- c(nrmse, mlim.error(complete(mc,i), dfNA, df)[1])
for (i in 1:m) missclass <- c(missclass, mlim.error(complete(mc,i), dfNA, df)[2])
for (i in 1:m) missrank <- c(missrank, mlim.error(complete(mc,i), dfNA, df)[3])
(nrmse <- mean(nrmse))
(missclass <- mean(missclass))
(missrank <- mean(missrank))

# Random Forest Imputation with missForest
# ===========================================================
set.seed(2022)
tic()
RF <- missForest(dfNA)
toc()
(RFerror <- mlim.error(RF$ximp, dfNA, df))
(RFerror <- mlim.error(RF$ximp, dfNA, df, ignore.rank=TRUE))

tic()
rngr <- missRanger(dfNA, seed = 2022)
toc()
(missRanger <- mlim.error(rngr, dfNA, df))
(missRanger <- mlim.error(rngr, dfNA, df, ignore.rank=TRUE))

# SAVE IMAGE
# ===========================================================
save.image(file="sat.act_10.RData")


# Create the plot
# ===========================================================
plotdata <- data.frame(Error = c(nrmse, RFerror[1], missRanger[1],
                                 kNNerror[1], ELNETerr[1]),
                       Algorithms = c("MICE", "missForest", "missRanger",
                                      "kNN", "MLIM"))
library(ggplot2)
ggplot(plotdata, aes(x=Algorithms, y=Error, fill=Algorithms)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  ggtitle("Comparison of ML Imputation Algorithms") +
  theme_minimal() +
  ylab("NRMSE") +
  ggtitle("Imputing continuous variables (sat.act dataset from psych R package)") +
  theme(legend.position = "none")
