setwd("~/Documents/GitHub/mlim/check/mlim vs mice")
source("./functions.R")


library(mlimlight)
library(mice)
data("iris")

# add completely at random missingness at 15% rate
# ==================================================
df <- iris
dfNA <- mlim.na(df, p = .15, stratify = FALSE, seed = 2022)
#names(dfNA) <- c("SepalLength", "SepalWidth",  "PetalLength", "PetalWidth",  "Species")
#readstata13::save.dta13(dfNA, file = "iris.dta")

mlim.random <- mlim(data=dfNA, m=1, algos = c("ELNET"),
                    flush = TRUE,
                    tuning_time = 30, ram = 8, cpu = 1, seed=2022,
                    preimpute = "random", tolerance = 0.0001)



# MLIM - ELNET + random preimputation
# ==================================================
mlim.random <- mlim(data=dfNA, m=20, algos = c("ELNET", "GBM", "RF"),
                    flush = TRUE,
                    tuning_time = 30, ram = 20, cpu = 8, seed=2022,
                    preimpute = "random", tolerance = 0.00001)

mlim.random.results <- evaluate.imputation(
  imputed = mlim.random,
  incomplete = dfNA,
  complete = df
)

mlim.random.results$accuracy.summary
mlim.random.results$structure.summary


# MLIM - ELNET + mm preimputation
# ==================================================
mlim.mm <- mlim(data=dfNA, m=20, algos = c("ELNET", "GBM", "RF"),
                flush = TRUE,
                tuning_time = 30, ram = 20, cpu = 8, seed=2022,
                preimpute = "mm",
                tolerance = 0.00001)

mlim.mm.results <- evaluate.imputation(
  imputed = mlim.mm,
  incomplete = dfNA,
  complete = df
)

mlim.mm.results$accuracy.summary
mlim.mm.results$structure.summary



# prepare mice methods for mixed data
# ==================================================
#
# Regression:
# numeric = norm
# binary = logreg
# ordinal = polr
# nominal = polyreg
#
# PMM:
# numeric = pmm
# binary = logreg
# ordinal = polr
# nominal = polyreg
#
# RF:
# rf for all incomplete variables
# ==================================================

mice.regression.method <- mice::make.method(dfNA)
mice.pmm.method <- mice::make.method(dfNA)
mice.rf.method <- mice::make.method(dfNA)

for (v in names(dfNA)) {
  if (is.numeric(df[[v]])) {
    mice.regression.method[v] <- "norm"
    mice.pmm.method[v] <- "pmm"
    mice.rf.method[v] <- "rf"
  } else if (is.ordered(df[[v]])) {
    mice.regression.method[v] <- "polr"
    mice.pmm.method[v] <- "polr"
    mice.rf.method[v] <- "rf"
  } else if (is.factor(df[[v]]) && nlevels(df[[v]]) == 2) {
    mice.regression.method[v] <- "logreg"
    mice.pmm.method[v] <- "logreg"
    mice.rf.method[v] <- "rf"
  } else if (is.factor(df[[v]])) {
    mice.regression.method[v] <- "polyreg"
    mice.pmm.method[v] <- "polyreg"
    mice.rf.method[v] <- "rf"
  }
}

# MICE regression
# ==================================================
mc.regression <- mice(dfNA, m = 50,
                      method = mice.regression.method,
                      maxit = 10, seed = 2022)

mc.regression.list <- complete(mc.regression, action = "all", include = FALSE)

mc.regression.results <- evaluate.imputation(
  imputed = mc.regression.list,
  incomplete = dfNA,
  complete = df
)

mc.regression.results$accuracy.summary
mc.regression.results$structure.summary



# MICE PMM
# ==================================================
mc.pmm <- mice(dfNA, m = 50, method = mice.pmm.method, maxit = 10, seed = 2022)

mc.pmm.list <- complete(mc.pmm, action = "all", include = FALSE)

mc.pmm.results <- evaluate.imputation(
  imputed = mc.pmm.list,
  incomplete = dfNA,
  complete = df
)

mc.pmm.results$accuracy.summary
mc.pmm.results$structure.summary



# MICE RF
# ==================================================
mc.rf <- mice(dfNA, m = 50, method = mice.rf.method, maxit = 10, ntree = 500, seed = 2022)

mc.rf.list <- complete(mc.rf, action = "all", include = FALSE)

mc.rf.results <- evaluate.imputation(
  imputed = mc.rf.list,
  incomplete = dfNA,
  complete = df
)

mc.rf.results$accuracy.summary
mc.rf.results$structure.summary



# summarize cell-level imputation accuracy
# ==================================================
accuracy.results <- data.frame(
  Algorithm = c(
    "MLIM-random",
    "MLIM-mm",
    "MICE-regression",
    "MICE-PMM",
    "MICE-RF"
  ),

  NRMSE = c(
    mlim.random.results$accuracy.summary$Mean[
      mlim.random.results$accuracy.summary$Metric == "NRMSE"
    ],
    mlim.mm.results$accuracy.summary$Mean[
      mlim.mm.results$accuracy.summary$Metric == "NRMSE"
    ],
    mc.regression.results$accuracy.summary$Mean[
      mc.regression.results$accuracy.summary$Metric == "NRMSE"
    ],
    mc.pmm.results$accuracy.summary$Mean[
      mc.pmm.results$accuracy.summary$Metric == "NRMSE"
    ],
    mc.rf.results$accuracy.summary$Mean[
      mc.rf.results$accuracy.summary$Metric == "NRMSE"
    ]
  ),

  MPCE = c(
    mlim.random.results$accuracy.summary$Mean[
      mlim.random.results$accuracy.summary$Metric == "MPCE"
    ],
    mlim.mm.results$accuracy.summary$Mean[
      mlim.mm.results$accuracy.summary$Metric == "MPCE"
    ],
    mc.regression.results$accuracy.summary$Mean[
      mc.regression.results$accuracy.summary$Metric == "MPCE"
    ],
    mc.pmm.results$accuracy.summary$Mean[
      mc.pmm.results$accuracy.summary$Metric == "MPCE"
    ],
    mc.rf.results$accuracy.summary$Mean[
      mc.rf.results$accuracy.summary$Metric == "MPCE"
    ]
  ),

  RankError = c(
    mlim.random.results$accuracy.summary$Mean[
      mlim.random.results$accuracy.summary$Metric == "RankError"
    ],
    mlim.mm.results$accuracy.summary$Mean[
      mlim.mm.results$accuracy.summary$Metric == "RankError"
    ],
    mc.regression.results$accuracy.summary$Mean[
      mc.regression.results$accuracy.summary$Metric == "RankError"
    ],
    mc.pmm.results$accuracy.summary$Mean[
      mc.pmm.results$accuracy.summary$Metric == "RankError"
    ],
    mc.rf.results$accuracy.summary$Mean[
      mc.rf.results$accuracy.summary$Metric == "RankError"
    ]
  )
)

print(accuracy.results)



# summarize data-structure preservation
# ==================================================
structure.results <- data.frame(
  Algorithm = c(
    "MLIM-random",
    "MLIM-mm",
    "MICE-regression",
    "MICE-PMM",
    "MICE-RF"
  ),

  SD.Bias = c(
    mlim.random.results$structure.summary$Mean[1],
    mlim.mm.results$structure.summary$Mean[1],
    mc.regression.results$structure.summary$Mean[1],
    mc.pmm.results$structure.summary$Mean[1],
    mc.rf.results$structure.summary$Mean[1]
  ),

  Prevalence.Bias = c(
    mlim.random.results$structure.summary$Mean[2],
    mlim.mm.results$structure.summary$Mean[2],
    mc.regression.results$structure.summary$Mean[2],
    mc.pmm.results$structure.summary$Mean[2],
    mc.rf.results$structure.summary$Mean[2]
  ),

  Association.Error = c(
    mlim.random.results$structure.summary$Mean[3],
    mlim.mm.results$structure.summary$Mean[3],
    mc.regression.results$structure.summary$Mean[3],
    mc.pmm.results$structure.summary$Mean[3],
    mc.rf.results$structure.summary$Mean[3]
  )
)

print(structure.results)




# inspect detailed results if needed
# ==================================================

print(accuracy.results)
print(structure.results)

# cell-level error for each imputation
mlim.random.results$accuracy

# relative SD bias for each numeric variable
mlim.random.results$sd.bias

# prevalence bias for each categorical/ordinal level
mlim.random.results$prevalence.bias

# association recovery for each variable pair
mlim.random.results$association.error



save.image(file = "iris_15p_mlim_vs_mice_zero_threshold.RData")
