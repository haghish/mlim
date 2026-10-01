source("~/Documents/GitHub/mlimlight/Test/check/mlim vs mice/functions.R")
library(mlimlight)
library(mice)
data("mtcars")
df <- mtcars

# add completely at random missingness at 15% rate
# ==================================================
dfNA <- mlim.na(df, p = .15, stratify = FALSE, seed = 2022)

# MLIM with random replacement preimputation
# ==================================================
mlim.random <- mlim(data=dfNA, m=5,
                    algos = "CAT", #algos = c("ELNET", "LGBM", "CAT"),
                    max_models = 10, maxiter = 10, tuning_time = 900,
                    report = "test.md", #debug = TRUE, #verbosity = "debug",
                    cpu = 8, seed=2022,
                    preimpute = "random", tolerance = 0.0001)

mlim.summarize(mlim.random)
mlim.error(mlim.random, dfNA, mtcars, varwise = TRUE)

# MICE PMM
# ==================================================
mc <- mice(dfNA, m = 5, method = "pmm")
mcList <- complete(mc, action = "all", include = FALSE)
(mcError <- mlim.error(mcList, dfNA, df, varwise = F))
