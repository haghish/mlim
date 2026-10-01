options(prefer_RCurl = TRUE)

library(mlimlight)
library(mice)
data("mtcars")

# add completely at random missingness at 15% rate
# ==================================================
df <- mtcars
dfNA <- df
dfNA[, 1:4] <- mlim.na(mtcars[, 1:4], p = .15, stratify = FALSE, seed = 2022)
#names(dfNA) <- c("SepalLength", "SepalWidth",  "PetalLength", "PetalWidth",  "Species")
#readstata13::save.dta13(dfNA, file = "iris2.dta")

mlim.random <- mlim(data=dfNA, m=1, algos = c("ELNET", "LGBM", "CAT"), max_models = 100, maxiter = 2,
                    report = "test.md", debug = TRUE, verbosity = "debug",
                    tuning_time = 30, cpu = 6, seed=2022,
                    preimpute = "random", tolerance = 0.00001)

mlim.summarize(mlim.random)
mlim.error(mlim.random, dfNA, mtcars, varwise = TRUE)

# MICE PMM
# ==================================================
mc <- mice(dfNA, m = 5, method = "pmm")
mcList <- complete(mc, action = "all", include = FALSE)
(mcError <- mlim.error(mcList, dfNA, df, varwise = F))
