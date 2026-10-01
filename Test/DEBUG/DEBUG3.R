options(prefer_RCurl = TRUE)

library(mlim)
egsingle <- readRDS("~/Documents/GitHub/mlim/data/egsingle.RDS")
data("egsingle")

# add completely at random missingness at 15% rate
# ==================================================
dfNA <- egsingle
missingness <- c("year","grade","math","retained","female","black","hispanic","size","lowinc","mobility")
missingness <- c("year","grade")
dfNA[,missingness] <- mlim.na(egsingle[,missingness], p = .15, stratify = FALSE, seed = 2022)
#names(dfNA) <- c("SepalLength", "SepalWidth",  "PetalLength", "PetalWidth",  "Species")
#readstata13::save.dta13(dfNA, file = "iris2.dta")

mlim.random <- mlim(data=dfNA, m=2, hierarchy = c("schoolid", "childid"),
                    algos = c("ELNET"), max_models = 1,
                    report = "DEBUG3.md", debug = TRUE, verbosity = "debug",
                    tuning_time = 30, cpu = 6, seed=2022, 
                    preimpute = "random", tolerance = 0.01)

mlim.summarize(mlim.random)
mlim.error(mlim.random, dfNA, cars, varwise = TRUE)
