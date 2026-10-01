options(prefer_RCurl = TRUE)

library(mlimlight)
data("iris")

# add completely at random missingness at 15% rate
# ==================================================
dfNA <- iris
dfNA[,1:4] <- mlim.na(iris[,1:4], p = .15, stratify = FALSE, seed = 2022)
#names(dfNA) <- c("SepalLength", "SepalWidth",  "PetalLength", "PetalWidth",  "Species")
#readstata13::save.dta13(dfNA, file = "iris2.dta")

mlim.random <- mlim(data=dfNA, m=2, algos = c("RF"), max_models = 1,
                    report = "mlimlight.md", debug = TRUE, verbosity = "debug", superdebug = TRUE,
                    tuning_time = 30, cpu = 8, seed=2022, 
                    preimpute = "random", tolerance = 0.01)

mlim.summarize(mlim.random)
mlim.error(mlim.random, dfNA, iris, varwise = TRUE)
