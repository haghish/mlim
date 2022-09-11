
data(iris)
library(mlim)
library(h2o)
h2o.shutdown(F)

# add stratified missing observations to the data. to make the example run
# faster, I add NAs only to a single variable.
dfNA <- iris
dfNA$Species <- mlim.na(dfNA$Species, p = 0.3, stratify = TRUE, seed = 2022)
#dfNA$Sepal.Length <- mlim.na(dfNA$Sepal.Length, p = 0.2, seed = 2022)
#dfNA$Sepal.Width <- mlim.na(dfNA$Sepal.Width, p = 0.6, seed = 2022)
# dfNA <- mlim.na(dfNA, p = 0.1, seed = 2023)

# run the ELNET single imputation (fastest imputation via 'mlim')
MLIM <- mlim(dfNA, shutdown = FALSE, m = 1, save = "test.mlim", flush = FALSE,
             tolerance = NULL)





mlim.error(MLIM, dfNA, iris)

# stop after 2 iterations
h2o.shutdown(F)


library(mlim)
MLIM <- mlim(load = "test.mlim")

# stop at the end of data 1 iteration 3 and repeat it at 4
dfNA <- iris
dfNA <- mlim.na(dfNA, p = 0.1, seed = 2023)
MLIM <- mlim(dfNA, shutdown = FALSE, m = 3, save = "test.mlim", flush = FALSE)
MLIM <- mlim(load = "test.mlim")


object <- readRDS("test.mlim")

MLIM2 <- mlim(dfNA, m = 5, report = "yes.md", verbosity = "warn",
              flush = TRUE, shutdown = FALSE)
mlim.error(MLIM2, dfNA, iris)

MLIM50 <- mlim(dfNA, m = 50, report = "yes.md", verbosity = "warn",
              flush = TRUE, shutdown = FALSE)


library(h2o)
a <-h2o.load_frame('RTMP_sid_84ff_60', dir = paste0(getwd(),"/.flush"))
print(h2o.dim(a))

h2o.shutdown(F)
h2o.init()
fit <- h2o.automl(include_algos ="GLM", x="Species", y="Sepal.Length",
                  training_frame =as.h2o(dodo),
        weights_column = "???????", nfolds = 5)

h2o.performance(fit@leader, xval = TRUE)


library(tictoc)
IRIS <- iris[sample(1:nrow(iris), 100000, replace = TRUE), ]
IRIS <- sample(IRIS, 1000, replace = T)
colnames(IRIS) <- paste0("v", 1:ncol(IRIS))
tic(); a.hex <- as.h2o(IRIS); id <- h2o.getId(a.hex); a <- as.data.frame(a.hex); as.h2o(a);toc()
tic(); a.hex <- as.h2o(IRIS); id <- h2o.getId(a.hex); h2o.save_frame(a.hex, dir = getwd()); h2o.load_frame(id, dir = getwd());toc()
