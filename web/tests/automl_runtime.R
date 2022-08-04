data("iris")
library(h2o)
library(tictoc)

h2o.init(min_mem_size="2G", max_mem_size="4g")
tic()
model <- h2o.automl(x=colnames(iris[,2:5]), , y="Sepal.Length",
                    training_frame = as.h2o(iris),
                    include_algos = "GBM", max_models = 100, nfolds = 10,
                    seed=2022)
toc()

gc()
gc(reset=T)
memory.profile()
