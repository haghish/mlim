library(feather)
library(arrow) # for parquet and feather

lst <- list(data1=cars, data2=iris)

datamix <- iris
attr(datamix, "metrics") <- 1:10
attr(datamix, "error_metric") <- 3
attr(datamix, "cars") <- cars

attributes(datamix)

write_feather(x=datamix, sink="feather")
write_parquet(x=datamix, sink="parquet")
