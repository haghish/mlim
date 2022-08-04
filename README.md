> This R package is still in _Beta Version_. Please E-mail the bugs to <haghish@uio.no> or [report an issue](https://github.com/haghish/mlim/issues). 

# `mlim` : Missing Data Imputation with Automated Machine Learning

<a href="http://haghish.com/mlim"><img src="./web/mlim.png" align="left" width="140" hspace="10" vspace="6"></a>

In reccent years, there have been several attempts for using machine learning for missing data imputation. Yet, `mlim` R package is unique because it is the first R package to implement automation for missing data imputation. In other words, `mlim` implements automated machine learning and brings the state-of-the-arts of this technique, which is expected to result in imputation with lower imputation error compared to other standard procedures of missing data imputation. 

The figure below shows the normalized RMSE of the imputation of several algorithms, including `MICE`, `missForest`, `missRanger`, and `mlim`. Here, two of `mlim`'s algorithms, Elastic Net (ELNET) and Gradient Boosting Machine (GBM) are used for the imputation and the result are compared with Random Forest imputations as well as Multiple Imputation with Chained Equations (MICE), which uses Predictive Mean Matching (PMM). This imputation was carried out on __iris__ dataset in R, by adding 10% artifitial missing data and comparing the imputed values with the original. 

<img src="https://github.com/haghish/mlim/blob/main/web/data_iris.jpeg" width="600" height="400">

Supported algorithms
--------------------

`mlim` supports several algorithms. However, officially, only __ELNET__ is _recommended for personal computers with limited RAM_. `mlim` is __extremely__ computation hungry and is more suitable for servers with a lot of RAM. However, __ELNET__ converges rather fast and hence, provides a fast, scalable, yet highly flexible solution for missing data imputation. Compared to a fine-tuned __GBM__, __ELNET__ generally performs poorer, but their computational demands are vastly different. In order to fine-tune a __GBM__ model that out-performs __ELNER__, you need to include a large number of models to allow `mlim` to search for the ideal parameters for each variable, within each iteration. 

| **Algorithm** | **Speed**      | **RAM**        | **CPU**        |
|:--------------|:---------------|:---------------|:---------------|
| ELNET         | High           | Low            | Low            |
| GBM           | Low           | High           | High           |
<!--| XGBoost       | Low           | High           | High           |
| Ensemble      | Extremely Low | Extremely High | Extremely High |-->

Advantages and limitations
--------------------------

`mlim` fine-tunes models for imputation, a procedure that has never been implemented in other R packages. This procedure often yields much higher accuracy compared to other machine learning imputation methods or missing data imputation procedures because of using more accurate models that are fine-tuned for each feature in the dataset. The cost, however, is computational resources. If you have access to a very powerful machine, with a huge amount of RAM per CPU, then try __GBM__ or __XGBoost__. If you specify a high enough number of models in each fine-tuning process, you are likely to get a more accurate imputation that __ELNET__. Howevver, for personal machines and laptops, __ELNET__ is generally recommended (see below). 

Preimputation
-------------

`mlim` implements a trick to reduce number of iterations needed for reaching the optimized imputation. Usually, prior to the imputation, the missing data are replaced with mean, mode, or even random values from within the variable. This is a fair start-point for the imputation procedure, but makes the optimization very time consuming. Another possibility would be to use a fast and well-established imputation algorithm for the pre-imputation and then improve the imputed values. `mlim` supports the following algorithms for preimputation:

| **Algorithm**  |    **Speed**   |  **RAM** |  **CPU**  |
|:---------------|:---------------|:---------|:----------|
| `kNN`       |    Very fast   |    Low   |    Low    |
| `ranger`    |      fast      |   High   |    High   |
| `missForest` |    Very Slow   |   High   | Very High |
| `mm`        | Extremely fast | Very Low |  Very Low |


Example 
-------

`iris` ia a small dataset with 150 rows only. Let's add 50% of artifitial missing data and compare several state-of-the-art machine learning missing data imputation procedures. __ELNET__ comes up as a winner for a very simple reason! Because it was fine-tuned and all the rest were not. The larger the dataset and the higher the number of features, the difference between __ELNET__ and the others becomes more vivid. 

```R
# Comparison of different R packages imputing iris dataset
# ===========================================================
rm(list = ls())
library(mlim)
library(mice)
library(missForest)
library(missRanger)
library(VIM)

# Add artifitial missing data
# ===========================================================
irisNA <- missRanger::generateNA(iris, p = 0.5, seed = 2022)

# ELNET Imputation with mlim
# ===========================================================
mlimELNET <- mlim(irisNA, init = TRUE, maxiter = 10,
                  include_algos = "ELNET", preimpute = "knn",
                  report = "mlimELNET.log", verbosity = "debug",
                  max_models = 1, min_mem_size = "6G", nthreads = 1,
                  max_mem_size = "8G", iteration_stopping_tolerance = .01,
                  shutdown = TRUE, flush=FALSE, seed = 2022)
(mlimELNETerror <- mixError(mlimELNET, irisNA, iris))

# kNN Imputation with VIM
# ===========================================================
kNN <- kNN(irisNA, imp_var=FALSE)
(kNNerror <- mixError(kNN, irisNA, iris))

# MICE Imputation with mice (10 datasets)
# ===========================================================
m <- 10
mc <- mice(irisNA, m=m, maxit = 50, method = 'pmm', seed = 500)
MCerror <- NULL
for (i in 1:m) MCerror <- c(MCerror, mixError(complete(mc,i), irisNA, iris)[1])
(MCerror <- mean(MCerror))

# Random Forest Imputation with missForest
# ===========================================================
set.seed(2022)
RF <- missForest(irisNA)
(RFerror <- mixError(RF$ximp, irisNA, iris))

rngr <- missRanger(irisNA, num.trees=100, seed = 2022)
(missRanger <- mixError(rngr, irisNA, iris))
```
<img src="https://github.com/haghish/mlim/blob/main/web/data_iris_50.png" width="600" height="400">

