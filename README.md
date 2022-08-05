> This R package is still in _Beta Version_. Please E-mail the bugs to <haghish@uio.no> or [report an issue](https://github.com/haghish/mlim/issues). 

# `mlim` : Missing Data Imputation with Automated Machine Learning

<a href="https://github.com/haghish/mlim"><img src="./web/mlim.png" align="left" width="140" hspace="10" vspace="6"></a>

In reccent years, there have been several attempts for using machine learning for missing data imputation. Yet, `mlim` R package is unique because it is the first R package to implement automation for missing data imputation. In other words, `mlim` implements automated machine learning and brings the state-of-the-arts of this technique, which is expected to result in imputation with lower imputation error compared to other standard procedures of missing data imputation. 

The figure below shows the normalized RMSE of the imputation of several algorithms, including `MICE`, `missForest`, `missRanger`, and `mlim`. Here, two of `mlim`'s algorithms, Elastic Net (ELNET) and Gradient Boosting Machine (GBM) are used for the imputation and the result are compared with Random Forest imputations as well as Multiple Imputation with Chained Equations (MICE), which uses Predictive Mean Matching (PMM). This imputation was carried out on __iris__ dataset in R, by adding 10% artifitial missing data and comparing the imputed values with the original. 

<img src="https://github.com/haghish/mlim/blob/main/web/data_iris.jpeg" width="600" height="400">

Supported algorithms
--------------------

`mlim` supports several algorithms. However, officially, only __`ELNET`__ is _recommended for personal computers with limited RAM_. `mlim` is __extremely__ computation hungry and is more suitable for servers with a lot of RAM. However, __`ELNET`__ converges rather fast and hence, provides a fast, scalable, yet highly flexible solution for missing data imputation. Compared to a fine-tuned __`GBM`__, __`ELNET`__ generally performs poorer, but their computational demands are vastly different. In order to fine-tune a __`GBM`__ model that out-performs __`ELNET`__, you need to include a large number of models to allow `mlim` to search for the ideal parameters for each variable, within each iteration. 

| **Algorithm** | **Speed**      | **RAM**        | **CPU**        |
|:--------------|:---------------|:---------------|:---------------|
| `ELNET`         | High           | Low            | Low            |
| `GBM`           | Low           | High           | High           |
<!--| XGBoost       | Low           | High           | High           |
| Ensemble      | Extremely Low | Extremely High | Extremely High |-->

### `GBM` vs `ELNET`

But which one should you choose, assuming computation resources are not in question? Well, __`GBM`__ is very liokely to outperform __`ELNET`__, if you specify a large enough `max_models` argument to well-tune the algorithm for imputing each feature. That basically means generating more than 100 models, at least. But you will enjoy a slight -- yet probably statistically significant -- improvement in the imputation accuracy. The option is there, for those who can use it, and to my knowledge, fine-tuning __`GBM`__ with large enough number of models will be the most accurate imputation algorithm compared to any other procedure I know. But __`ELNET`__ comes second and compared to its speed advantage, it is indeed charming!

Advantages and limitations
--------------------------

`mlim` fine-tunes models for imputation, a procedure that has never been implemented in other R packages. This procedure often yields much higher accuracy compared to other machine learning imputation methods or missing data imputation procedures because of using more accurate models that are fine-tuned for each feature in the dataset. The cost, however, is computational resources. If you have access to a very powerful machine, with a huge amount of RAM per CPU, then try __`GBM`__. If you specify a high enough number of models in each fine-tuning process, you are likely to get a more accurate imputation that __`ELNET`__. However, for personal machines and laptops, __`ELNET`__ is generally recommended (see below). __If your machine is not powerful enough, it is likely that the imputation crashes due to memory problems...__. So, perhaps begin with __`ELNET`__, unless you are working with a powerful server. This is my general advice as long as `mlim` is in Beta version and under development.

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

`iris` ia a small dataset with 150 rows only. Let's add 50% of artifitial missing data and compare several state-of-the-art machine learning missing data imputation procedures. __`ELNET`__ comes up as a winner for a very simple reason! Because it was fine-tuned and all the rest were not. The larger the dataset and the higher the number of features, the difference between __`ELNET`__ and the others becomes more vivid. 

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

But that is not all! `mlim` also outperforms other R packages for imputing categorical and ordinal variables. Here is an example from the `trait` dataset, which is included in the package. 

<img src="https://github.com/haghish/mlim/blob/main/web/trait_categorical.png" width="600" height="400">

