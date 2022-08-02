> This R package is still in _Beta Version_. Please E-mail the bugs to <haghish@uio.no> or [report an issue](https://github.com/haghish/mlim/issues). 

`mlim` : Extreme Missing Data Imputation with Automated Machine Learning
========================================================================

In reccent years, there have been several attempts for using machine learning for missing data imputation. Yet, `mlim` R package is unique because it is the first R package to implement automation for missing data imputation. In other words, `mlim` implements automated machine learning and brings the state-of-the-arts of this technique, which is expected to result in imputation with lower imputation error compared to other standard procedures of missing data imputation. 

The figure below shows the normalized RMSE of the imputation of several algorithms, including `MICE`, `missForest`, `missRanger`, and `mlim`. Here, two of `mlim`'s algorithms, Elastic Net (ELNET) and Gradient Boosting Machine (GBM) are used for the imputation and the result are compared with Random Forest imputations as well as Multiple Imputation with Chained Equations (MICE), which uses Predictive Mean Matching (PMM). This imputation was carried out on __iris__ dataset in R, by adding 10% artifitial missing data and comparing the imputed values with the original. 

![comparison](https://github.com/haghish/mlim/blob/main/web/data_iris.jpeg)
