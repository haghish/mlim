> This R package is still in _Beta Version_. Please E-mail the bugs to <haghish@uio.no> or [report an issue](https://github.com/haghish/mlim/issues). 

# `mlim` : Extreme Missing Data Imputation with Automated Machine Learning

<a href="http://haghish.com/mlim"><img src="./web/mlim.png" align="left" width="140" hspace="10" vspace="6"></a>

In reccent years, there have been several attempts for using machine learning for missing data imputation. Yet, `mlim` R package is unique because it is the first R package to implement automation for missing data imputation. In other words, `mlim` implements automated machine learning and brings the state-of-the-arts of this technique, which is expected to result in imputation with lower imputation error compared to other standard procedures of missing data imputation. 

The figure below shows the normalized RMSE of the imputation of several algorithms, including `MICE`, `missForest`, `missRanger`, and `mlim`. Here, two of `mlim`'s algorithms, Elastic Net (ELNET) and Gradient Boosting Machine (GBM) are used for the imputation and the result are compared with Random Forest imputations as well as Multiple Imputation with Chained Equations (MICE), which uses Predictive Mean Matching (PMM). This imputation was carried out on __iris__ dataset in R, by adding 10% artifitial missing data and comparing the imputed values with the original. 

<img src="https://github.com/haghish/mlim/blob/main/web/data_iris.jpeg" width="600" height="400">

Supported algorithms
--------------------

`mlim` supports several algorithms. However, officially, only __ELNET__ is _recommended for personal computers with limited RAM_. `mlim` is __extremely__ computation hungry and is more suitable for servers with a lot of RAM. However, __ELNET__ converges rather fast and hence, provides a fast, scalable, yet highly flexible solution for missing data imputation. 

| **Algorithm** | **Speed**      | **RAM**        | **CPU**        |
|:--------------|:---------------|:---------------|:---------------|
| ELNET         | High           | Low            | Low            |
| GBM           | Slow           | High           | High           |
| XGBoost       | Slow           | High           | High           |
| Ensemble      | Extremely Slow | Extremely High | Extremely High |

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

__TO BE CONTINUED...__
