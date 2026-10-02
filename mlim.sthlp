{smcl}
{it:v. 0.6.1}


{title:mlim}

{p 4 4 2}
{bf:mlim} - Single and Multiple Imputation with Automated Machine Learning


{title:Syntax}

{p 8 8 2} {bf:mlim} [, {it:m(#)} {it:algos(string)} {it:stochastic} {it:nostochastic}
{it:ignore(varlist)} {it:hierarchy(varlist)} {it:tuningtime(#)} {it:maxmodels(#)} {it:maxiter(#)}
{it:cv(#)} {it:noautobalance} {it:seed(#)}
{it:verbosity(string)} {it:report(string)} {it:tolerance(#)} {it:preimpute(string)}
{it:cpu(#)} {it:save(string)} {it:load(string)} {it:filename(string)} {it:debug} ]


{title:Description}

{p 4 4 2}
{bf:mlim} imputes missing values in the dataset currently in memory by calling the
R package {bf:mlim} through {bf:rcall}. By default, it performs a single imputation.
Specify {bf:m(#)} with a value larger than 1 to carry out multiple imputations.

{p 4 4 2}
For a single imputation ({it:_m(1)_}), the completed dataset returned by R replaces
the dataset in memory. For multiple imputation ({it:_m()>1_}), {bf:mlim} converts the
R result to Stata{c 39}s {bf:flong} format and then runs {bf:mi import flong}. The variables
that were imputed are registered with Stata as imputed variables.

{p 4 4 2}
The wrapper follows the current R {bf:mlim::mlim()} interface. Options that control
model fitting, stochastic imputation, matching, hierarchy, convergence, and
reproducibility are passed directly to R.


{title:Requirements}

{p 4 4 2}
{bf:mlim} requires the Stata package {bf:rcall} and the following R packages:

{col 5}Requirement{col 22}Minimum version
{space 4}{hline 34}
{col 5}{bf:mlim}{col 22}0.6.0
{col 5}{bf:readstata13}{col 22}0.11.0
{space 4}{hline 34}
{p 4 4 2}
R version 4.1.0 or newer is required. The current R implementation uses {bf:mlr3}
and {bf:mlr3tuning} rather than {bf:h2o}, so Java and an H2O server are not required.
Additional learner packages are required only when their corresponding optional
algorithms are selected.


{title:Options}

{col 5}{it:Option}{col 28}{it:Description}
{space 4}{hline}
{col 5}{bf:m(#)}{col 28}Number of imputations. The default is 1 (dry run).
{col 5}{bf:algos(string)}{col 28}Supported algorithms include {bf:ELNET}, {bf:RF}, {bf:CRF}, {bf:GBM}, {bf:XGB}, {bf:LGBM}, {bf:CAT}, {bf:NNET}, and {bf:ENSEMBLE}.
{col 5}{bf:stochastic}{col 28}Sets {bf:stochastic = TRUE}. For multiple imputation, stochastic imputation is TRUE by default.
{col 5}{bf:nostochastic}{col 28}Sets {bf:stochastic = FALSE}. May not be combined with {bf:stochastic}.
{col 5}{bf:ignore(varlist)}{col 28}Excludes variables from the imputation process.
{col 5}{bf:hierarchy(varlist)}{col 28}Specifies clustering variables from the highest to the lowest level.
{col 5}{bf:tuningtime(#)}{col 28}Sets  the maximum tuning time per variable and iteration.
{col 5}{bf:maxmodels(#)}{col 28}Sets the maximum number of hyperparameter evaluations per variable and iteration.
{col 5}{bf:maxiter(#)}{col 28}Sets the maximum number of imputation iterations.
{col 5}{bf:cv(#)}{col 28}Sets the number of cross-validation folds.
{col 5}{bf:noautobalance}{col 28}Sets {bf:autobalance = FALSE}.
{col 5}{bf:seed(#)}{col 28}Sets random-number seed.
{col 5}{bf:verbosity(string)}{col 28}Accepts {bf:warn}, {bf:info}, {bf:debug}, or NULL.
{col 5}{bf:report(string)}{col 28}Specify a report filename.
{col 5}{bf:tolerance(#)}{col 28}Sets the convergence {bf:tolerance}.
{col 5}{bf:preimpute(string)}{col 28}Sets the initial preimputation method, such as {bf:random} or {bf:mm}.
{col 5}{bf:cpu(#)}{col 28}Sets the number of CPU threads supplied to learners.
{col 5}{bf:save(string)}{col 28}Saves the current imputation state to an {bf:.mlim} RDS file after variable-level updates.
{col 5}{bf:load(string)}{col 28}Resumes an imputation from a previously saved {bf:.mlim} state.
{col 5}{bf:filename(string)}{col 28}Saves the completed data to the specified Stata {bf:.dta} file.
{col 5}{bf:debug}{col 28}Passes the hidden R argument {bf:debug = TRUE}.
{space 4}{hline}

{title:Remarks}

{p 4 4 2}{bf:Algorithms}

{p 4 4 2}
The current R implementation supports the following algorithm names:

{break}    * {bf:ELNET}: elastic net
{break}    * {bf:RF}: random forest
{break}    * {bf:CRF}: conditional random forest
{break}    * {bf:GBM}: gradient boosting
{break}    * {bf:XGB}: XGBoost
{break}    * {bf:LGBM}: LightGBM
{break}    * {bf:CAT}: CatBoost
{break}    * {bf:NNET}: single-hidden-layer neural network
{break}    * {bf:SVM}: kernel support vector machine
{break}    * {bf:KNN}: k-nearest neighbors
{break}    * {bf:ENSEMBLE}: stacked ensemble using the successfully tuned base learners

{p 4 4 2}
Some algorithms are provided through {bf:mlr3extralearners} and therefore require
that package and the corresponding learner package to be installed. For example,
{bf:LGBM}, {bf:CAT}, and {bf:SVM} use optional learner extensions. {bf:KNN} is not
available for multiple imputation when bootstrap observation weights are required,
because its current learner does not support observation weights. {bf:GBM} is also
skipped for multinomial targets when its current learner does not support multiclass
classification.

{p 4 4 2}{bf:Stochastic imputation}

{p 4 4 2}
When {bf:stochastic} is TRUE, continuous numeric predictions receive stochastic
variation based on the model{c 39}s cross-validation RMSE, whereas categorical predictions
are sampled from their predicted class-probability vectors. 

{p 4 4 2}{bf:Hierarchical imputation}

{p 4 4 2}
The {bf:hierarchy()} option specifies nested clustering variables from the highest
to the lowest level. For example, {bf:hierarchy(city school classroom student)}
represents students nested within classrooms, classrooms nested within schools, and
schools nested within cities. Hierarchy variables must exist in the data and cannot
contain missing values.

{p 4 4 2}{bf:Variables selected for imputation}

{p 4 4 2}
For a new imputation, the R package selects variables that contain missing values
but are not completely missing, excluding variables specified in {bf:ignore()}. The
Stata wrapper uses the same criterion when preparing the variable list needed by
{bf:mi import flong}. String variables are reported to the user; they should be
encoded as numeric categorical variables or placed in {bf:ignore()}.

{p 4 4 2}{bf:Single versus multiple imputation}

{p 4 4 2}
With {bf:m(1)}, {bf:mlim::mlim()} returns one completed data frame and {bf:rcall} loads
it into Stata.

{p 4 4 2}
With {bf:m()>1}, the R result is converted by {bf:mlim::mlim.stata()} to {bf:flong}
format. Stata then runs:

{p 8 8 2} {bf:mi import flong, m(m) id(id) imputed(varlist)}

{p 4 4 2}
where {it:varlist} contains the variables imputed by {bf:mlim}. The resulting data are
registered as a Stata multiple-imputation dataset and {bf:mi describe} is displayed.

{p 4 4 2}{bf:Loading a saved imputation}

{p 4 4 2}
{bf:load()} is different from starting a new imputation. The R package reads the
saved {bf:mlim} state and restores its data, iteration position, model settings,
number of imputations, and other saved options. Therefore, options such as
{bf:algos()}, {bf:m()}, {bf:tuningtime()}, and {bf:maxmodels()} do not override the
saved state when {bf:load()} is used.

{p 4 4 2}
For a loaded multiple-imputation state, the wrapper determines the number of
imputations from the saved object before deciding whether to run {bf:mi import flong}.
This is important because {bf:m()} defaults to 1 in the Stata syntax but a saved
state may contain multiple imputations.

{p 4 4 2}
{bf:save()} and {bf:load()} cannot be specified together. A loaded state restores its
saved {bf:save} setting, so a new {bf:save()} path cannot be supplied by the wrapper
when resuming an existing state.

{p 4 4 2}{bf:Protecting the dataset}

{p 4 4 2}
The command uses {bf:preserve} before the R call. If R execution or the subsequent
{bf:mi import flong} fails, the original dataset is restored. On success, the command
uses {bf:restore, not}, retaining the completed data returned by R.

{p 4 4 2}{bf:Reserved names}

{p 4 4 2}
For multiple imputation, variables named {bf:m} and {bf:id} are reserved for the
Stata {bf:flong} representation. Rename existing variables with these names before
running a multiple imputation.

{p 4 4 2}{bf:R execution}

{p 4 4 2}
The program checks for {bf:rcall.ado} and verifies the required R and package versions
with {bf:rcall_check}. R is called in {bf:vanilla} mode. Errors returned by R are
propagated to Stata after the original dataset is restored.


{title:Examples}

{p 4 4 2}
Let{c 39}s first prepare a dataset with missing values

    . sysuse auto, clear
    . replace mpg = . if mod(_n, 7) == 0
    . replace weight = . if mod(_n, 9) == 0
    . encode make, gen(make_cat)
    . drop make

{p 4 4 2}
Single imputation using the default algorithm:

{p 8 8 2} . {bf:mlim}

{p 4 4 2}
Multiple imputation with five datasets:

{p 8 8 2} . {bf:mlim, m(5)}

{p 4 4 2}
Use several algorithms and allow up to 10 minutes of tuning per variable and iteration:

{p 8 8 2} . {bf:mlim, m(5) algos(ELNET RF XGB) tuningtime(600) maxmodels(50)}

{p 4 4 2}
Use a hierarchical structure:

{p 8 8 2} . {bf:mlim, m(5) hierarchy(schoolid childid)}

{p 4 4 2}
Disable stochastic imputation:

{p 8 8 2} . {bf:mlim, m(1) nostochastic {bf:

{p 4 4 2}
Ignore a variable and use a reproducible seed:

{p 8 8 2} . {bf:mlim, m(5) ignore(length) seed(2026)}

{p 4 4 2}
Use four CPU threads:

{p 8 8 2} . {bf:mlim, m(5) cpu(4)}

{p 4 4 2}
Save an imputation state:

{p 8 8 2} . {bf:mlim, m(5) save("my_imputation.mlim")}

{p 4 4 2}
Continue a previously saved imputation:

{p 8 8 2} . {bf:mlim, load("my_imputation.mlim")}

{p 4 4 2}
Save the completed data to a Stata file:

{p 8 8 2} . {bf:mlim, m(5) filename("imputed_data.dta")}

{p 4 4 2}
Inspect the R arguments generated by the Stata wrapper:

{p 8 8 2} . {bf:mlim, debug}


{title:Stored results}

{p 4 4 2}
The wrapper does not define a separate documented {bf:r()}, {bf:e()}, or {bf:s()}
result. The primary result is the completed dataset left in memory. The internal
R result indicating the number of imputations is used by the wrapper to decide
whether Stata{c 39}s {bf:mi import flong} step is required.


{title:Acknowledgments}

{p 4 4 2}
{bf:mlim} is a Stata interface to the R package  {browse "http://github.com/haghish/mlim":{bf:mlim}}
and uses  {browse "http://github.com/haghish/rcall":{bf:rcall}} for communication between Stata
and R. Dataset exchange relies on the R package {bf:readstata13}.


{title:Author}

{p 4 4 2}
E. F. Haghish    {break}
Faculty of Psychological Sciences    {break}
University of Bergen    {break}
haghish@uib.no    {break}

{p 4 4 2}
{browse "github.com/haghish/mlim":rcall Homepage}    {break}
Package Updates on  {browse "http://www.x.com/Haghish":X}    {break}


{title:License}

{p 4 4 2}
{it:MIT License}

{space 4}{hline}

{p 4 4 2}
This documentation is written in Markdown inside a MarkDoc documentation block.
After saving the program as {bf:mlim.ado}, generate the Stata help file with:

{p 8 8 2} . {bf:markdoc "mlim.ado", mini export(sthlp) replace}



