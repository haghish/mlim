{smcl}
{it:v. 01}


{title:mlim}

{p 4 4 2}
{bf:mlim} - Single and Multiple Imputation with Automated Machine Learning


{title:Syntax}

{p 8 8 2} {bf:mlim} [, {it:m(#)} {it:algos(string)} {it:stochastic} {it:nostochastic}
{it:ignore(varlist)} {it:tuningtime(#)} {it:maxmodels(#)} {it:maxiter(#)}
{it:cv(#)} {it:matching} {it:noautobalance} {it:balance(varlist)} {it:seed(#)}
{it:verbosity(string)} {it:report(string)} {it:tolerance(#)} 
{it:preimpute(string)} {it:cpu(#)} {it:ram(#)} {it:flush} {it:save(string)}
{it:load(string)} {it:java(string)} {it:filename(string)}
{it:debug} ]


{title:Description}

{p 4 4 2}
{bf:mlim} imputes missing values in the dataset currently in memory by calling the
R package {bf:mlim} through {bf:rcall}. By default, it performs a single imputation.
Specify {bf:m(#)} with a value larger than 1 to carry out multiple imputations.

{p 4 4 2}
The command first identifies variables containing missing observations. Variables
listed in {bf:ignore()} are excluded from imputation. If string variables are present,
{bf:mlim} returns an error and requests encoding them as numeric categorical variables
or placing them in {bf:ignore()}.

{p 4 4 2}
For a single imputation ({it:_m(1)_}), the imputed dataset returned by R replaces the
dataset in memory. For multiple imputation ({it:_m()>1_}), the R result is converted to
{bf:flong} form with and then imported with Stata{c 39}s {bf:mi import flong}. 
In this case, {bf:mlim} reserves the variable names {bf:m} and
{bf:id} for appending the imputed datasets. 


{title:Requirements}

{p 4 4 2}
{bf:mlim} requires the Stata package {bf:rcall} and the following R packages:

{col 5}Requirement{col 22}Minimum version
{space 4}{hline 34}
{col 5}{bf:mlim}{col 22}0.4.0
{col 5}{bf:readstata13}{col 22}0.11.0
{col 5}{bf:h2o}{col 22}-
{space 4}{hline 34}
{p 4 4 2}
R and Java Runtime should also be accessible via path environment.


{title:Options}

{col 5}{it:Option}{col 26}{it:Description}
{space 4}{hline}
{col 5}{bf:m(#)}{col 26}Number of imp utations. The default is 1 and values must be at least 1.
{col 5}{bf:algos(string)}{col 26}Passes a space-separated set of algorithm names to the R argument {bf:algos}.
{col 5}{bf:stochastic}{col 26}Passes {bf:stochastic = TRUE} to R. Experimental in this source.
{col 5}{bf:nostochastic}{col 26}Passes {bf:stochastic = FALSE} to R. May not be combined with {bf:stochastic}. Experimental feature.
{col 5}{bf:ignore(varlist)}{col 26}Excludes variables from the set of variables to be imputed
{col 5}{bf:tuningtime(#)}{col 26}Passes {bf:tuning_time = #} to R.
{col 5}{bf:maxmodels(#)}{col 26}Passes {bf:max_models = #} to R.
{col 5}{bf:maxiter(#)}{col 26}Passes {bf:maxiter = #} to R.
{col 5}{bf:cv(#)}{col 26}Passes {bf:cv = #} to R.
{col 5}{bf:matching}{col 26}Experimental option related to predictive matching. See Remarks below.
{col 5}{bf:noautobalance}{col 26}Turns off class imbalance correction in single imputation
{col 5}{bf:balance(varlist)}{col 26}Passes the listed variables to R as {bf:balance}. Experimental in this source.
{col 5}{bf:seed(#)}{col 26}Passes the integer random-number seed to R.
{col 5}{bf:verbosity(string)}{col 26}Passes {bf:verbosity} to R.
{col 5}{bf:report(string)}{col 26}Passes a report path or report specification to R.
{col 5}{bf:tolerance(#)}{col 26}Passes the convergence {bf:tolerance} to R.
{col 5}{bf:preimpute(string)}{col 26}Passes {bf:preimpute} to R.
{col 5}{bf:cpu(#)}{col 26}Passes the requested number of CPUs to R.
{col 5}{bf:ram(#)}{col 26}Passes the requested RAM value to R.
{col 5}{bf:flush}{col 26}Passes {bf:flush = TRUE} to R, requesting cleanup of H2O models.
{col 5}{bf:save(string)}{col 26}Passes {bf:save} to the R package to save its imputation state (recommended).
{col 5}{bf:load(string)}{col 26}Passes {bf:load} to the R package to load a previously saved imputation state.
{col 5}{bf:java(string)}{col 26}Passes a Java path to R. Backslashes are converted to forward slashes.
{col 5}{bf:filename(string)}{col 26}Saves the imputed data to a Stata {bf:.dta} file (recommended).
{col 5}{bf:debug}{col 26}Used for debugging the program.
{space 4}{hline}

{title:Remarks}

{p 4 4 2}{bf:Variables selected for imputation}

{p 4 4 2}
The command scans all variables in the dataset and selects every variable containing
at least one missing observation. Variables specified in {bf:ignore()} are then removed
from that list. If no variables remain, the command exits with an error.

{p 4 4 2}
String variables are not automatically encoded. The command warns when string
variables remain outside {bf:ignore()}, but the current source does not stop execution
after that warning.

{p 4 4 2}{bf:Single versus multiple imputation}

{p 4 4 2}
With {bf:m(1)}, {bf:mlim::mlim()} returns one completed dataset and {bf:rcall} loads
that dataset into Stata.

{p 4 4 2}
With {bf:m()>1}, the command asks {bf:mlim::mlim.stata()} to create data in
{bf:flong} format. Stata then runs:

{p 8 8 2} {bf:mi import flong, m(m) id(id) imputed(varlist)}

{p 4 4 2}
where {it:varlist} is the set of variables that had missing values before imputation,
after exclusions in {bf:ignore()}. The resulting data remain in memory and
{bf:mi describe} is displayed.

{p 4 4 2}{bf:Protecting the dataset}

{p 4 4 2}
Before calling R, the command uses {bf:preserve}. If R execution or the subsequent
{bf:mi import flong} fails, the original data are restored. On success, the command
uses {bf:restore, not}, retaining the imputed dataset loaded by R.

{p 4 4 2}{bf:Experimental matching option}

{p 4 4 2}
The current syntax declares {bf:matching} as a switch. The implementation then treats
its local macro as though it could contain values such as TRUE, FALSE, or AUTO.
Consequently, in this development snapshot, specifying {bf:matching} does not enable
matching; the generated R argument falls through to {bf:matching = FALSE}. This option
should therefore be regarded as under development until its syntax and implementation
are reconciled.

{p 4 4 2}{bf:Reserved names}

{p 4 4 2}
When {bf:m()>1}, variables named {bf:m} and {bf:id} are reserved for conversion to
Stata{c 39}s flong MI representation. Rename existing variables with those names before
calling {bf:mlim}.

{p 4 4 2}{bf:R execution}

{p 4 4 2}
The program checks for {bf:rcall.ado} and calls {bf:rcall_check} before imputation.
All imputation calls use {bf:rcall vanilla}, which starts a fresh R session for the call.
Errors returned by R are propagated to Stata after the original dataset is restored.


{title:Examples}

{p 4 4 2}
Let{c 39}s first prepare a dataset with missing values

    . sysuse auto, clear
    . replace mpg = . if mod(_n, 7) == 0
    . replace weight = . if mod(_n, 9) == 0
    . encode make, gen(make_cat)              // note that the make variable is encoded
    . drop make

{p 4 4 2}
Single imputation using default arguments:

{p 8 8 2} . {bf:mlim}

{p 4 4 2}
Impute five datasets:

{p 8 8 2} . {bf:mlim, m(5)}

{p 4 4 2}
Ignore a variable and use a reproducible seed:

{p 8 8 2} . {bf:mlim, m(5) ignore(length) seed(2026)}

{p 4 4 2}
Limit computational resources to 4 CPU and 8GB of RAM:

{p 8 8 2} . {bf:mlim, m(5) cpu(4) ram(8)}

{p 4 4 2}
Spend up to 10 minutes on hyperparameter tuning for each variable in each itteration:

{p 8 8 2} . {bf:mlim, m(5) tuningtime(600) maxmodels(200) maxiter(10) cv(5)}

{p 4 4 2}
Disable automatic balancing and request balancing for selected variables:

{p 8 8 2} . {bf:mlim, m(5) noautobalance balance(outcome group)}

{p 4 4 2}
Save the imputed dataset to disk:

{p 8 8 2} . {bf:mlim, m(5) filename("imputed_data.dta")}

{p 4 4 2}
Inspect the R arguments generated by the Stata wrapper:

{p 8 8 2} . {bf:mlim, debug}


{title:Stored results}

{p 4 4 2}
This Stata wrapper does not explicitly store documented {bf:r()}, {bf:e()}, or {bf:s()}
results. Its primary result is the imputed dataset left in memory. With multiple
imputation, the dataset is registered as Stata MI data in flong form.


{title:Acknowledgments}

{p 4 4 2}
{bf:mlim} is a Stata interface to the R package  {browse "http://github.com/haghish/mlim":{bf:mlim}} 
and uses  {browse "http://github.com/haghish/rcall":{bf:rcall}} for 
communication between Stata and R. Dataset exchange also relies on the R package
{bf:readstata13} in the single-imputation file-writing path.


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



