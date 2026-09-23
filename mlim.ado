/***
_v. 01_

mlim
====

__mlim__ - Single and Multiple Imputation with Automated Machine Learning

Syntax
------

> __mlim__ [, _m(#)_ _algos(string)_ _stochastic_ _nostochastic_
_ignore(varlist)_ _tuningtime(#)_ _maxmodels(#)_ _maxiter(#)_
_cv(#)_ _matching_ _noautobalance_ _balance(varlist)_ _seed(#)_
_verbosity(string)_ _report(string)_ _tolerance(#)_ _nodoublecheck_
_preimpute(string)_ _cpu(#)_ _ram(#)_ _flush_ _save(string)_
_load(string)_ _java(string)_ _filename(string)_
_debug_ ]

Description
-----------

__mlim__ imputes missing values in the dataset currently in memory by calling the
R package __mlim__ through __rcall__. By default, it performs a single imputation.
Specify __m(#)__ with a value larger than 1 to carry out multiple imputations.

The command first identifies variables containing missing observations. Variables
listed in __ignore()__ are excluded from imputation. If string variables are present,
__mlim__ returns an error and requests encoding them as numeric categorical variables
or placing them in __ignore()__.

For a single imputation (__m(1)__), the imputed dataset returned by R replaces the
dataset in memory. For multiple imputation (__m()>1__), the R result is converted to
__flong__ form with and then imported with Stata's __mi import flong__. 
In this case, __mlim__ reserves the variable names __m__ and
__id__ for appending the imputed datasets. 

Requirements
------------

__mlim__ requires the Stata package __rcall__ and the following R packages:

| Requirement     | Minimum version |
|:----------------|:----------------|
| __mlim__        | 0.4.0           |
| __readstata13__ | 0.11.0          |
| __h2o__         | -               |

R and Java Runtime should also be accessible via path environment.

Options
-------

| _Option_            | _Description_                                                                |
|:--------------------|:-----------------------------------------------------------------------------|
| __m(#)__            | Number of imp utations. The default is 1 and values must be at least 1. |
| __algos(string)__   | Passes a space-separated set of algorithm names to the R argument __algos__. |
| __stochastic__      | Passes __stochastic = TRUE__ to R. Experimental in this source. |
| __nostochastic__    | Passes __stochastic = FALSE__ to R. May not be combined with __stochastic__. Experimental feature.     |
| __ignore(varlist)__ | Excludes variables from the set of variables to be imputed |
| __tuningtime(#)__   | Passes __tuning_time = #__ to R. |
| __maxmodels(#)__    | Passes __max_models = #__ to R. |
| __maxiter(#)__      | Passes __maxiter = #__ to R. |
| __cv(#)__           | Passes __cv = #__ to R. |
| __matching__        | Experimental option related to predictive matching. See Remarks below. |
| __noautobalance__   | Turns off class imbalance correction in single imputation |
| __balance(varlist)__ | Passes the listed variables to R as __balance__. Experimental in this source. |
| __seed(#)__         | Passes the integer random-number seed to R. |
| __verbosity(string)__ | Passes __verbosity__ to R. |
| __report(string)__    | Passes a report path or report specification to R. |
| __tolerance(#)__    | Passes the convergence __tolerance__ to R. |
| __nodoublecheck__   | Passes __doublecheck = FALSE__ to R. Experimental in this source. |
| __preimpute(string)__ | Passes __preimpute__ to R. |
| __cpu(#)__ | Passes the requested number of CPUs to R. |
| __ram(#)__ | Passes the requested RAM value to R. |
| __flush__ | Passes __flush = TRUE__ to R, requesting cleanup of H2O models. |
| __save(string)__    | Passes __save__ to the R package to save its imputation state (recommended). |
| __load(string)__    | Passes __load__ to the R package to load a previously saved imputation state. |
| __java(string)__    | Passes a Java path to R. Backslashes are converted to forward slashes.              |
| __filename(string)__ | Saves the imputed data to a Stata __.dta__ file (recommended).                  |
| __debug__           |                                                  Used for debugging the program.|

Remarks
-------

### Variables selected for imputation

The command scans all variables in the dataset and selects every variable containing
at least one missing observation. Variables specified in __ignore()__ are then removed
from that list. If no variables remain, the command exits with an error.

String variables are not automatically encoded. The command warns when string
variables remain outside __ignore()__, but the current source does not stop execution
after that warning.

### Single versus multiple imputation

With __m(1)__, __mlim::mlim()__ returns one completed dataset and __rcall__ loads
that dataset into Stata.

With __m()>1__, the command asks __mlim::mlim.stata()__ to create data in
__flong__ format. Stata then runs:

> __mi import flong, m(m) id(id) imputed(varlist)__

where _varlist_ is the set of variables that had missing values before imputation,
after exclusions in __ignore()__. The resulting data remain in memory and
__mi describe__ is displayed.

### Protecting the dataset

Before calling R, the command uses __preserve__. If R execution or the subsequent
__mi import flong__ fails, the original data are restored. On success, the command
uses __restore, not__, retaining the imputed dataset loaded by R.

### Experimental matching option

The current syntax declares __matching__ as a switch. The implementation then treats
its local macro as though it could contain values such as TRUE, FALSE, or AUTO.
Consequently, in this development snapshot, specifying __matching__ does not enable
matching; the generated R argument falls through to __matching = FALSE__. This option
should therefore be regarded as under development until its syntax and implementation
are reconciled.

### Reserved names

When __m()>1__, variables named __m__ and __id__ are reserved for conversion to
Stata's flong MI representation. Rename existing variables with those names before
calling __mlim__.

### R execution

The program checks for __rcall.ado__ and calls __rcall_check__ before imputation.
All imputation calls use __rcall vanilla__, which starts a fresh R session for the call.
Errors returned by R are propagated to Stata after the original dataset is restored.

Examples
--------

Let's first prepare a dataset with missing values

    . sysuse auto, clear
    . replace mpg = . if mod(_n, 7) == 0
    . replace weight = . if mod(_n, 9) == 0
    . encode make, gen(make_cat)              // note that the make variable is encoded
    . drop make

Single imputation using default arguments:

> . __mlim__

Impute five datasets:

> . __mlim, m(5)__

Ignore a variable and use a reproducible seed:

> . __mlim, m(5) ignore(length) seed(2026)__

Limit computational resources to 4 CPU and 8GB of RAM:

> . __mlim, m(5) cpu(4) ram(8)__

Spend up to 10 minutes on hyperparameter tuning for each variable in each itteration:

> . __mlim, m(5) tuningtime(600) maxmodels(200) maxiter(10) cv(5)__

Disable automatic balancing and request balancing for selected variables:

> . __mlim, m(5) noautobalance balance(outcome group)__

Save the imputed dataset to disk:

> . __mlim, m(5) filename("imputed_data.dta")__

Inspect the R arguments generated by the Stata wrapper:

> . __mlim, debug__

Stored results
--------------

This Stata wrapper does not explicitly store documented __r()__, __e()__, or __s()__
results. Its primary result is the imputed dataset left in memory. With multiple
imputation, the dataset is registered as Stata MI data in flong form.

Acknowledgments
---------------

__mlim__ is a Stata interface to the R package [__mlim__](http://github.com/haghish/mlim) 
and uses [__rcall__](http://github.com/haghish/rcall) for 
communication between Stata and R. Dataset exchange also relies on the R package
__readstata13__ in the single-imputation file-writing path.

Author
------

E. F. Haghish  
Faculty of Psychological Sciences  
University of Bergen  
haghish@uib.no  

[rcall Homepage](github.com/haghish/mlim)  
Package Updates on [X](http://www.x.com/Haghish)  

License
-------

_MIT License_

Generating the help file
------------------------

This documentation is written in Markdown inside a MarkDoc documentation block.
After saving the program as __mlim.ado__, generate the Stata help file with:

> . __markdoc "mlim.ado", mini export(sthlp) replace__

***/


capture program drop mlim
program define mlim
    version 14

    syntax [, M(integer 1)                                  ///
		ALGOS(string)                                       ///
		STOCHASTIC                                          /// UNDER TESTING
		NOSTOCHASTIC                                        /// UNDER TESTING
		IGNORE(varlist)                                     ///
		TUNINGTime(numlist integer max=1)                   ///
		MAXModels(numlist integer max=1)                    ///
		MAXITER(numlist integer max=1)                      ///
		CV(numlist integer max=1)                           ///
		MATCHING                                            /// UNDER TESTING
		NOAUTOBALANCE                                       /// UNDER TESTING
		SEED(numlist integer max=1)                         ///
		VERBOSITY(string)                                   ///
		REPORT(string)                                      ///
		TOLERANCE(numlist max=1)                            ///
		NODOUBLECHECK                                       /// UNDER TESTING
		PREIMPUTE(string)                                   ///
		CPU(numlist integer max=1)                          ///
		RAM(numlist max=1)                                  ///
		FLUSH                                               ///
		SAVE(string)                                        ///
		LOAD(string)                                        ///
		JAVA(string)                                        ///
		FILENAME(string)                                    ///
		DEBUG                                               ///
		]                                  
		      
		///POSTIMPUTE                                          ///
		///PREIMPUTED(string asis)                             ///
		///NOSHUTDOWN                                          /// NOT APPLICABLE
		///BALANCE(varlist)                                    /// UNDER TESTING

    // SYntax check
    // ============================================================
    if `m' < 1 {
        display as error "m must be 1 or larger"
        exit 198
    }
	
    if "`stochastic'" != "" & "`nostochastic'" != "" {
        display as error "stochastic and nostochastic cannot be specified together"
        exit 198
    }
	
	// Warn about string variables
	// ============================================================
	quietly ds, has(type string)
	local stringvars `r(varlist)'

	// Ignore string variables already specified in ignore()
	if "`ignore'" != "" {
		local stringvars : list stringvars - ignore
	}

	if "`stringvars'" != "" {
		display as error "string variables found:"
		display as error "`stringvars'"
		display as txt "encode them to categorical numeric variables or include them in ignore()."
	}

    // Check rcall and required R packages
    // ============================================================
    capture findfile rcall.ado
    if _rc {
        display as error "rcall package is required"
        exit 198
    }
	
    rcall_check mlim>=0.3.0 readstata13>=0.11.0  //UPDATE mlim to 0.4.0

    // Identify variables with missing observations that should be imputed
    // ============================================================
    local imputed
    quietly ds
    local variables `r(varlist)'
    foreach variable of local variables {
        quietly count if missing(`variable')
        if r(N) > 0 {
            local imputed `imputed' `variable'
        }
    }

    // Remove ignored variables from the list of imputed variables
    if "`ignore'" != "" {
        local imputed : list imputed - ignore
    }

    if "`imputed'" == "" {
        display as error "no variables with missing observations were found"
        exit 198
    }
	
	
    // Multiple imputation requires m and id variables
    // ============================================================
    if `m' > 1 {
        capture confirm variable m
        if !_rc {
            display as error `"variable "m" already exists in the dataset. this name is preserved for mlim"'
            exit 110
        }

        capture confirm variable id
        if !_rc {
            display as error `"variable "id" already exists in the dataset. this name is preserved for mlim"'
            exit 110
        }
    }


    // Prepare mlim() arguments
    // ============================================================
    local rargs `"m = `m'"'

    if `"`algos'"' != "" local rargs `"`rargs', algos = scan(text = "`algos'", what = character(), quiet = TRUE)"'

    // stochastic imputation (UNDER TESTING)
    if "`stochastic'" != "" local rargs `"`rargs', stochastic = TRUE"'

    if "`nostochastic'" != "" local rargs `"`rargs', stochastic = FALSE"'

    // variables to ignore
    if "`ignore'" != "" local rargs `"`rargs', ignore = scan(text = "`ignore'", what = character(), quiet = TRUE)"'

    // tuning
    if "`tuningtime'" != "" local rargs `"`rargs', tuning_time = `tuningtime'"'
    if "`maxmodels'" != "" local rargs `"`rargs', max_models = `maxmodels'"'
    if "`maxiter'" != "" local rargs `"`rargs', maxiter = `maxiter'"'
    if "`cv'" != "" local rargs `"`rargs', cv = `cv'"'

    // matching (must be specified because it is an experimental feature)
    if `"`matching'"' != "" {
        local matching_upper = upper(`"`matching'"')

        if "`matching_upper'" == "TRUE" | "`matching_upper'" == "T" {
            local rargs `"`rargs', matching = TRUE"'
        }
        else if "`matching_upper'" == "FALSE" | "`matching_upper'" == "F" {
            local rargs `"`rargs', matching = FALSE"'
        }
		else if "`matching_upper'" == "AUTO" | "`matching_upper'" == "auto" {
            local rargs `"`rargs', matching = AUTO"'
        }
		
		// if matching is not specified, turn it off! 
        else {
            local rargs `"`rargs', matching = FALSE"'
        }
    }

    // automatic class balancing
    if "`noautobalance'" != "" local rargs `"`rargs', autobalance = FALSE"'
	
    // variables to balance
    //if "`balance'" != "" local rargs `"`rargs', balance = scan(text = "`balance'", what = character(), quiet = TRUE)"'

    // random seed
    if "`seed'" != "" local rargs `"`rargs', seed = `seed'"'

    // verbosity
    if `"`verbosity'"' != "" local rargs `"`rargs', verbosity = "`verbosity'""'

    // report
    if `"`report'"' != "" {
        local report_r = subinstr(`"`report'"', char(92), "/", .)
        local rargs `"`rargs', report = "`report_r'""'
    }

    // convergence tolerance
    if "`tolerance'" != "" local rargs `"`rargs', tolerance = `tolerance'"'

    // double checking
    if "`nodoublecheck'" != "" local rargs `"`rargs', doublecheck = FALSE"'

    // preimputation
    if `"`preimpute'"' != "" local rargs `"`rargs', preimpute = "`preimpute'""'

    // CPUs
    if "`cpu'" != "" local rargs `"`rargs', cpu = `cpu'"'

    // RAM
    if "`ram'" != "" local rargs `"`rargs', ram = `ram'"'

    // flush H2O models
    if "`flush'" != "" local rargs `"`rargs', flush = TRUE"'

    // save mlim state
    if `"`save'"' != "" {
        local save_r = subinstr(`"`save'"', char(92), "/", .)
        local rargs `"`rargs', save = "`save_r'""'
    }

    // load mlim state
    if `"`load'"' != "" {
        local load_r = subinstr(`"`load'"', char(92), "/", .)
        local rargs `"`rargs', load = "`load_r'""'
    }

    // Java path
    if `"`java'"' != "" {
        local java_r = subinstr(`"`java'"', char(92), "/", .)
        local rargs `"`rargs', java = "`java_r'""'
    }

    // Preimputed dataset
    // ============================================================
    local precode

    if `"`preimputed'"' != "" {
        local preimputed_r = subinstr(`"`preimputed'"', char(92), "/", .)
        local precode `"preimp <- st.data("`preimputed_r'");"'
        local rargs `"`rargs', preimputed.data = preimp"'
    }

    // Prepare output filename
    // ============================================================
    if `"`filename'"' != "" local filename_r = subinstr(`"`filename'"', char(92), "/", .)

    // Protect the currently loaded dataset
    // ============================================================
    preserve
	
	
	// TEST THE CODE
	if "`debug'" != "" {
		display `"`precode'"'
		display `"`rargs'"'
	}

	if `"`verbosity'"' != "" display "calling mlim via Rcall..."

    // Single imputation
    // ============================================================
    if `m' == 1 {
        if `"`filename'"' == "" {
            capture noisily rcall vanilla:                    ///
                df <- st.data();                              ///
                `precode'                                     ///
                imp <- mlim::mlim(data = df, `rargs');        ///
                st.load(imp);                                 ///
                st.return <- "rc"
        }

        else {
            capture noisily rcall vanilla:                    ///
                df <- st.data();                              ///
                `precode'                                     ///
                imp <- mlim::mlim(data = df, `rargs');        ///
                outfile <- "`filename_r'";                    ///
                if (!endsWith(tolower(outfile), ".dta"))      ///
                    outfile <- paste0(outfile, ".dta");       ///
                readstata13::save.dta13(                      ///
                    data = imp,                               ///
                    file = outfile,                           ///
                    convert.factors = TRUE,                   ///
                    add.rownames = FALSE);                    ///
                st.load(imp);                                 ///
                st.return <- "rc"
        }
    }


    // Multiple imputation
    // ============================================================
    else {
        if `"`filename'"' == "" {
            capture noisily rcall vanilla:                    ///
                df <- st.data();                              ///
                `precode'                                     ///
                imp <- mlim::mlim(data = df, `rargs');        ///
                stata.data <- mlim::mlim.stata(               ///
                    mlim = imp,                               ///
                    df = df,                                  ///
                    format = "flong");                        ///
                st.load(stata.data);                          ///
                st.return <- "rc"
        }

        else {
            capture noisily rcall vanilla:                    ///
                df <- st.data();                              ///
                `precode'                                     ///
                imp <- mlim::mlim(data = df, `rargs');        ///
                stata.data <- mlim::mlim.stata(               ///
                    mlim = imp,                               ///
                    df = df,                                  ///
                    format = "flong",                         ///
                    filename = "`filename_r'");               ///
                st.load(stata.data);                          ///
                st.return <- "rc"
        }
    }


    // Check R execution
    // ============================================================
    local rc = _rc
    if `rc' {
        restore
        exit `rc'
    }

    // Import flong data as a Stata MI dataset
    // ============================================================
    if `m' > 1 {
        capture noisily mi import flong,                      ///
            m(m)                                              ///
            id(id)                                            ///
            imputed(`imputed')
        local rc = _rc
        if `rc' {
            restore
            exit `rc'
        }
    }

    // Keep the imputed dataset in memory
    // ============================================================
    restore, not

    // Describe multiple imputation data
    // ============================================================
    if `m' > 1 {
        mi describe
    }


end
