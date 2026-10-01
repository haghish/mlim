/***
_v. 0.6.0_

mlim
====

__mlim__ - Single and Multiple Imputation with Automated Machine Learning

Syntax
------

> __mlim__ [, _m(#)_ _algos(string)_ _stochastic_ _nostochastic_
_ignore(varlist)_ _hierarchy(varlist)_ _tuningtime(#)_ _maxmodels(#)_ _maxiter(#)_
_cv(#)_ _nomatching_ _noautobalance_ _seed(#)_
_verbosity(string)_ _report(string)_ _tolerance(#)_ _preimpute(string)_
_cpu(#)_ _save(string)_ _load(string)_ _filename(string)_ _debug_ ]

Description
-----------

__mlim__ imputes missing values in the dataset currently in memory by calling the
R package __mlim__ through __rcall__. By default, it performs a single imputation.
Specify __m(#)__ with a value larger than 1 to carry out multiple imputations.

For a single imputation (__m(1)__), the completed dataset returned by R replaces
the dataset in memory. For multiple imputation (__m()>1__), __mlim__ converts the
R result to Stata's __flong__ format and then runs __mi import flong__. The variables
that were imputed are registered with Stata as imputed variables.

The wrapper follows the current R __mlim::mlim()__ interface. Options that control
model fitting, stochastic imputation, matching, hierarchy, convergence, and
reproducibility are passed directly to R.

Requirements
------------

__mlim__ requires the Stata package __rcall__ and the following R packages:

| Requirement     | Minimum version |
|:----------------|:----------------|
| __mlim__        | 0.6.0           |
| __readstata13__ | 0.11.0          |

R version 4.1.0 or newer is required. The current R implementation uses __mlr3__
and __mlr3tuning__ rather than __h2o__, so Java and an H2O server are not required.
Additional learner packages are required only when their corresponding optional
algorithms are selected.

Options
-------

| _Option_ | _Description_ |
|:---------|:--------------|
| __m(#)__ | Number of imputations.  |
| __algos(string)__ | Supported learners: __ELNET__, __RF__, __CRF__, __GBM__, __XGB__, __LGBM__, __CAT__, __NNET__, __SVM__, __KNN__, and __ENSEMBLE__ |
| __nostochastic__ | Sets __stochastic = FALSE__. Should be avoided in multiple imputation |
| __ignore(varlist)__ | Excludes variables from the imputation. |
| __hierarchy(varlist)__ | Specifies clustering variables from the highest to the lowest level. |
| __tuningtime(#)__ | The maximum tuning time per variable and iteration.  |
| __maxmodels(#)__ | Sets __max_models = #__, the maximum number of hyperparameter evaluations per variable and iteration. |
| __maxiter(#)__ | Sets the maximum number of imputation iterations. |
| __cv(#)__ | Sets the number of cross-validation folds. |
| __nomatching__ | Sets __matching = FALSE__. By default, the R package uses __matching = TRUE__. |
| __noautobalance__ | Sets __autobalance = FALSE__. |
| __seed(#)__ | Sets the R random-number seed. |
| __verbosity(string)__ | Specifies __verbosity__, which can be __warn__, __info__, __debug__, or NULL. |
| __report(string)__ | Specifies report filename  |
| __tolerance(#)__ | Sets the convergence __tolerance__ (minimum relative improvement). |
| __preimpute(string)__ | Sets the initial preimputation method, such as __random__ or __mm__. |
| __cpu(#)__ | Sets the number of CPU threads supplied to learners that support internal multithreading. |
| __save(string)__ | Saves the current imputation state to an __.mlim__ RDS file after variable-level updates. |
| __load(string)__ | Resumes an imputation from a previously saved __.mlim__ state.  |
| __filename(string)__ | Saves the completed data to the specified Stata __.dta__ file in addition to loading it into Stata. |
| __debug__ | Activates debug logging in the report. |

Remarks
-------

### Algorithms

The current implementation supports the following algorithm names:

* __ELNET__: elastic net
* __RF__: random forest
* __CRF__: conditional random forest
* __GBM__: gradient boosting
* __XGB__: XGBoost
* __LGBM__: LightGBM
* __CAT__: CatBoost
* __NNET__: single-hidden-layer neural network
* __SVM__: kernel support vector machine
* __KNN__: k-nearest neighbors
* __ENSEMBLE__: stacked ensemble using the successfully tuned base learners

Some algorithms are provided through __mlr3extralearners__ and therefore require
that package and the corresponding learner package to be installed. For example,
__LGBM__, __CAT__, and __SVM__ use optional learner extensions. __GBM__ is also
skipped for multinomial targets when its current learner does not support multiclass
classification.

### Stochastic imputation and matching

When __stochastic__ is TRUE, continuous numeric predictions receive stochastic
variation based on the model's cross-validation RMSE, whereas categorical predictions
are sampled from their predicted class-probability vectors. Numeric matching is enabled
by default in R. The Stata option __nomatching__ sets __matching = FALSE__. When
matching is enabled and stochastic imputation is used, integer-valued numeric predictions
are stochastically mapped to neighboring observed integer values after the stochastic
numeric value has been generated.

Note that matching is currently in experimental stage!

### Hierarchical imputation

The __hierarchy()__ option specifies nested clustering variables from the highest
to the lowest level. For example, __hierarchy(city school classroom student)__
represents students nested within classrooms, classrooms nested within schools, and
schools nested within cities. Hierarchy variables must exist in the data and cannot
contain missing values.

### Variables selected for imputation

For a new imputation, the R package selects variables that contain missing values
but are not completely missing, excluding variables specified in __ignore()__. The
Stata wrapper uses the same criterion when preparing the variable list needed by
__mi import flong__. String variables are reported to the user; they should be
encoded as numeric categorical variables or placed in __ignore()__.

### Single versus multiple imputation

With __m(1)__, __mlim::mlim()__ returns one completed data frame and __rcall__ loads
it into Stata.

With __m()>1__, the R result is converted by __mlim::mlim.stata()__ to __flong__
format. Stata then runs:

> __mi import flong, m(m) id(id) imputed(varlist)__

where _varlist_ contains the variables imputed by __mlim__. The resulting data are
registered as a Stata multiple-imputation dataset and __mi describe__ is displayed.

### Loading a saved imputation

__load()__ is different from starting a new imputation. The R package reads the
saved __mlim__ state and restores its data, iteration position, model settings,
number of imputations, and other saved options. Therefore, options such as
__algos()__, __m()__, __tuningtime()__, and __maxmodels()__ do not override the
saved state when __load()__ is used.

For a loaded multiple-imputation state, the wrapper determines the number of
imputations from the saved object before deciding whether to run __mi import flong__.
This is important because __m()__ defaults to 1 in the Stata syntax but a saved
state may contain multiple imputations.

__save()__ and __load()__ cannot be specified together. A loaded state restores its
saved __save__ setting, so a new __save()__ path cannot be supplied by the wrapper
when resuming an existing state.

### Protecting the dataset

The command uses __preserve__ before the R call. If R execution or the subsequent
__mi import flong__ fails, the original dataset is restored. On success, the command
uses __restore, not__, retaining the completed data returned by R.

### Reserved names

For multiple imputation, variables named __m__ and __id__ are reserved for the
Stata __flong__ representation. Rename existing variables with these names before
running a multiple imputation.

### R execution

The program checks for __rcall.ado__ and verifies the required R and package versions
with __rcall_check__. R is called in __vanilla__ mode. Errors returned by R are
propagated to Stata after the original dataset is restored.

Examples
--------

Let's first prepare a dataset with missing values

    . sysuse auto, clear
    . replace mpg = . if mod(_n, 7) == 0
    . replace weight = . if mod(_n, 9) == 0
    . encode make, gen(make_cat)
    . drop make

Single imputation using the default algorithm:

> . __mlim__

Multiple imputation with five datasets:

> . __mlim, m(5)__

Use several algorithms and allow up to 10 minutes of tuning per variable and iteration:

> . __mlim, m(5) algos(ELNET RF XGB) tuningtime(600) maxmodels(50)__

Use a hierarchical structure:

> . __mlim, m(5) hierarchy(schoolid childid)__

Disable stochastic imputation and numeric matching:

> . __mlim, m(1) nostochastic nomatching__

Ignore a variable and use a reproducible seed:

> . __mlim, m(5) ignore(length) seed(2026)__

Use four CPU threads:

> . __mlim, m(5) cpu(4)__

Save an imputation state:

> . __mlim, m(5) save("my_imputation.mlim")__

Continue a previously saved imputation:

> . __mlim, load("my_imputation.mlim")__

Save the completed data to a Stata file:

> . __mlim, m(5) filename("imputed_data.dta")__

Inspect the R arguments generated by the Stata wrapper:

> . __mlim, debug__


Acknowledgments
---------------

__mlim__ is a Stata interface to the R package [__mlim__](http://github.com/haghish/mlim)
and uses [__rcall__](http://github.com/haghish/rcall) for communication between Stata
and R. Dataset exchange relies on the R package __readstata13__.

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

- - -

This documentation is written in Markdown inside a MarkDoc documentation block.
After saving the program as __mlim.ado__, generate the Stata help file with:

> . __markdoc "mlim.ado", mini export(sthlp) replace__

***/


program define mlim
    version 14

    syntax [, M(integer 1)                                  ///
        ALGOS(string)                                       ///
        STOCHASTIC                                          ///
        NOSTOCHASTIC                                        ///
        IGNORE(varlist)                                     ///
        HIERARCHY(varlist)                                  ///
        TUNINGTime(numlist integer max=1)                   ///
        MAXModels(numlist integer max=1)                    ///
        MAXITER(numlist integer max=1)                      ///
        CV(numlist integer max=1)                           ///
        NOMATCHING                                          ///
        NOAUTOBALANCE                                       ///
        SEED(numlist integer max=1)                         ///
        VERBOSITY(string)                                   ///
        REPORT(string)                                      ///
        TOLERANCE(numlist max=1)                            ///
        PREIMPUTE(string)                                   ///
        CPU(numlist integer max=1)                          ///
        SAVE(string)                                        ///
        LOAD(string)                                        ///
        FILENAME(string)                                    ///
        DEBUG                                               ///
        ]

    // Syntax checks
    // ============================================================
    if `m' < 1 {
        display as error "m must be 1 or larger"
        exit 198
    }

    if "`stochastic'" != "" & "`nostochastic'" != "" {
        display as error "stochastic and nostochastic cannot be specified together"
        exit 198
    }


    if `"`save'"' != "" & `"`load'"' != "" {
        display as error "save() and load() cannot be specified together"
        exit 198
    }

    // Warn about string variables
    // ============================================================
    quietly ds, has(type string)
    local stringvars `r(varlist)'

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

    rcall_check mlim>=0.6.0 readstata13>=0.11.0, rversion(4.1.0)

    // If a saved state is loaded, determine the number of imputations and
    // the variables that were imputed from the saved object. This is necessary
    // because the Stata m() option defaults to 1 and therefore cannot be used
    // to determine the output type of a loaded state.
    // ============================================================
    local loaded = 0
    local imputed

    if `"`load'"' != "" {
        local load_r = subinstr(`"`load'"', char(92), "/", .)

        capture noisily rcall vanilla:                       ///
            load_state <- readRDS("`load_r'");               ///
            if (!inherits(load_state, "mlim"))               ///
                stop("loaded object must be of class 'mlim'"); ///
            loaded_m <- as.integer(load_state$m);             ///
            loaded_imputed <- paste(load_state$vars2impute,   ///
                                    collapse = " ");           ///
            st.return <- "rc"

        local rc = _rc
        if `rc' {
            exit `rc'
        }

        local m = r(loaded_m)
        local imputed `"`r(loaded_imputed)'"'
        local loaded = 1
    }

    // Identify variables with missing observations for a new imputation.
    // Match the R selectVariables() criterion: a variable must contain
    // missing values but cannot be completely missing.
    // ============================================================
    if `loaded' == 0 {
        quietly ds
        local variables `r(varlist)'

        foreach variable of local variables {
            quietly count if missing(`variable')
            local nmiss = r(N)
            quietly count if !missing(`variable')
            local nobs = r(N)

            if `nmiss' > 0 & `nobs' > 0 {
                local imputed `imputed' `variable'
            }
        }

        if "`ignore'" != "" {
            local imputed : list imputed - ignore
        }

        if "`imputed'" == "" {
            display as error "no variables with missing observations were found"
            exit 198
        }
    }

    // Multiple imputation requires m and id variables.
    // For load(), m has already been recovered from the saved state.
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
    local rargs

    if `loaded' == 1 {
        // A loaded object contains the complete imputation settings.
        // Do not pass new model-fitting options because the R function
        // restores them from the saved state.
        local rargs `"load = "`load_r'""'
    }
    else {
        local rargs `"m = `m'"'

        if `"`algos'"' != "" local rargs `"`rargs', algos = scan(text = "`algos'", what = character(), quiet = TRUE)"'

        // stochastic imputation
        if "`stochastic'" != "" local rargs `"`rargs', stochastic = TRUE"'
        if "`nostochastic'" != "" local rargs `"`rargs', stochastic = FALSE"'

        // variables to ignore
        if "`ignore'" != "" local rargs `"`rargs', ignore = scan(text = "`ignore'", what = character(), quiet = TRUE)"'

        // hierarchy
        if "`hierarchy'" != "" local rargs `"`rargs', hierarchy = scan(text = "`hierarchy'", what = character(), quiet = TRUE)"'

        // tuning and iteration settings
        if "`tuningtime'" != "" local rargs `"`rargs', tuning_time = `tuningtime'"'
        if "`maxmodels'" != "" local rargs `"`rargs', max_models = `maxmodels'"'
        if "`maxiter'" != "" local rargs `"`rargs', maxiter = `maxiter'"'
        if "`cv'" != "" local rargs `"`rargs', cv = `cv'"'

        // numeric matching
        // The R default is matching = TRUE. Stata exposes only
        // nomatching, which explicitly switches matching off.
        if "`nomatching'" != "" local rargs `"`rargs', matching = FALSE"'

        // automatic class balancing
        // The R default is autobalance = TRUE. Stata exposes only
        // noautobalance, which explicitly switches balancing off.
        if "`noautobalance'" != "" local rargs `"`rargs', autobalance = FALSE"'

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

        // preimputation
        if `"`preimpute'"' != "" local rargs `"`rargs', preimpute = "`preimpute'""'

        // CPUs
        if "`cpu'" != "" local rargs `"`rargs', cpu = `cpu'"'

        // save mlim state
        if `"`save'"' != "" {
            local save_r = subinstr(`"`save'"', char(92), "/", .)
            local rargs `"`rargs', save = "`save_r'""'
        }
    }

    // Hidden debug argument
    if "`debug'" != "" {
        local rargs `"`rargs', debug = TRUE"'
    }

    // Preimputed dataset is not a public Stata option in the current wrapper.
    // It remains available as an internal R argument through the R package.

    // Prepare output filename
    // ============================================================
    if `"`filename'"' != "" local filename_r = subinstr(`"`filename'"', char(92), "/", .)

    // Protect the currently loaded dataset
    // ============================================================
    preserve

    if "`debug'" != "" {
        display as txt "calling mlim via Rcall..."
        display as txt `"`rargs'"'
    }

    // The R result determines whether this is a single or multiple imputation.
    // This also makes load() work correctly when the saved state has m > 1.
    // ============================================================
    if `"`filename'"' == "" {
        capture noisily rcall vanilla:                    ///
            df <- st.data();                              ///
            imp <- mlim::mlim(data = df, `rargs');         ///
            if (inherits(imp, "mlim.mi")) {               ///
                stata.data <- mlim::mlim.stata(            ///
                    mlim = imp,                           ///
                    df = df,                              ///
                    format = "flong");                    ///
                st.load(stata.data);                      ///
                mlim_m <- as.integer(length(imp));        ///
            } else {                                       ///
                st.load(imp);                              ///
                mlim_m <- 1L;                              ///
            }                                              ///
            st.return <- "rc"
    }
    else {
        capture noisily rcall vanilla:                    ///
            df <- st.data();                              ///
            imp <- mlim::mlim(data = df, `rargs');         ///
            if (inherits(imp, "mlim.mi")) {               ///
                stata.data <- mlim::mlim.stata(            ///
                    mlim = imp,                           ///
                    df = df,                              ///
                    format = "flong",                     ///
                    filename = "`filename_r'");            ///
                st.load(stata.data);                      ///
                mlim_m <- as.integer(length(imp));        ///
            } else {                                       ///
                outfile <- "`filename_r'";                 ///
                if (!endsWith(tolower(outfile), ".dta"))  ///
                    outfile <- paste0(outfile, ".dta");   ///
                readstata13::save.dta13(                   ///
                    data = imp,                            ///
                    file = outfile,                        ///
                    convert.factors = TRUE,                ///
                    add.rownames = FALSE);                 ///
                st.load(imp);                              ///
                mlim_m <- 1L;                              ///
            }                                              ///
            st.return <- "rc"
    }

    // Check R execution
    // ============================================================
    local rc = _rc
    if `rc' {
        restore
        exit `rc'
    }

    // Use the actual number of imputations returned by R.
    // ============================================================
    local returned_m = r(mlim_m)

    if `returned_m' > 1 {
        capture noisily mi import flong,                  ///
            m(`returned_m')                               ///
            id(id)                                         ///
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
    if `returned_m' > 1 {
        mi describe
    }

end
