#' @title missing data imputation with automated machine learning
#' @description imputes data.frame with mixed variable types using automated
#'              machine learning (AutoML)
#'
# @param preimpute character. specifies the 'primary' procedure of handling the missing
#                  data, before optimization takes place. the default procedure
#                  is "rf", which models the missing data with parallel Random Forest
#                  model. this is a very fast procedure, which will be refined within the
#                  "imputation" algorithms (see below). possible alternative is \code{"mm"},
#                  which carries out mean/mode replacement. "mm" is much faster than "rf", but
#                  will increase the number of required itterations, significantly adding
#                  to the required computational resources. if your dataset is very
#                  large, consider imputing it before hand and then passing the
#                  imputed dataset for optimization (see "preimputed.data" argument)
# @param impute character. specify a vector of algorithms to be used
#        in the process of auto-tuning. the supported main algorithms are
#        \code{"ELNET"}, \code{"RF"},
#        \code{"GBM"}, \code{"DL"}, \code{"XGB"} (available for Mac and Linux), and \code{"Ensemble"}.
#
#        the default is \code{"AUTO"}, which is mostly based on "ELNET", but also
#        uses Extremely Randomized Forests, in addition to Random Forest, before
#        concluding the imputation, when "ELNET" stops improving. This procedure is
#        relatively fast and yields charming results, often equal to specifying "impute = c('ELNET', 'RF')",
#        which at each step of the imputation, uses both "ELNET" and "RF", doubling the imputation
#        time, and thus, it is advised.
#
#        "GBM", "DL", "XGB", and "Ensemble" take the full given "tuning_time" (see below) to
#        tune the best model for imputing he given variable. it is advised to use these extensive
#        algorithms in the process of "postimputation" and let "ELNET" do most of the legwork to save
#        computational resources.
# @param postimpute character. specify a vector of algorithms - that are computationally extensive -
#        to be used to optimize the imputed results. default imputation. possible algorithms are
#        \code{"GBM"}, \code{"DL"}, \code{"XGB"} (available for Mac and Linux), and \code{"Ensemble"}.
#        the default value is "GBM". postimputation will only run if maximum iteration limit is not
#        reached. if you specify more than 1 postimputation algorithm, the "tuning_time" will be
#        devided between different algorithms, but not necessarily equally. generally, "GBM" and "XGB"
#        tune faster than "DL", and thus are advised to non-expert users, for general purposes.
#
#        postimputation fullz consumes the given "tuning_time" to tune a computationally extensive
#        algorithm and see whether it outperform "ELNET". this procedure extensively consumes RAM,
#        so make sure you know the limits of your machine before administering it. otherwise,
#        set this argument to NULL to deactivate postimputation.
#'
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output packageVersion
#' @importFrom h2o h2o.init as.h2o h2o.automl h2o.predict h2o.ls
#'             h2o.removeAll h2o.rm h2o.shutdown
#' @importFrom md.log md.log
#' @importFrom memuse Sys.meminfo
#' @importFrom stats var setNames na.omit
#' @importFrom curl curl
#' @param data a \code{data.frame} (strictly) with missing data to be
#'             imputed. if \code{'load'} argument is provided, this argument will be ignored.
#' @param m integer, specifying number of multiple imputations. the default value is
#'          1, carrying out a single imputation.
#' @param algos character vector, specifying algorithms to be used for missing data
#'              imputation. supported algorithms are "ELNET", "RF", "GBM", "DL",
#'              "XGB", and "Ensemble". if more than one algorithm is specified,
#'              mlim changes behavior to save on runtime. for example,
#'              the default is "ELNET", which only uses Elastic Net for the imputation.
#'              However, 'algos = c("ELNET", "GBM")' will not only use
#'              ELNET for the initial imputation, but also, uses 'GBM'
#'              as long as the 'maxiter' argument is not reached or GBM stops
#'              improving. However, note that by specifying more than one algorithm,
#'              "mlim" does not fine-tune them all together. Instead, it carries
#'              out imputation with the first one and when the algorithm stops
#'              improving, it follows with postimputation, (in this example "GBM")
#'              to further optimize the imputations. the reason for having
#'              this setup is that in general, "ELNET" fine-tunes much faster than "GBM",
#'              "XGB", and "DL".
#'
#'              Note that code{"XGB"} is only available in Mac OS and Linux. moreover,
#'              "GBM", "DL", "XGB", and "Ensemble" take the full given "tuning_time" (see below) to
#'              tune the best model for imputing he given variable.
# @param min_ram character. specifies the minimum size.
#' @param ignore character vector of column names or index of columns that should
#'               should be ignored in the process of imputation.
#' @param tuning_time integer. maximum runtime (in seconds) for fine-tuning the
#'                               imputation model for each variable in each iteration. the default
#'                               time is 600 seconds but for a large dataset, you
#'                               might need to provide a larger model development
#'                               time. this argument also influences \code{max_models},
#'                               see below.
#' @param max_models integer. maximum number of models that can be generated in
#'                   the proecess of fine-tuning the parameters. this value
#'                   default to 100, meaning that for imputing each variable in
#'                   each iteration, up to 100 models can be fine-tuned. increasing
#'                   this value should be consistent with increasing
#'                   \code{max_model_runtime_secs}, allowing the model to spend
#'                   more time in the process of individualized fine-tuning.
#'                   as a result, the better tuned the model, the more accurate
#'                   the imputed values are expected to be
#' @param autobalance logical. if TRUE, binary and multinomial factor variables
#'                    will be balanced before the imputation to increase the
#'                    Mean Per Class Error (MPCE) in the process of optimization.
#'                    if FALSE, MMPCE will be sacrificed for overall accuracy, which
#'                    is not recommended. in fact, higher overall accuracy does
#'                    not mean a better imputation as long as minority classes
#'                    are neglected, which increases the bias in favor of the
#'                    majority class. if you do not wish to autobalance all the
#'                    factor variables, you can manually specify the variables
#'                    that should be balanced using the 'balance' argument (see below)
#' @param balance character vector, specifying variable names that should be
#'                balanced before imputation. balancing the prevalence might
#'                decrease the overall accuracy of the imputation, because it
#'                attempts to ensure the representation of the rare outcome.
#'                this argument is optional and intended for advanced users that
#'                impute a severely imbalance categorical (nominal) variable.
#' @param matching logical. if \code{TRUE}, imputed values are coerced to the
#'                 closest value to the non-missing values of the variable.
#'                 if set to "AUTO", 'mlim' decides whether to match
#'                 or not, based on the variable classes. the default is "AUTO".
# @param ignore.rank logical, if FALSE (default), ordinal variables
#                    are imputed as continuous integers with regression plus matching
#                    and are reverted to ordinal later again. this procedure is
#                    recommended. if FALSE, the rank of the categories will be ignored
#                    the the algorithm will try to optimize for classification accuracy.
#                    WARNING: the latter often results in very high classification accuracy but at
#                    the cost of higher rank error. see the "mlim.error" function
#                    documentation to see how rank error is computed. therefore, if you
#                    intend to carry out analysis on the rank data as numeric, it is
#                    recommended that you set this argument to FALSE.
#' @param maxiter integer. maximum number of iterations. the default value is \code{15},
#'        but it can be reduced to \code{3} (not recommended, see below).
# @param miniter integer. minimum number of iterations. the default value is
#                2.
# @param flush logical (experimental). if TRUE, after each model, the server is
#              cleaned to retrieve RAM. this feature is in testing mode and is
#              currently set to TRUE.
# @param cv logical. specify number of k-fold Cross-Validation (CV). values of
#               10 or higher are recommended. default is 10.
# @param error_metric character. specify the minimum improvement
#                                  in the estimated error to proceed to the
#                                  following iteration or stop the imputation.
#                                  the default is 10^-4 for \code{"MAE"}
#                                  (Mean Absolute Error). this criteria is only
#                                  applied from the end of the fourth iteration.
#                                  \code{"RMSE"} (Root Mean Square
#                                  Error). other possible values are \code{"MSE"},
#                                  \code{"MAE"}, \code{"RMSLE"}.
#' @param tolerance numeric. the minimum rate of improvement in estimated error metric
#'                  of a variable to qualify the imputation for another round of iteration,
#'                  if the \code{maxiter} is not yet reached. any improvement of imputation
#'                  is desirable.  however, specifying values above 0 can reduce the number
#'                  of required iterations at a marginal increase of imputation error.
#'                  for larger datasets, value of "1e-3" is recommended t reduce number
#'                  of iterations. the default value is 1e-4.
#' @param doublecheck logical. default is TRUE (which is conservative). if FALSE, if the estimated
#'                    imputation error of a variable does not improve, the variable
#'                    will be not reimputed in the following iterations. in general,
#'                    deactivating this argument will slightly reduce the imputation
#'                    accuracy, however, it significantly reduces the computation time.
#'                    if your dataset is large, you are advised to set this argument to
#'                    FALSE. (EXPERIMENTAL: consider that by avoiding several iterations
#'                    that marginally improve the imputation accuracy, you might gain
#'                    higher accuracy by investing your computational resources in fine-tuning
#'                    better algorithms such as "GBM")
#'
# @param stopping_metric character.
# @param stopping_rounds integer.
# @param stopping_tolerance numeric.
#' @param weights_column non-negative integer. a vector of observation weights
#'                       can be provided, which should be of the same length
#'                       as the dataframe. giving an observation a weight of
#'                       Zero is equivalent of ignoring that observation in the
#'                       model. in contrast, a weight of 2 is equivalent of
#'                       repeating that observation twice in the dataframe.
#'                       the higher the weight, the more important an observation
#'                       becomes in the modeling process. the default is NULL.
#' @param seed integer. specify the random generator seed
# @param plot logical. If TRUE, estimated error of the imputed dataset is plotted,
#        showing the reduction in CV error

#' @param report filename. if a filename is specified (e.g. report = "mlim.md"), the \code{"md.log"} R
#'               package is used to generate a Markdown progress report for the
#'               imputation. the format of the report is adopted based on the
#'               \code{'verbosity'} argument. the higher the verbosity, the more
#'               technical the report becomes. if verbosity equals "debug", then
#'               a log file is generated, which includes time stamp and shows
#'               the function that has generated the message. otherwise, a
#'               reduced markdown-like report is generated. default is NULL.
#' @param verbosity character. controls how much information is printed to console.
#'                  the value can be "warn" (default), "info", "debug", or NULL.
# @param init logical. should h2o Java server be initiated? the default is TRUE.
#             however, if the Java server is already running, set this argument
#'             to FALSE.
#' @param cpu integer. number of CPUs to be dedicated for the imputation.
#'                 the default takes all of the available CPUs.
#' @param ram integer. specifies the maximum size, in Gigabytes, of the
#'                     memory allocation. by default, all the available memory is
#'                     used for the imputation.
#'                     large memory size is particularly advised, especially
#'                     for multicore processes. the more you give the more you get!
# @param shutdown logical. if TRUE, h2o server is closed after the imputation.
#                 the default is TRUE
#' @param preimputed.data data.frame. if you have used another software for missing
#'                      data imputation, you can still optimize the imputation
#'                      by handing the data.frame to this argument, which will
#'                      bypass the "preimpute" procedure.
#' @param save (NOT YET IMPLEMENTED FOR R). filename. if a filename is specified, an \code{mlim} object is
#'             saved after the end of each variable imputation. this object not only
#'             includes the imputed dataframe and estimated cross-validation error, but also
#'             includes the information needed for continuing the imputation,
#'             which is very useful feature for imputing large datasets, with a
#'             long runtime. this argument is activated by default and an
#'             mlim object is stored in the local directory named \code{"mlim.rds"}.
#' @param load (NOT YET IMPLEMENTED FOR R). an object of class "mlim", which includes the data, arguments,
#'                 and settings for re-running the imputation, from where it was
#'                 previously stopped. the "mlim" object saves the current state of
#'                 the imputation and is particularly recommended for large datasets
#'                 or when the user specifies a computationally extensive settings
#'                 (e.g. specifying several algorithms, increasing tuning time, etc.).
# @param force.load (NOT YET IMPLEMENTED FOR R).logical (default is TRUE). if TRUE, when loading the mlim class
#                 object, its preserved settings are used for restoring and saving the
#                 following itterations. otherwise, if FALSE, the current arguments of
#                 mlim are used to overpower the settings of the mlim object. the settings
#                 include the full list of the mlim arguments.
# @param ... Arguments passed to \code{h2o.automl()}.
#            The following arguments are e.g. incompatible with \code{h2o}
#' @return a \code{data.frame}, showing the
#'         estimated imputation error from the cross validation within the data.frame's
#'         attribution
#' @author E. F. Haghish
#'
#' @examples
#'
#' \donttest{
#' data(iris)
#' irisNA <- mlim.na(iris, p = 0.1, stratify = TRUE, seed = 2022)
#'
#' # run the default imputation (fastest imputation via 'mlim')
#' MLIM <- mlim(irisNA)
#'
#' # or if you want to carry out multiple imputation with 5 datasets
#' MLIM <- mlim(irisNA, m = 5)
#'
#' # you can check the accuracy of the imputation, if you have the original dataset
#' mlim.error(MLIM, irisNA, iris)
#'
#' ## run GBM model and allow 15 minutes of tuning for each variable
#' ## this requires a lot of RAM on your machine
#' # MLIM <- mlim(irisNA, impute = "GBM", tuning_time=60*15)
#' # mlim.error(MLIM, irisNA, iris)
#'
#' # if you have a larger data, there is a few things you can set to make the
#' # algorithm faster, yet, having only a marginal accuracy reduction as a trade-off
#' MLIM <- mlim(irisNA, algos = 'ELNET', tolerance = 1e-3, doublecheck = FALSE)
#' }
#' @export


mlim <- function(data = NULL,
                 m = 1,
                 algos = c("ELNET"), #impute, postimpute
                 ignore = NULL,

                 # computational resources
                 tuning_time = 180,
                 max_models = NULL, # run all that you can
                 maxiter = 10L,
                 #miniter = 2L,
                 #cv = 10L,
                 #validation = 0,

                 matching = "AUTO",    #EXPERIMENTAL
                 autobalance = TRUE,
                 balance = NULL,       #EXPERIMENTAL
                 #ignore.rank = FALSE, #to ignore it, they should make it unordered!
                 weights_column = NULL,

                 # report and reproducibility
                 seed = NULL,
                 verbosity = NULL,
                 report = NULL,

                 # stopping criteria
                 tolerance = 1e-3,
                 doublecheck = TRUE,

                 ## simplify the settings by taking these arguments out
                 #preimpute = "rf",
                 #impute = "AUTO",
                 #postimpute = "AUTO",
                 #error_metric  = "RMSE", #??? mormalize it
                 #stopping_metric = "AUTO",
                 #stopping_rounds = 3,
                 #stopping_tolerance=1e-3,

                 # setup the h2o cluster
                 cpu = -1,
                 ram = NULL,
                 #flush = TRUE,
                 #init = TRUE,
                 #shutdown = TRUE,

                 # NOT YET IMPLEMENTED
                 preimputed.data = NULL,
                 save = NULL,
                 load = NULL
                 #force.load = TRUE,
                 #...
                 ) {


  # initial warnings
  # ============================================================
  if (m > 1) {
    cat("multiple imputation feature is in test-mode and its algorithm can change in the future\n")
  }

  # improvements for the next release
  # ============================================================
  # instead of using all the algorithms at each iteration, add the
  #    other algorithms when the first algorithm stops being useful.
  #    perhaps this will help optimizing, while reducing the computation burdon
  # h2o DRF does not give OOB error, so initial comparison preimputation is not possible
  #    HOWEVER, I can estimate the CV for the preimputation procedure
  #
  # instead of adding postimpute, extract it from specified algorithms

  # Simplify the syntax by taking arguments that are less relevant to the majority
  # of the users out
  # ============================================================
  #stopping_metric <- "AUTO"
  #stopping_rounds <- 3
  #stopping_tolerance <- 1e-3
  MI          <- list()
  bdata       <- NULL
  metrics     <- NULL
  error       <- NULL
  debug       <- FALSE
  cv          <- 10L
  miniter     <- 2L
  init        <- TRUE
  shutdown    <- TRUE
  flush       <- TRUE
  verbose     <- 0
  error_metric<- "RMSE"
  preimpute   <- "RF"
  ignore.rank <- FALSE #EXPERIMENTAL
  sleep       <- .25
  set.seed(seed)



  # ============================================================
  # ============================================================
  # LOAD SETTINGS FROM mlim class object
  # ============================================================
  # ============================================================
  if (!is.null(load)) {
    if (inherits(load, "character")) load <- readRDS(load)
    if (!inherits(load, "mlim")) stop("loaded object must be of class 'mlim'")

    # Data
    # ----------------------------------
    MI          <- load$MI
    dataNA      <- load$dataNA
    data        <- load$data
    bdata       <- load$bdata
    dataLast    <- load$dataLast
    metrics     <- load$metrics
    mem         <- load$mem
    orderedCols <- load$orderedCols

    # Loop data
    # ----------------------------------
    m           <- load$m
    m.it        <- load$m.it
    k           <- load$k
    z           <- load$z
    X           <- load$X
    Y           <- load$Y
    vars2impute <- load$vars2impute
    FAMILY      <- load$FAMILY

    # settings
    # ----------------------------------
    ITERATIONVARS  <- load$ITERATIONVARS
    impute         <- load$impute
    postimpute     <- load$postimpute
    autobalance    <- load$autobalance
    balance        <- load$balance
    ignore         <- load$ignore
    save           <- load$save
    maxiter        <- load$maxiter
    miniter        <- load$miniter
    cv             <- load$cv
    tuning_time    <- load$tuning_time
    max_models     <- load$max_models
    matching       <- load$matching
    ignore.rank    <- load$ignore.rank #KEEP IT HIDDEN
    weights_column <- load$weights_column
    seed           <- load$seed
    verbosity      <- load$verbosity
    verbose        <- load$verbose #KEEP IT HIDDEN
    debug          <- load$debug   #KEEP IT HIDDEN
    report         <- load$report
    flush          <- load$flush
    error_metric   <- load$error_metric #KEEP IT HIDDEN
    error          <- load$error  #KEEP IT HIDDEN
    tolerance      <- load$tolerance
    cpu            <- load$cpu
    max_ram        <- load$max_ram
    min_ram        <- load$min_ram #KEEP IT HIDDEN
    keep_cv <- load$keep_cv
    pkg            <- load$pkg #KEEP IT HIDDEN
  }

  # ============================================================
  # ============================================================
  # Prepare the imputation settings
  # ============================================================
  # ============================================================
  else {
    alg <- algoSelector(algos)
    # preimpute <- "RF" #alg$preimpute ## for now, make this global
    impute <- alg$impute
    postimpute <- alg$postimpute

    synt <- syntaxProcessing(data, preimpute, impute, ram,
                             matching=matching, miniter, maxiter, max_models,
                             tuning_time, cv, weights_column, verbosity=verbosity, report)
    min_ram <- synt$min_ram
    max_ram <- synt$max_ram
    keep_cv <- synt$keep_cross_validation_predictions
    verbose <- synt$verbose
    debug <- synt$debug
  }

  # disable h2o progress_bar
  if (!debug) h2o::h2o.no_progress()

  # Initialize the Markdown report / log
  # ============================================================

  if (is.null(report)) {
    md.log("System information",
           file=tempfile(), trace=TRUE, sys.info = TRUE,
           date=TRUE, time=TRUE) #, print=TRUE
  }
  else {
    md.log("System information",
           file=report, trace=TRUE, sys.info = TRUE,
           date=TRUE, time=TRUE) #, print=TRUE
  }

  # Run H2O on the statistics server¤
  # ============================================================
  if (init) {
    #sink(file = report, append = TRUE)
    #cat("\n") # for Markdown styling
    capture.output(connection <- init(nthreads = cpu,
                                      min_mem_size = min_ram,
                                      max_mem_size = max_ram,
                                      ignore_config = TRUE,
                                      report),
                   file = report,
                   append = TRUE)
    #sink()

    ## ??? DO NOT CLOSE ALL THE CONNECTIONS
    #sink.reset <- function(){
    #  for(i in seq_len(sink.number())){
    #    sink(NULL)
    #  }
    #}
    #sink.reset()


    ##closeAllConnections()
    ##print(connection)
  }

  # Identify variables for imputation and their models' families
  # ============================================================
  if (is.null(load)) {
    VARS <- selectVariables(data, ignore, verbose, report)
    dataNA <- VARS$dataNA
    allPredictors <- VARS$allPredictors
    vars2impute <- VARS$vars2impute
    vars2postimpute <- VARS$vars2impute
    storeVars2impute <- vars2impute
    X <- VARS$X
    bdata <- NULL

    # if there is only one variable to impute, there is no need to iterate!
    if (length(vars2impute) < 1) stop("\nthere is nothing to impute!\n")
    else if (length(vars2impute) == 1) {
      if (!is.valid(postimpute)) {
        maxiter <- 1
      }
    }

    # .........................................................
    # check the variables for compatibility
    # .........................................................
    # if preimputed data is provided, take it into consideration!
    if (!is.null(preimputed.data)) {

      # if a multiple imputation object is given, take the first dataset
      # ??? in the future, consider that each of the given datasets can
      # be fed independently as a separate "m". for now, this is NOT AN
      # announced feature and thus, just take the first dataset as preimputation
      if (inherits(preimputed.data, "mlim.mi")) preimputed.data <- preimputed.data[[1]]
      data <- preimputed.data

      # reset the relevant predictors
      X <- allPredictors
    }

    Features <- checkNconvert(data, vars2impute, ignore,
                              ignore.rank=ignore.rank, report)

    FAMILY<- Features$family
    data  <- Features$data
    mem <- Features$mem
    orderedCols <- Features$orderedCols

    # ??? deactivate "iterate" preimputation, because it's dull!
    # .........................................................
    # PREIMPUTATION
    # .........................................................
    if (preimpute != "iterate" & is.null(preimputed.data)) {
      data <- mlim.preimpute(data=data, preimpute=preimpute,
                             seed = seed, report=report, debug=debug)

      # reset the relevant predictors
      X <- allPredictors
    }
  }

  # ............................................................
  # ............................................................
  # ITERATION LOOP
  # ............................................................
  # ............................................................
  if (is.null(load)) {
    k     <- 1L
    z     <- 1L
    m.it  <- 1L
    MI    <- NULL
    error <- setNames(rep(1, length(vars2impute)), vars2impute)
  }

  for (m.it in m.it:m) {
    dataLast <- iteration_loop(MI, dataNA, data, bdata, boot=m>1,
                               metrics, tolerance, doublecheck,
                               m, k, X, Y, z, m.it,
                               # loop data
                               vars2impute, vars2postimpute, storeVars2impute,
                               allPredictors, preimpute, impute, postimpute,
                               # settings
                               error_metric, FAMILY=FAMILY, cv, tuning_time,
                               max_models, weights_column,
                               keep_cv,
                               autobalance, balance, seed, save, flush,
                               verbose, debug, report, sleep,
                               # saving settings
                               mem, orderedCols, ignore, maxiter,
                               miniter, matching, ignore.rank,
                               verbosity, error, cpu, max_ram=max_ram, min_ram=min_ram,
                               shutdown=FALSE, clean = TRUE)

    if (m > 1) MI[[m.it]] <- dataLast
    else MI <- dataLast
  }

  if (shutdown) {
    md.log("shutting down the server", trace=FALSE)
    h2o::h2o.shutdown(prompt = FALSE)
    Sys.sleep(sleep)
  }

  if (m > 1) class(MI) <- "mlim.mi"
  else class(MI) <- c("mlim", "data.frame")

  return(MI)
}




