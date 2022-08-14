#' @title missing data imputation with automated machine learning
#' @description imputes data.frame with mixed variable types using automated
#'              machine learning (AutoML)
#'
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output packageVersion
#' @importFrom h2o h2o.init as.h2o h2o.automl h2o.predict h2o.ls
#'             h2o.removeAll h2o.rm h2o.shutdown
#' @importFrom md.log md.log
#' @importFrom memuse Sys.meminfo
#' @importFrom stats var setNames na.omit
#' @param data a \code{data.frame} or \code{matrix} with missing data to be
#'             imputed. if \code{load} is provided, this argument will be ignored.
#' @param m integer, specifying number of multiple imputations. the default value is
#'          1, carrying out a single imputation.
#' @param algos character vector, specifying algorithms to be used for missing data
#'              imputation. the default is 'c("RF", "ELNET", "GBM")', which uses
#'              Random Forest for a fast initial imputation and then uses ELNET to
#'              improve the imputation and once ELNET stops improving, attempts using
#'              "GBM", as long as the 'maxiter' argument is not reached. in other words,
#'              "mlim" carries out 3 rounds of imputation, which are 1) preimputation with "RF",
#'              2) imputation with "ELNET", and 3) postimputation with "GBM". the reason for
#'              this setup is that in general, "RF" is faster than a fine-tuned "ELNET" and
#'              "ELNET" fine-tunes much faster than "GBM".
#'
#'              in addition to these algorithms, \code{"DL"} (Deep Learning) and \code{"XGB"}
#'              (Extreme Gradient Boosting, only available in Mac OS and Linux) are also
#'              supported.  "GBM", "DL", "XGB", and "Ensemble" take the full given "tuning_time" (see below) to
#'        tune the best model for imputing he given variable.
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
#' @param preimputed.data data.frame. if you have used another software for missing
#'                      data imputation, you can still optimize the imputation
#'                      by handing the data.frame to this argument, which will
#'                      bypass the "preimpute" procedure.
#' @param init logical. should h2o Java server be initiated? the default is TRUE.
#'             however, if the Java server is already running, set this argument
#'             to FALSE.
#' @param cpu integer. number of CPUs to be dedicated for the imputation.
#'                 the default takes all of the available CPUs.
#' @param ram integer. specifies the maximum size, in Gigabytes, of the
#'                     memory allocation. by default, all the available memory is
#'                     used for the imputation.
#'                     large memory size is particularly advised, especially
#'                     for multicore processes. the more you give the more you get!
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
#' @param miniter integer. minimum number of iterations. the default value is
#'                2.
#' @param flush logical (experimental). if TRUE, after each model, the server is
#'              cleaned to retrieve RAM. this feature is in testing mode.
#' @param cv logical. specify number of k-fold Cross-Validation (CV). values of
#'               10 or higher are recommended. default is 10.
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
#'                  for larger datasets, value of "1e-3" is recommended. note that the
#'                  best accuracy is reached when this value is equal to zero.
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
#' @param ... Arguments passed to \code{h2o.automl()}.
#' The following arguments are e.g. incompatible with \code{ranger}: \code{write.forest}, \code{probability}, \code{split.select.weights}, \code{dependent.variable.name}, and \code{classification}.
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
#' @param shutdown logical. if TRUE, h2o server is closed after the imputation.
#'                 the default is TRUE
#' @param sleep integer. number of seconds to wait after each interaction with h2o
#'              server. the default is 1 second. larger values might be needed
#'              depending on your computation power or dataset size.
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
#' @param force.load (NOT YET IMPLEMENTED FOR R).logical (default is TRUE). if TRUE, when loading the mlim class
#'                 object, its preserved settings are used for restoring and saving the
#'                 following itterations. otherwise, if FALSE, the current arguments of
#'                 mlim are used to overpower the settings of the mlim object. the settings
#'                 include the full list of the mlim arguments.
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
#' mlim.error(MLIM, irisNA, iris)
#'
#' # run GBM model and allow 15 minutes of tuning for each variable
#' MLIM <- mlim(irisNA, impute = "GBM", tuning_time=60*15)
#' mlim.error(MLIM, irisNA, iris)
#' }
#' @export


mlim <- function(data = NULL,
                 m = 1,
                 algos = c("RF", "ELNET", "GBM"), #preimpute, impute, postimpute
                 preimputed.data = NULL,
                 ignore = NULL,

                 # computational resources
                 tuning_time = 180,
                 max_models = NULL, # run all that you can
                 maxiter = 10L,
                 miniter = 2L,
                 cv = 10L,
                 #validation = 0,

                 matching = "AUTO",    #EXPERIMENTAL
                 balance = NULL,       #EXPERIMENTAL
                 #ignore.rank = FALSE, #EXPERIMENTAL
                 weights_column = NULL,

                 # general setup
                 seed = NULL,
                 verbosity = NULL,
                 report = NULL,

                 # stopping criteria
                 tolerance = 0, #1e-3
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
                 flush = FALSE,
                 init = TRUE,
                 shutdown = TRUE,
                 sleep = .5,

                 # NOT YET IMPLEMENTED
                 save = NULL,
                 load = NULL,
                 force.load = TRUE,
                 ...
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
  MI      <- list()
  metrics <- NULL
  error   <- NULL
  debug   <- FALSE
  verbose <- 0
  error_metric  <- "RMSE"
  ignore.rank <- FALSE #EXPERIMENTAL
  set.seed(seed)

  preimpute <- algos[1]
  impute <- algos[2]
  postimpute <- algos[3:length(algos)]

  if ("XGB" %in% algos & Sys.info()["sysname"] == "Windows") {
    stop("XGB is not available in Windows")
  }

  #if (is.null(postimpute)) {
  #  if (impute == "AUTO") postimpute <- "DRF"
  #  # ...
  #}
  #else if (length(postimpute) == 1 & postimpute[1] == "AUTO") {
  #  #1. check the OS
  #  #2. make sure GBM has at least 200 non-missing rows
  #  #3. for smaller datasets, prefer XGB, if OS allows
  #  OS <- Sys.info()["sysname"]
  #  if (OS == "Windows") {

  #  }
  #}

  # feature request:
  # ??? add arguments in syntaxProcessing to make sure load is well-prepared

  # ============================================================
  # ============================================================
  # LOAD SETTINGS FROM mlim class object
  # ============================================================
  # ============================================================
  if (!is.null(load)) {
    if (class(load) != "mlim") stop("'load' must be of class 'mlim'")

    # Data
    # ----------------------------------
    data        <- load$data
    dataLast    <- load$dataLast
    metrics     <- load$metrics
    mem         <- load$mem
    orderedCols <- load$orderedCols

    # Loop data
    # ----------------------------------
    k           <- load$k
    z           <- load$z
    X           <- load$X
    Y           <- load$Y
    vars2impute <- load$vars2impute
    FAMILY      <- load$FAMILY

    # settings
    # ----------------------------------
    if (force.load) {
      impute         <- load$impute
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
      pkg            <- load$pkg #KEEP IT HIDDEN
    }
    else {
      cat("The following settings are ignored from the loaded 'mlim' object:\n\nn")
      cat("'impute', 'ignore', 'save', 'maxiter', 'miniter', 'cv', \n")
      cat("'tuning_time', 'max_models', 'matching', 'weights_column', \n")
      cat("'seed', 'verbosity', 'report', 'flush', 'tolerance', 'cpu', 'ram'\n")

      synt <- syntaxProcessing(data, preimpute, impute, ram,
                               matching=matching, miniter, maxiter, max_models,
                               tuning_time, cv, weights_column,
                               verbosity, report)

      min_ram <- synt$min_ram
      max_ram <- synt$max_ram
      keep_cross_validation_predictions <- synt$keep_cross_validation_predictions
      impute <- synt$impute
      verbose <- synt$verbose
      debug <- synt$debug
    }
  }

  # ============================================================
  # ============================================================
  # Prepare the imputation settings
  # ============================================================
  # ============================================================
  else {
    synt <- syntaxProcessing(data, preimpute, impute, ram,
                             matching=matching, miniter, maxiter, max_models,
                             tuning_time, cv, weights_column, verbosity=verbosity, report)

    min_ram <- synt$min_ram
    max_ram <- synt$max_ram
    keep_cross_validation_predictions <- synt$keep_cross_validation_predictions
    impute <- synt$impute
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
  VARS <- selectVariables(data, ignore, verbose, report)
  dataNA <- VARS$dataNA
  allPredictors <- VARS$allPredictors
  vars2impute <- VARS$vars2impute
  vars2postimpute <- VARS$vars2impute
  storeVars2impute <- vars2impute
  X <- VARS$X

  # if there is only one variable to impute, there is no need to iterate!
  if (length(vars2impute) == 1) maxiter <- 1

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
  else if (!is.null(preimputed.data)) data <- preimputed.data

  # ............................................................
  # ............................................................
  # ITERATION LOOP
  # ............................................................
  # ............................................................
  if (is.null(load)) {
    k <- 0L
    error <- setNames(rep(1, length(vars2impute)), vars2impute)
  }

  for (MIit in 1:m) {
    dataLast <- iteration_loop(MIit, dataNA, data, boot=MIit>1, #bootstrap if MIit is mroe than 1
                               metrics, tolerance, doublecheck,
                               k, X, Y, z,
                               # loop data
                               vars2impute, vars2postimpute, storeVars2impute,
                               allPredictors, preimpute, impute, postimpute,
                               # settings
                               error_metric, FAMILY=FAMILY, cv, tuning_time,
                               max_models, weights_column,
                               keep_cross_validation_predictions,
                               balance, seed, save, flush,
                               verbose, debug, report, sleep,
                               # saving settings
                               mem, orderedCols, ignore, maxiter,
                               miniter, matching, ignore.rank,
                               verbosity, error, cpu, max_ram=max_ram, min_ram=min_ram,
                               shutdown=FALSE, clean = TRUE)

    if (m > 1) MI[[MIit]] <- dataLast
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




