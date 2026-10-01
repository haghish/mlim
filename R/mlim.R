#' @title missing data imputation with automated machine learning
#' @description imputes data.frame with mixed variable types using automated
#'              machine learning (AutoML)
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output packageVersion
#' @importFrom tools file_ext
#' @importFrom md.log md.log
#' @importFrom memuse Sys.meminfo
#' @importFrom stats var setNames na.omit
#' @param data a \code{data.frame} (strictly) with missing data to be
#'             imputed. if \code{'load'} argument is provided, this argument will be ignored.
#' @param m integer, specifying number of multiple imputations. the default value is
#'          1, carrying out a single imputation.
#' @param algos character vector specifying the machine-learning algorithms used
#'   for imputation. Supported algorithms are \code{"ELNET"} (elastic net via
#'   \code{glmnet}), \code{"RF"} (random forest via \code{ranger}),
#'   \code{"CRF"} (conditional random forest via \code{partykit::cforest}),
#'   \code{"GBM"} (classical gradient boosting via \code{gbm}),
#'   \code{"XGB"} (XGBoost), \code{"LGBM"} (LightGBM), \code{"CAT"}
#'   (CatBoost), \code{"NNET"} (single-hidden-layer neural network via
#'   \code{nnet}), \code{"SVM"} (kernel support vector machine via
#'   \code{kernlab::ksvm}), \code{"KNN"} (k-nearest neighbors), and
#'   \code{"ENSEMBLE"}.
#'   The default is \code{"ELNET"}.
#'
#'   When several base algorithms are supplied, \code{max_models} and
#'   \code{tuning_time} are divided across the base algorithms and the
#'   best-performing candidate is used. If \code{"ENSEMBLE"} is included,
#'   at least two additional base algorithms must also be supplied. The ensemble
#'   is a stacked model constructed with \code{mlr3pipelines} using
#'   cross-validated predictions from the successfully tuned base learners and
#'   is evaluated as an additional candidate after base-learner tuning.
#'
#'   The current \code{mlr3extralearners} \code{classif.gbm} wrapper supports
#'   two-class classification but not multiclass classification; therefore
#'   \code{"GBM"} is skipped for multinomial targets when other learners are
#'   available.
#'
#'   \code{"KNN"} does not support observation weights in its current mlr3
#'   learner. It can therefore be evaluated in single imputation, but is skipped
#'   during multiple imputation because bootstrap multiplicity weights are
#'   required for model fitting. When class balancing is requested in single
#'   imputation, KNN is fitted without learner weights, although balancing
#'   weights are retained for performance assessment.
#' @param preimpute Character specifying the initial treatment of missing values before
#'   iterative model-based imputation. The default is \code{"random"}, which performs
#'   random sampling from each feature. The alternative is \code{"mm"},
#'   which performs median/mode preimputation.
#                   feature is currently experimental, prone to over-fitting, and highly computationally extensive.
#' @param stochastic Logical. If \code{TRUE}, stochastic variation is added after each
#'   accepted variable-specific imputation update. For continuous variables, values
#'   are drawn from a normal distribution centered on the model prediction with the
#'   current cross-validation RMSE as the standard deviation. For categorical
#'   variables, values are sampled from the predicted class probabilities. The
#'   default is \code{FALSE} for single imputation and \code{TRUE} for multiple
#'   imputation.
#' @param ignore character vector of column names or index of columns that should
#'               should be ignored in the process of imputation.
#' @param hierarchy Character vector specifying the clustering variables from the
#'   highest to the lowest level. For example,
#'   \code{hierarchy = c("city", "school", "classroom", "student")} specifies
#'   students nested within classrooms, classrooms nested within schools, and schools
#'   nested within cities. Hierarchy variables must exist in \code{data} and cannot
#'   contain missing values. The default is \code{NULL}, which assumes no
#'   hierarchical structure.
#' @param tuning_time Numeric. Maximum base-learner tuning runtime in seconds for
#'   each variable and iteration. The default is \code{3600}. When several base
#'   algorithms are selected, this budget is divided across them. Tuning stops
#'   when the applicable time or evaluation limit is reached. A requested stacked
#'   ensemble is evaluated after base-learner tuning and does not consume this
#'   base-learner tuning-time allocation.
#' @param max_models Integer or \code{NULL}. Maximum number of hyperparameter
#'   evaluations across the base algorithms for each variable and iteration.
#'   The default is \code{100}. When several base algorithms are selected, this
#'   budget is divided across them. If \code{NULL}, no explicit evaluation-count
#'   limit is supplied by \code{mlim}. \code{"ENSEMBLE"} does not count as a base
#'   algorithm for this allocation.
#' @param autobalance logical. if TRUE (default), binary and multinomial factor variables
#'                    are balanced during single imputation. During multiple imputation,
#'                    balancing weights are combined with bootstrap multiplicity weights.
#                    if FALSE, imputation fairness will be sacrificed for overall accuracy, which
#                    is not recommended, although it is commonly practiced in other missing data
#                    imputation software. MLIM is highly concerned with imputation fairness for
#                    factor variables and autobalancing is generally recommended.
#                    in fact, higher overall accuracy does not mean a better imputation as
#                    long as minority classes are neglected, which increases the bias in favor of the
#                    majority class. if you do not wish to autobalance all the
#                    factor variables, you can manually specify the variables
#                    that should be balanced using the 'balance' argument (see below).
#
#                    NOTE: during multiple imputation, target-specific balancing weights are
#                    combined with bootstrap multiplicity weights.
# @param balance character vector, specifying variable names that should be
#                balanced before imputation. balancing the prevalence might
#                decrease the overall accuracy of the imputation, because it
#                attempts to ensure the representation of the rare outcome.
#                this argument is optional and intended for advanced users that
#                impute a severely imbalance categorical (nominal) variable.
#' @param matching Logical. If \code{TRUE}, post-processing is applied to
#'   imputed values for integer-valued numeric variables when \code{stochastic}
#'   is also \code{TRUE}. Fractional predictions are stochastically matched
#'   between the two bounding observed values. The probability of selecting
#'   each value is based on its distance from the prediction, so that the
#'   expected matched value equals the original prediction. If the prediction
#'   falls outside the observed range, the nearest boundary value is used. If
#'   there is a gap in the observed values, the nearest lower and upper observed
#'   values are used. Matching is applied after stochastic variation has been
#'   added to the numeric prediction. Set to \code{FALSE} to disable numeric
#'   matching. For categorical variables, stochastic matching to the observed
#'   categories is handled by \code{stochastic}; see the \code{stochastic}
#'   argument for details.
#' @param maxiter integer. maximum number of iterations. the default value is \code{15},
#'        but it can be reduced to \code{3} (not recommended, see below).
#' @param cv Integer specifying the number of cross-validation folds. Values of
#'   \code{5} or higher are required. the default is \code{5}.
#' @param tolerance numeric. the minimum rate of improvement in estimated error metric
#'                  of a variable to qualify the imputation for another round of iteration,
#'                  if the \code{maxiter} is not yet reached. any improvement of imputation
#'                  is desirable.  however, specifying values above 0 can reduce the number
#'                  of required iterations at a marginal increase of imputation error.
#'                  for larger datasets, value of "1e-3" is recommended to reduce number
#'                  of iterations. the default value is '1e-3'.
#'
#' @param seed integer. specify the random generator seed

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
#' @param save filename (with .mlim extension). if a filename is specified, an \code{mlim} object is
#'             saved after the end of each variable imputation. this object not only
#'             includes the imputed dataframe and estimated cross-validation error, but also
#'             includes the information needed for continuing the imputation,
#'             which is very useful feature for imputing large datasets, with a
#'             long runtime. this argument is activated by default and an
#'             mlim object is stored in the local directory named \code{"mlim.rds"}.
#' @param load filename (with .mlim extension). an object of class "mlim", which includes the data, arguments,
#'                 and settings for re-running the imputation, from where it was
#'                 previously stopped. the "mlim" object saves the current state of
#'                 the imputation and is particularly recommended for large datasets
#'                 or when the user specifies a computationally extensive settings
#'                 (e.g. specifying several algorithms, increasing tuning time, etc.).
#' @param cpu Integer specifying the number of CPU threads supplied to learners that support internal multithreading. The default is \code{1}.
#' @param ... arguments that are used internally between 'mlim'
#'            these arguments are not documented in the help file and are not
#'            intended to be used by end user.
#
# ARGUMENTS FOR ..., WHICH ARE MOSTLY EXPERIMENTAL
# 1. preimputed.data data.frame. if you have used another software for missing data imputation, you can still
#    optimize the imputation by handing the data.frame to this argument, which will bypass the "preimpute" procedure.
# 2. ignore.rank logical, if FALSE (default), ordinal variables
#                    are imputed as continuous integers with regression plus matching
#                    and are reverted to ordinal later again. this procedure is
#                    recommended. if FALSE, the rank of the categories will be ignored
#                    the the algorithm will try to optimize for classification accuracy.
#                    WARNING: the latter often results in very high classification accuracy but at
#                    the cost of higher rank error. see the "mlim.error" function
#                    documentation to see how rank error is computed. therefore, if you
#                    intend to carry out analysis on the rank data as numeric, it is
#                    recommended that you set this argument to FALSE.
# @param stopping_metric character.
# @param stopping_rounds integer.
# @param stopping_tolerance numeric.
# @param weights_column non-negative integer. a vector of observation weights
#                       can be provided, which should be of the same length
#                       as the dataframe. giving an observation a weight of
#                       Zero is equivalent of ignoring that observation in the
#                       model. in contrast, a weight of 2 is equivalent of
#                       repeating that observation twice in the dataframe.
#                       the higher the weight, the more important an observation
#                       becomes in the modeling process. the default is NULL.
# @param error_metric character. specify the minimum improvement
#                                  in the estimated error to proceed to the
#                                  following iteration or stop the imputation.
#                                  the default is 10^-4 for \code{"MAE"}
#                                  (Mean Absolute Error). this criteria is only
#                                  applied from the end of the fourth iteration.
#                                  \code{"RMSE"} (Root Mean Square
#                                  Error). other possible values are \code{"MSE"},
#                                  \code{"MAE"}, \code{"RMSLE"}.
#' @return a \code{data.frame}, showing the
#'         estimated imputation error from the cross validation within the data.frame's
#'         attribution
#' @author E. F. Haghish
#'
#' @examples
#'
#' \dontrun{
#' data(iris)
#'
#' # add stratified missing observations to the data. to make the example run
#' # faster, I add NAs only to a single variable.
#' dfNA <- iris
#' dfNA$Species <- mlim.na(dfNA$Species, p = 0.1, stratify = TRUE, seed = 2022)
#'
#' # run the ELNET single imputation (fastest imputation via 'mlim')
#' MLIM <- mlim(dfNA)
#'
#' # in single imputation, you can estimate the imputation accuracy via cross validation RMSE
#' mlim.summarize(MLIM)
#'
#' ### or if you want to carry out ELNET multiple imputation with 5 datasets.
#' ### next, to carry out analysis on the multiple imputation, use the 'mlim.mids' function
#' ### minimum of 5 datasets
#' MLIM2 <- mlim(dfNA, m = 5)
#' mids <- mlim.mids(MLIM2, dfNA)
#' fit <- with(data=mids, exp=glm(Species ~ Sepal.Length, family = "binomial"))
#' res <- mice::pool(fit)
#' summary(res)
#'
#' # you can check the accuracy of the imputation, if you have the original dataset
#' mlim.error(MLIM2, dfNA, iris)
#
# ### compare several learners and a stacked ensemble
# MLIM <- mlim(dfNA, algos = c("ELNET", "RF", "XGB", "ENSEMBLE"), tuning_time=60*60)
# mlim.error(MLIM, dfNA, iris)
#
# ### if you have a larger data, there is a few things you can set to make the
# ### algorithm faster, yet, having only a marginal accuracy reduction as a trade-off
# MLIM <- mlim(dfNA, algos = 'ELNET', tolerance = 1e-3)
#' }
#' @export


mlim <- function(data = NULL,
                 m = 1,
                 algos = c("ELNET", "LGBM"),
                 preimpute = "random",

                 ignore = NULL,
                 hierarchy = NULL,

                 # stopping criteria
                 tolerance = 1e-3,
                 #error_metric  = "RMSE", #??? mormalize it
                 #stopping_metric = "AUTO",
                 #stopping_rounds = 3,
                 #stopping_tolerance=1e-3,

                 # computational resources
                 tuning_time = 300,
                 max_models = 50, #
                 maxiter = 15L,
                 cv = 5L,
                 cpu = 1,

                 # fairness
                 stochastic = m > 1,
                 matching = TRUE,
                 autobalance = TRUE,

                 # report and reproducibility
                 seed = NULL,
                 verbosity = NULL,
                 report = NULL,

                 save = NULL,
                 load = NULL,
                 ...
) {

  # improvements for the next release
  # ============================================================
  # instead of using all the algorithms at each iteration, add the
  #    other algorithms when the first algorithm stops being useful.
  #    perhaps this will help optimizing, while reducing the computation burdon

  # check the ... arguments
  # ============================================================
  hidden_args <- c("superdebug", "ignore.rank", "sleep", "debug", "preimputed.data")
  stopifnot("incompatible '...' arguments" = (names(list(...)) %in% hidden_args))

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
  running     <- TRUE
  debug       <- threeDots(name = "debug", ..., default = FALSE)
  miniter     <- 2L #this is the minimum number of iterations
  verbose     <- 0
  error_metric<- "RMSE"
  ignore.rank <- threeDots(name = "ignore.rank", ..., default = FALSE)  #EXPERIMENTAL
  sleep       <- threeDots(name = "sleep", ..., default = .25)
  superdebug  <- threeDots(name = "superdebug", ..., default = FALSE)
  preimputed.data  <- threeDots(name = "preimputed.data", ..., default = NULL)
  #stochastic  <- threeDots(name = "stochastic", ..., default = FALSE)



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
    MI             <- load$MI           # dataLast or multiple-imputation data
    dataNA         <- load$dataNA
    preimputed.data<- load$preimputed.data
    data           <- load$data         # preimputed dataset that is constantly updated
    #bdata          <- load$bdata
    #dataLast       <- load$dataLast
    metrics        <- load$metrics
    mem            <- load$mem
    orderedCols    <- load$orderedCols

    # Loop data
    # ----------------------------------
    m              <- load$m            # number of datasets to impute
    m.it           <- load$m.it         # current dataset to impute
    k              <- load$k            # current loop number (global imputation iteration)
    z              <- load$z            # current local iteration number
    X              <- load$X
    Y              <- load$Y            # last-imputed imputed variable. outside the 'load' argument, it means current variable to be imputed
    vars2impute    <- load$vars2impute
    FAMILY         <- load$FAMILY

    if (!is.null(load$allPredictors)) {
      allPredictors <- load$allPredictors
    }
    else {
      allPredictors <- colnames(data)[!colnames(data) %in% load$ignore]
    }

    # settings
    # ----------------------------------
    ITERATIONVARS  <- load$ITERATIONVARS# variables to be imputed
    impute         <- toupper(load$impute) # reimputation algorithm(s)
    autobalance    <- load$autobalance #EXPERIMENTAL
    if ("preimpute" %in% names(load)) preimpute <- load$preimpute
    if ("hierarchy" %in% names(load)) hierarchy <- load$hierarchy
    if ("stochastic" %in% names(load)) stochastic <- load$stochastic
    #balance        <- load$balance #EXPERIMENTAL
    ignore         <- load$ignore
    save           <- load$save
    maxiter        <- load$maxiter
    miniter        <- load$miniter
    cv             <- load$cv
    tuning_time    <- load$tuning_time
    max_models     <- load$max_models
    matching       <- load$matching
    ignore.rank    <- load$ignore.rank #KEEP IT HIDDEN
    #weights_column <- load$weights_column
    seed           <- load$seed
    verbosity      <- load$verbosity
    verbose        <- load$verbose #KEEP IT HIDDEN
    debug          <- load$debug   #KEEP IT HIDDEN
    report         <- load$report
    error_metric   <- load$error_metric #KEEP IT HIDDEN
    error          <- load$error  #KEEP IT HIDDEN
    tolerance      <- load$tolerance
    cpu            <- load$cpu
    if ("sleep" %in% names(load)) sleep <- load$sleep
    pkg            <- load$pkg #KEEP IT HIDDEN


    # MOVE-ON to the next variable after loading an mlim object
    # ---------------------------------------------------------
    if (z == length(ITERATIONVARS)) {
      SC <- stoppingCriteria(method="varwise_NA", miniter, maxiter,
                             metrics, k, vars2impute,
                             error_metric,
                             tolerance,
                             md.log = report)
      running <- SC$running
      error <- SC$error
    }

    if (running) {
      moveOn <- iterationNextVar(m, m.it, k, z, Y, ITERATIONVARS, maxiter)
      m    <- moveOn$m
      m.it <- moveOn$m.it
      k    <- moveOn$k
      z    <- moveOn$z
      Y    <- moveOn$Y
    }
  }

  # ============================================================
  # ============================================================
  # Prepare the imputation settings
  # ============================================================
  # ============================================================
  else {
    if (!is.null(seed)) set.seed(seed) # avoid setting seed by default if it is a continuation

    impute <- unique(toupper(algos))

    synt <- syntaxProcessing(data, hierarchy, preimpute, impute,
                             matching=matching, maxiter, max_models,
                             tuning_time, cv, cpu,
                             verbosity=verbosity, report, save)
    verbose <- synt$verbose
    debug <- synt$debug
  }


  # ============================================================
  # Initialize the Markdown report
  # ============================================================
  if (is.null(report)) md.log("System information", file=tempfile(),
                              trace=TRUE, sys.info = TRUE, date=TRUE, time=TRUE)

  else if (is.null(load)) md.log("System information", file=report,
                                 append = FALSE, trace=TRUE, sys.info = TRUE,
                                 date=TRUE, time=TRUE) #, print=TRUE

  else if (!is.null(load)) md.log("\nContinuing from where it was left...", file=report,
                                  append = TRUE, trace=TRUE, sys.info = TRUE,
                                  date=TRUE, time=TRUE)

  # Identify variables for imputation and their models' families
  # ============================================================
  if (is.null(load)) {
    VARS <- selectVariables(data, ignore, verbose, report)

    dataNA <- VARS$dataNA # the missing data placeholder
    allPredictors <- VARS$allPredictors
    vars2impute <- VARS$vars2impute
    X <- VARS$X
    bdata <- NULL

    # if there is only one variable to impute, there is no need to iterate!
    if (length(vars2impute) < 1) stop("\nthere is nothing to impute!\n")
    else if (length(vars2impute) == 1) {
      maxiter <- 1
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
      if (inherits(preimputed.data, "mlim.mi")) {
        #preimputed.data <- preimputed.data[[1]]
        stop("multiple imputation datasets cannot be used as 'preimputed.data'\n")
      }

      # if the preimputation was done with mlim, extract the metrics
      else if (inherits(preimputed.data, "mlim")) {


        # remove the NAs of the last imputation and replace them with
        # the minimum
        metrics <- getMetrics(preimputed.data)
      }

      # SAVE RAM: if preimputed.data is given, replace the original data because
      # its missing data is reserved within dataNA
      data <- preimputed.data

      # reset the relevant predictors
      X <- allPredictors
    }

    Features <- checkNconvert(data, vars2impute, ignore,
                              ignore.rank=ignore.rank, report)

    FAMILY<- Features$family

    # data  <- Features$data ##> this will be moved inside the loop because
    #                            in multiple imputation, we want to start over
    #                            everytime!
    mem <- Features$mem
    orderedCols <- Features$orderedCols

    # .........................................................
    # PREIMPUTATION: replace data with preimputed data
    # .........................................................
    if (preimpute != "iterate" & is.null(preimputed.data)) {

      # preimpute in single imputation ONLY. for multiple imputation, each
      # bootstrap dataset is imputed seperately
      if (m == 1) {
        data <- mlim.preimpute(data=data, preimpute=preimpute, seed = NULL) # DO NOT RESET THE SEED!
      }

      # reset the relevant predictors
      X <- allPredictors
    }

    # .........................................................
    # Remove 'Features', but keep 'preimputed.data' in MI
    # .........................................................
    if (m > 1) preimputed.data <- Features$data
    else preimputed.data <- NULL
    rm(Features)
    gc()
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

  # drop 'load' from the memory
  # ---------------------------
  rm(load)
  gc()
  load <- NULL

  # ??? bdata must be NULL at the beginning of each itteration. Currently
  # this is NOT happenning when the 'mlim' object is loaded

  for (m.it in m.it:m) {

    # Start the new imputation data fresh, if it is multiple imputation
    if (k == 1 & z == 1) {
      if (!is.null(preimputed.data)) data  <- preimputed.data
      md.log(paste("Dataset", m.it), section="section")
    }

    #it is always NULL. It doesn't have to be saved
    bdata <- NULL
    dataLast <- iteration_loop(MI, dataNA, preimputed.data, data, bdata, boot=m>1,
                               metrics, tolerance,
                               m, k, X, Y, z, m.it,
                               # loop data
                               vars2impute,
                               allPredictors, preimpute, impute,
                               hierarchy = hierarchy,
                               # settings
                               error_metric, FAMILY=FAMILY, cv, tuning_time,
                               max_models,
                               autobalance, #balance,
                               seed, save,
                               verbose, debug, report, sleep,
                               # saving settings
                               mem, orderedCols, ignore, maxiter,
                               miniter, matching, ignore.rank,
                               verbosity, error, cpu, clean = TRUE,
                               stochastic=stochastic,
                               running=running)

    if (m > 1) MI[[m.it]] <- dataLast
    else MI <- dataLast

    if (!running) {
      k <- 1L
      z <- 1L
      running <- TRUE
    }
  }

  message("\n")



  if (m > 1) class(MI) <- "mlim.mi"
  else class(MI) <- c("mlim", "data.frame")

  return(MI)
}
