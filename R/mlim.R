#' @title Missing data imputation with automated machine learning
#' @description Imputes missing values in data frames containing mixed variable types
#' using automated machine learning (AutoML). The function supports both single and
#' multiple imputation.
#'
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output packageVersion
#' @importFrom tools file_ext
#' @importFrom h2o h2o.init as.h2o h2o.automl h2o.predict h2o.ls
#'             h2o.removeAll h2o.rm h2o.shutdown h2o.no_progress
#' @importFrom md.log md.log
#' @importFrom memuse Sys.meminfo
#' @importFrom stats var setNames na.omit
#' @importFrom curl curl
#' @param data A \code{data.frame} containing missing values to be imputed. If
#'   \code{load} is provided, the data stored in the saved \code{mlim} object are used
#'   instead.
#' @param m Integer specifying the number of imputations. The default is \code{1},
#'   which performs single imputation. Values greater than \code{1} perform multiple
#'   imputation and return \code{m} completed datasets.
#' @param algos Character vector specifying the machine-learning algorithms used for
#'   imputation. Supported names are \code{"ELNET"}, \code{"RF"}, \code{"GBM"},
#'   \code{"DL"}, \code{"XGB"}, and \code{"Ensemble"}. The default is
#'   \code{"ELNET"}. More than one algorithm can be supplied, in which case H2O
#'   AutoML selects the best-performing model from the fitted candidates for each
#'   imputed variable and iteration.
#' @param preimpute Character specifying the initial treatment of missing values before
#'   iterative model-based imputation. The default is \code{"mm"}, which performs
#'   median/mode preimputation. Random sampling from the observed values of each
#'   variable can be requested with \code{"random"}.
#' @param stochastic Logical. If \code{TRUE}, stochastic variation is added after each
#'   accepted variable-specific imputation update. For continuous variables, values
#'   are drawn from a normal distribution centered on the model prediction with the
#'   current cross-validation RMSE as the standard deviation. For categorical
#'   variables, values are sampled from the predicted class probabilities. The
#'   default is \code{FALSE} for single imputation and \code{TRUE} for multiple
#'   imputation.
#'
#' @param ignore Character vector of column names or numeric indices identifying
#'   variables that should be retained in the data but excluded from the imputation
#'   models.
#' @param hierarchy Character vector specifying the clustering variables from the
#'   highest to the lowest level. For example,
#'   \code{hierarchy = c("city", "school", "classroom", "student")} specifies
#'   students nested within classrooms, classrooms nested within schools, and schools
#'   nested within cities. Hierarchy variables must exist in \code{data} and cannot
#'   contain missing values. The default is \code{NULL}, which assumes no
#'   hierarchical structure.
#' @param tuning_time Numeric. Maximum runtime in seconds for AutoML tuning of each
#'   variable in each iteration. The default is \code{900} seconds.
#'   this argument also influences \code{max_models}, see below.
#' @param max_models Integer or \code{NULL}. Maximum number of models that AutoML may
#'   fit for each variable and iteration. If \code{NULL}, no explicit model-count
#'   limit is supplied by \code{mlim}.
#' @param maxiter Integer specifying the maximum number of global imputation
#'   iterations. The default is \code{10}.
#'
#' @param cv Integer specifying the number of cross-validation folds. Values of
#'   at least \code{5} are required; the default is \code{10}.
#' @param matching Either \code{"AUTO"} or \code{FALSE}. With \code{"AUTO"},
#'   integer-like imputed values are matched to the nearest observed value when
#'   appropriate. Set to \code{FALSE} to disable matching. The default is
#'   \code{"AUTO"}.
#' @param autobalance Logical. If \code{TRUE}, H2O automatic class balancing is used
#'   for binomial and multinomial targets during single imputation. During multiple
#'   imputation, target-specific balancing weights are combined with the bootstrap
#'   multiplicity weights for binomial, multinomial, and ordered-factor targets.
#'   The default is \code{TRUE}.
#' @param seed Integer or \code{NULL}. Random-number seed used for reproducibility.
#' @param verbosity Character or \code{NULL}. Controls console output. Supported
#'   values are \code{"warn"}, \code{"info"}, and \code{"debug"}; the default is
#'   \code{NULL}.
#' @param report Character or \code{NULL}. Optional filename for a Markdown progress
#'   report produced during imputation.
#' @param tolerance Numeric. Minimum relative reduction in cross-validation RMSE
#'   required for a newly fitted model to replace the current imputation of a
#'   variable after the first iteration. The default is \code{1e-3}.
#' @param cpu Integer specifying the number of CPU threads available to H2O. The
#'   default, \code{-1}, uses all available threads.
#' @param ram Numeric or \code{NULL}. Maximum memory, in gigabytes, allocated to the
#'   H2O Java server. If \code{NULL}, H2O's default memory allocation is used.
#' @param flush Logical. If \code{TRUE}, H2O objects are removed after each
#'   variable-specific imputation to reduce memory use. This can increase runtime
#'   because the working data must be uploaded again. The default is \code{FALSE}.
#' @param port Object of class numeric representing the port number of the H2O server. The default is 54321.
#' @param preimputed.data Optional preimputed \code{data.frame}. When supplied, it is
#'   used as the starting working dataset and the \code{preimpute} step is bypassed.
#' @param save Character or \code{NULL}. Optional filename with extension
#'   \code{.mlim}. When provided, the current imputation state is saved after each
#'   variable-specific update so the procedure can later be resumed.
#' @param load Character, an object of class \code{mlim}, or \code{NULL}. A saved
#'   imputation state to resume. If a filename is supplied, it is read with
#'   \code{readRDS()}.
#' @param shutdown Logical. If \code{TRUE}, the H2O server is shut down after
#'   imputation. The default is \code{TRUE}.
#' @param java Character or \code{NULL}. Optional path to a 64-bit Java executable,
#'   primarily for systems where Java is installed but is not available on the system
#'   path.
#' @param ... Internal arguments used by \code{mlim}; these are not intended for
#'   regular use.
#' @return For \code{m = 1}, a completed \code{data.frame} with classes
#'   \code{"mlim"} and \code{"data.frame"}. For \code{m > 1}, a list of \code{m}
#'   completed datasets with class \code{"mlim.mi"}.
#' @author E. F. Haghish
#'
#' @examples
#'
#' \dontrun{
#' data(iris)
#'
#' # Add stratified missing observations. To make the example run
#' # faster, add NAs only to a single variable.
#' dfNA <- iris
#' dfNA$Species <- mlim.na(dfNA$Species, p = 0.1, stratify = TRUE, seed = 2022)
#'
#' # Run ELNET single imputation
#' MLIM <- mlim(dfNA, shutdown = FALSE)
#'
#' # Inspect the stored cross-validation error
#' mlim.summarize(MLIM)
#'
#' # Run ELNET multiple imputation with five datasets
#' # Convert the result to a mice::mids object for pooled analysis
#' MLIM2 <- mlim(dfNA, m = 5)
#' mids <- mlim.mids(MLIM2, dfNA)
#' fit <- with(data = mids, exp = lm(Sepal.Length ~ Species + Petal.Length))
#' res <- mice::pool(fit)
#' summary(res)
#'
#' # If the complete data are known, evaluate imputation error
#' mlim.error(MLIM2, dfNA, iris)
#' }
#' @export

mlim <- function(data = NULL,
                 m = 1,
                 algos = c("ELNET"),
                 preimpute = "mm",
                 stochastic = m > 1,
                 ignore = NULL,
                 hierarchy = NULL,

                 # computational resources
                 tuning_time = 900,
                 max_models = NULL,
                 maxiter = 10L,
                 cv = 10L,

                 matching = "AUTO",
                 autobalance = TRUE,

                 # report and reproducibility
                 seed = NULL,
                 verbosity = NULL,
                 report = NULL,

                 # stopping criteria
                 tolerance = 1e-3,

                 # setup the h2o cluster
                 cpu = -1,
                 ram = NULL,
                 flush = FALSE,
                 port = 54321,

                 preimputed.data = NULL,
                 save = NULL,
                 load = NULL,
                 shutdown = TRUE,
                 java = NULL,
                 ...) {

  # ------------------------------------------------------------
  # Internal settings
  # ============================================================
  loading <- !is.null(load)
  stochastic_supplied <- !missing(stochastic)

  hidden_args <- c("superdebug", "init", "ignore.rank", "sleep")
  dots <- list(...)
  dot_names <- names(dots)

  if (length(dots) > 0L &&
      (is.null(dot_names) || any(!nzchar(dot_names)) ||
       !all(dot_names %in% hidden_args))) {
    stop("Unsupported argument supplied through '...'.", call. = FALSE)
  }

  MI           <- NULL
  bdata        <- NULL
  metrics      <- NULL
  error        <- NULL
  debug        <- FALSE
  verbose      <- 0
  error_metric <- "RMSE"

  initialize_h2o <- threeDots(name = "init", ..., default = TRUE)
  ignore.rank <- threeDots(name = "ignore.rank", ..., default = FALSE)
  sleep       <- threeDots(name = "sleep", ..., default = .25)
  superdebug  <- threeDots(name = "superdebug", ..., default = FALSE)


  # ============================================================
  # LOAD SETTINGS FROM A SAVED mlim OBJECT
  # ============================================================
  if (loading) {

    state <- load
    if (inherits(state, "character")) state <- readRDS(state)
    if (!inherits(state, "mlim")) stop("Loaded object must be of class 'mlim'.", call. = FALSE)

    # Data
    # ----------------------------------------------------------
    MI              <- state$MI
    dataNA          <- state$dataNA
    preimputed.data <- state$preimputed.data
    data            <- state$data
    metrics         <- state$metrics
    mem             <- state$mem
    orderedCols     <- state$orderedCols

    # Loop data
    # ----------------------------------------------------------
    m             <- state$m
    m.it          <- state$m.it
    k             <- state$k
    z             <- state$z
    X             <- state$X
    Y             <- state$Y
    vars2impute   <- state$vars2impute
    FAMILY        <- state$FAMILY
    ITERATIONVARS <- state$ITERATIONVARS

    # allPredictors was added to the saved state when stochastic
    # updating was moved inside the chained iteration procedure.
    if (!is.null(state$allPredictors)) {
      allPredictors <- state$allPredictors
    }
    else {
      allPredictors <- colnames(data)[!colnames(data) %in% state$ignore]
    }

    # Settings
    # ----------------------------------------------------------
    impute      <- state$impute
    ignore      <- state$ignore
    autobalance <- state$autobalance
    save        <- state$save
    maxiter     <- state$maxiter
    cv          <- state$cv
    tuning_time <- state$tuning_time
    max_models  <- state$max_models
    matching    <- state$matching
    ignore.rank <- state$ignore.rank
    seed        <- state$seed
    verbosity   <- state$verbosity
    verbose     <- state$verbose
    debug       <- state$debug
    report      <- state$report
    flush       <- state$flush
    error_metric<- state$error_metric
    error       <- state$error
    tolerance   <- state$tolerance
    cpu         <- state$cpu
    max_ram     <- state$max_ram
    min_ram     <- state$min_ram

    if (!is.null(state$preimpute)) {
      preimpute <- state$preimpute
    }

    if (!is.null(state$stochastic)) {
      stochastic <- state$stochastic
    }
    else if (!stochastic_supplied) {
      stochastic <- m > 1
    }

    # Move to the variable following the last saved variable.
    # ----------------------------------------------------------
    moveOn <- iterationNextVar(m, m.it, k, z, ITERATIONVARS, maxiter)
    m.it <- moveOn$m.it
    k    <- moveOn$k
    z    <- moveOn$z
  }


  # ============================================================
  # PREPARE A NEW IMPUTATION
  # ============================================================
  else {

    if (!is.null(seed)) set.seed(seed)

    # Translate user-facing algorithm names to the names expected by H2O.
    # This replaces the old postimputation-aware algorithm selection logic.
    algorithm_map <- c(
      ELNET = "GLM",
      RF = "DRF",
      DL = "DeepLearning",
      GBM = "GBM",
      XGB = "XGBoost",
      Ensemble = "StackedEnsemble"
    )

    valid_algorithms <- c(
      names(algorithm_map),
      unname(algorithm_map)
    )

    if (!is.character(algos) || length(algos) < 1L || anyNA(algos) ||
        any(!algos %in% valid_algorithms)) {
      stop(
        "One or more values supplied to 'algos' are not recognized.",
        call. = FALSE
      )
    }

    impute <- algos
    mapped <- impute %in% names(algorithm_map)
    impute[mapped] <- unname(algorithm_map[impute[mapped]])

    synt <- syntaxProcessing(
      data = data,
      hierarchy = hierarchy,
      preimpute = preimpute,
      impute = impute,
      ram = ram,
      matching = matching,
      maxiter = maxiter,
      max_models = max_models,
      tuning_time = tuning_time,
      cv = cv,
      verbosity = verbosity,
      report = report,
      save = save
    )

    min_ram <- synt$min_ram
    max_ram <- synt$max_ram
    verbose <- synt$verbose
    debug   <- synt$debug

  }


  # ------------------------------------------------------------
  # Disable the H2O progress bar unless explicitly debugging it
  # ============================================================
  if (!superdebug) h2o::h2o.no_progress()


  # ============================================================
  # INITIALIZE THE MARKDOWN REPORT
  # ============================================================
  if (is.null(report)) {
    md.log("System information", file = tempfile(),
           trace = TRUE, sys.info = TRUE, date = TRUE, time = TRUE)
  }
  else if (!loading) {
    md.log("System information", file = report,
           append = FALSE, trace = TRUE, sys.info = TRUE,
           date = TRUE, time = TRUE)
  }
  else {
    md.log("\nContinuing from where it was left...", file = report,
           append = TRUE, trace = TRUE, sys.info = TRUE,
           date = TRUE, time = TRUE)
  }


  # ============================================================
  # INITIALIZE H2O
  # ============================================================
  if (initialize_h2o) {

    # Always begin with a fresh local H2O server on the requested port.
    # If an older H2O cluster is already running there, shut it down
    # and wait until the port is released before starting a new one.
    stopH2o(port = port)
    Sys.sleep(0.5)

    capture.output(
      init(
        nthreads = cpu,
        min_mem_size = min_ram,
        max_mem_size = max_ram,
        ignore_config = TRUE,
        java = java,
        debug = debug,
        port = port
      ),
      file = report,
      append = TRUE
    )
  }


  # ============================================================
  # IDENTIFY VARIABLES AND PREPARE A NEW IMPUTATION
  # ============================================================
  if (!loading) {

    VARS <- selectVariables(data, ignore, verbose, report)

    dataNA       <- VARS$dataNA
    allPredictors<- VARS$allPredictors
    vars2impute  <- VARS$vars2impute
    X            <- VARS$X
    bdata        <- NULL

    if (length(vars2impute) < 1L) {
      stop("There are no missing values to impute.", call. = FALSE)
    }

    # With only one incomplete variable, subsequent chained iterations
    # cannot update its predictor values and therefore add no information.
    if (length(vars2impute) == 1L) maxiter <- 1L


    # ----------------------------------------------------------
    # Handle an externally preimputed dataset
    # ----------------------------------------------------------
    if (!is.null(preimputed.data)) {

      if (inherits(preimputed.data, "mlim.mi")) {
        stop("Multiple-imputation datasets cannot be used as 'preimputed.data'.", call. = FALSE)
      }
      else if (inherits(preimputed.data, "mlim")) {
        metrics <- getMetrics(preimputed.data)
      }

      data <- preimputed.data
      X <- allPredictors
    }


    # ----------------------------------------------------------
    # Check variable types and determine model families
    # ----------------------------------------------------------
    Features <- checkNconvert(
      data, vars2impute, ignore,
      ignore.rank = ignore.rank, report
    )

    data        <- Features$data
    FAMILY      <- Features$family
    mem         <- Features$mem
    orderedCols <- Features$orderedCols


    # ----------------------------------------------------------
    # Preimputation
    # ----------------------------------------------------------
    if (is.null(preimputed.data)) {

      # Single imputation starts from one preimputed working dataset.
      # Multiple imputation preimputes each bootstrap dataset separately
      # inside iteration_loop().
      if (m == 1) {
        data <- mlim.preimpute(
          data = data,
          preimpute = preimpute,
          seed = NULL
        )
      }

      X <- allPredictors
    }


    # For multiple imputation, retain the original converted dataset as
    # the starting dataset for every bootstrap imputation.
    if (m > 1) {
      preimputed.data <- Features$data
    }
    else {
      preimputed.data <- NULL
    }

    rm(Features)
    gc()
  }


  # ============================================================
  # INITIALIZE LOOP STATE FOR A NEW IMPUTATION
  # ============================================================
  if (!loading) {
    k     <- 1L
    z     <- 1L
    m.it  <- 1L
    MI    <- NULL
    error <- setNames(rep(1, length(vars2impute)), vars2impute)
  }


  # ============================================================
  # IMPUTATION LOOP
  # ============================================================
  for (m.it in m.it:m) {

    # Each multiple-imputation dataset starts from the same original
    # converted dataset before drawing a new bootstrap sample.
    if (k == 1L && z == 1L) {
      if (!is.null(preimputed.data)) data <- preimputed.data
      md.log(paste("Dataset", m.it), section = "section")
    }

    bdata <- NULL

    dataLast <- iteration_loop(
      MI, dataNA, preimputed.data, data, bdata, boot = m > 1,
      metrics, tolerance,
      m, k, X, z, m.it,

      # loop data
      vars2impute,
      allPredictors, preimpute, impute,
      hierarchy = hierarchy,

      # settings
      error_metric, FAMILY = FAMILY, cv, tuning_time,
      max_models,
      autobalance,
      seed, save, flush,
      verbose, debug, report, sleep,

      # saving settings
      mem, orderedCols, ignore, maxiter,
      matching, ignore.rank,
      verbosity, error, cpu, max_ram = max_ram, min_ram = min_ram,
      shutdown = FALSE, clean = TRUE,
      stochastic = stochastic
    )

    if (m > 1) {
      MI[[m.it]] <- dataLast
    }
    else {
      MI <- dataLast
    }
  }


  message("\n")

  if (shutdown) {
    md.log("shutting down the server", trace = FALSE)
    h2o::h2o.shutdown(prompt = FALSE)
    Sys.sleep(sleep)
  }

  if (m > 1) {
    class(MI) <- "mlim.mi"
  }
  else {
    class(MI) <- c("mlim", "data.frame")
  }

  return(MI)
}

