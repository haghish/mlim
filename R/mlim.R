#' @title missing data imputation with automated machine learning
#' @description imputes data.frame with mixed variable types using automated
#'              machine learning (AutoML)
# @param impute character. specify a vector of algorithms to be used
#        in the process of auto-tuning. the supported main algorithms are
#        "ELNET", "RF",
#        "GBM", "DL", "XGB" (available for Mac and Linux), and "Ensemble".
#
#        the default is "AUTO", which is mostly based on "ELNET", but also
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
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output packageVersion
#' @importFrom tools file_ext
#' @importFrom h2o h2o.init as.h2o h2o.automl h2o.predict h2o.ls
#'             h2o.removeAll h2o.rm h2o.shutdown h2o.no_progress
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
#'              the default is "ELNET", which fine-tunes an Elastic Net model.
#'              In general, "ELNET" is expected to
#'              be the best algorithm because it fine-tunes very fast, it is
#'              very robust to over-fitting, and hence, it generalizes very well.
#'              However, if your data has many factor variables, each with several
#'              levels, it is recommended to have c("ELNET", "RF") as your imputation
#'              algorithms (and possibly add "Ensemble" as well, to make the most out
#'              of tuning the models).
#'
#'              Note that "XGB" is only available in Mac OS and Linux. moreover,
#'              "GBM", "DL" and "XGB" take the full given "tuning_time" (see below) to
#'              tune the best model for imputing he given variable, whereas "ELNET"
#'              will produce only one fine-tuned model, often at less time than
#'              other algorithms need for developing a single model, which is why "ELNET"
#'              is work horse of the mlim imputation package.
#' @param preimpute character. specifies the 'primary' procedure of handling the missing
#'                  data. before 'mlim' begins imputing the missing observations, they should
#'                  be prepared for the imputation algorithms and thus, they should be replaced
#'                  with some values.
#'                  the default procedure is a quick "RF", which models the missing
#'                  data with parallel Random Forest model. this is a very fast procedure,
#'                  which later on, will be replaced within the "reimputation" procedure (see below).
#'                  possible other alternative is \code{"mm"},
#'                  which carries out mean/mode replacement, as practiced by most imputation algorithms.
#'                  "mm" is much faster than "RF". if your dataset is very
#'                  large, consider pre-imputing it before hand using 'mlim.preimpute()'
#'                  function and passing the preimputed dataset to mlim (see "preimputed.data" argument).
#
#                  another alternative is "iterate", which instead of filling the missing observations with mean and mode, it
#                  gradually adds the imputed variables to the vector of predictors, as it carries out the
#                  first iteration.
#' @param postimpute (EXPERIMENTAL FEATURE) logical. if TRUE, mlim uses algorithms rather than 'ELNET' for carrying out
#'                   postimputation optimization. however, if FALSE, all specified algorihms will
#'                   be used in the process of 'reimputation' together. the 'Ensemble' algorithm
#'                   is encouraged when other algorithms are used. However, for general users
#'                   unspecialized in machine learning, postimpute is NOT recommended because this
#'                   feature is currently experimental, prone to over-fitting, and highly computationally extensive.
#' @param stochastic logical. by default it is set to TRUE for multiple imputation and FALSE for
#'                   single imputation. stochastic argument is currently under testing and is intended to
#'                   avoid inflating the correlation between imputed valuables.
#' @param ignore character vector of column names or index of columns that should
#'               should be ignored in the process of imputation.
#' @param tuning_time integer. maximum runtime (in seconds) for fine-tuning the
#'                               imputation model for each variable in each iteration. the default
#'                               time is 900 seconds but for a large dataset, you
#'                               might need to provide a larger model development
#'                               time. this argument also influences \code{max_models},
#'                               see below. If you are using 'ELNET' algorithm (default),
#'                               you can be generous with the 'tuning_time' argument because
#'                               'ELNET' tunes much faster than the rest and will only
#'                               produce one model.
#' @param max_models integer. maximum number of models that can be generated in
#'                   the proecess of fine-tuning the parameters. this value
#'                   default to 100, meaning that for imputing each variable in
#'                   each iteration, up to 100 models can be fine-tuned. increasing
#'                   this value should be consistent with increasing
#'                   \code{max_model_runtime_secs}, allowing the model to spend
#'                   more time in the process of individualized fine-tuning.
#'                   as a result, the better tuned the model, the more accurate
#'                   the imputed values are expected to be
#' @param autobalance logical. if TRUE (default), binary and multinomial factor variables
#'                    will be balanced before the imputation to obtain fairer
#'                    and less-biased imputations, which are typically in favor
#'                    of the majority class.
#'                    if FALSE, imputation fairness will be sacrificed for overall accuracy, which
#'                    is not recommended, although it is commonly practiced in other missing data
#'                    imputation software. MLIM is highly concerned with imputation fairness for
#'                    factor variables and autobalancing is generally recommended.
#'                    in fact, higher overall accuracy does not mean a better imputation as
#'                    long as minority classes are neglected, which increases the bias in favor of the
#'                    majority class. if you do not wish to autobalance all the
#'                    factor variables, you can manually specify the variables
#'                    that should be balanced using the 'balance' argument (see below).
#'
#                    NOTE: when a variable is balanced prior to the imputation, a different
#                    bootstrap sampling procedure will be used. in doing so, instead of
#                    carrying out bootstrap subsamples with replacement and adding the
#                    duplicated observations as weights in the imputation, undersampling
#                    bootstrap procedure without replacement is performed because the weights
#                    of the artificially balanced data will conflicts the weights of the
#                    bootstrap data.
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
#' @param flush logical (experimental). if TRUE, after each model, the server is
#'              cleaned to retrieve RAM. this feature is in testing mode and is
#'              currently set to FALSE by default, but it is recommended if you
#'              have limited amount of RAM or large datasets.
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
#'                  for larger datasets, value of "1e-3" is recommended to reduce number
#'                  of iterations. the default value is '1e-3'.
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
# @param weights_column non-negative integer. a vector of observation weights
#                       can be provided, which should be of the same length
#                       as the dataframe. giving an observation a weight of
#                       Zero is equivalent of ignoring that observation in the
#                       model. in contrast, a weight of 2 is equivalent of
#                       repeating that observation twice in the dataframe.
#                       the higher the weight, the more important an observation
#                       becomes in the modeling process. the default is NULL.
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
#' @param preimputed.data data.frame. if you have used another software for missing
#'                      data imputation, you can still optimize the imputation
#'                      by handing the data.frame to this argument, which will
#'                      bypass the "preimpute" procedure.
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
# @param force.load (NOT YET IMPLEMENTED FOR R).logical (default is TRUE). if TRUE, when loading the mlim class
#                 object, its preserved settings are used for restoring and saving the
#                 following itterations. otherwise, if FALSE, the current arguments of
#                 mlim are used to overpower the settings of the mlim object. the settings
#                 include the full list of the mlim arguments.
#' @param shutdown logical. if TRUE, h2o server is closed after the imputation.
#'                 the default is TRUE
#' @param java character, specifying path to the executable 64bit Java JDK on the
#'             Microsoft Windows machines, if JDK is installed but the path environment
#'             variable is not set.
#' @param ... arguments that are used internally between 'mlim' and 'mlim.postimpute'.
#'            these arguments are not documented in the help file and are not
#'            intended to be used by end user.
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
#' MLIM <- mlim(dfNA, shutdown = FALSE)
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
# ### run GBM, RF, ELNET, and Ensemble algos and allow 60 minutes of tuning for each variable
# ### this requires a lot of RAM on your machine and a lot of time!
# MLIM <- mlim(dfNA, algos = c("GBM", "RF","ELNET","Ensemble"), tuning_time=60*60)
# mlim.error(MLIM, dfNA, iris)
#
# ### if you have a larger data, there is a few things you can set to make the
# ### algorithm faster, yet, having only a marginal accuracy reduction as a trade-off
# MLIM <- mlim(dfNA, algos = 'ELNET', tolerance = 1e-3, doublecheck = FALSE)
#' }
#' @export


mlim <- function(data = NULL,
                 m = 1,
                 algos = c("ELNET"), #impute, postimpute
                 postimpute = FALSE,
                 stochastic = m > 1,
                 ignore = NULL,

                 # computational resources
                 tuning_time = 900,
                 max_models = NULL, # run all that you can
                 maxiter = 10L,
                 #miniter = 2L,
                 cv = 10L,
                 #validation = 0,

                 matching = "AUTO",    #EXPERIMENTAL
                 autobalance = TRUE,
                 balance = NULL,       #EXPERIMENTAL
                 #ignore.rank = FALSE, #to ignore it, they should make it unordered!
                 # weights_column = NULL,

                 # report and reproducibility
                 seed = NULL,
                 verbosity = NULL,
                 report = NULL,

                 # stopping criteria
                 tolerance = 1e-3,
                 doublecheck = TRUE,

                 ## simplify the settings by taking these arguments out
                 preimpute = "RF",
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


                 # NOT YET IMPLEMENTED
                 preimputed.data = NULL,
                 save = NULL,
                 load = NULL,
                 #init = TRUE,
                 shutdown = TRUE,
                 java = NULL,
                 #force.load = TRUE,
                 ...
                 ) {

  # improvements for the next release
  # ============================================================
  # instead of using all the algorithms at each iteration, add the
  #    other algorithms when the first algorithm stops being useful.
  #    perhaps this will help optimizing, while reducing the computation burdon
  # h2o DRF does not give OOB error, so initial comparison preimputation is not possible
  #    HOWEVER, I can estimate the CV for the preimputation procedure
  #
  # instead of adding postimpute_algos, extract it from specified algorithms

  # check the ... arguments
  # ============================================================
  hidden_args <- c("superdebug", "init", "ignore.rank", "sleep", "stochastic")
  stopifnot(
    "incompatible '...' arguments" = (names(list(...)) %in% hidden_args)
  )

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
  miniter     <- 2L
  init        <- threeDots(name = "init", ..., default = TRUE)
  # cv          <- threeDots(name = "cv", ..., default = 10L)
  # flush       <- threeDots(name = "flush", ..., default = TRUE)
  verbose     <- 0
  error_metric<- "RMSE"
  #preimpute   <- "RF"
  ignore.rank <- threeDots(name = "ignore.rank", ..., default = FALSE)  #EXPERIMENTAL
  sleep       <- threeDots(name = "sleep", ..., default = .25)
  superdebug  <- threeDots(name = "superdebug", ..., default = FALSE)
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

    # settings
    # ----------------------------------
    ITERATIONVARS  <- load$ITERATIONVARS# variables to be imputed
    impute         <- load$impute       # reimputation algorithm(s)
    postimputealgos<- load$postimputealgos
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
    #weights_column <- load$weights_column
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
    keep_cv        <- load$keep_cv
    pkg            <- load$pkg #KEEP IT HIDDEN


    # MOVE-ON to the next variable after loading an mlim object
    # ---------------------------------------------------------
    moveOn <- iterationNextVar(m, m.it, k, z, Y, ITERATIONVARS, maxiter)
    m    <- moveOn$m
    m.it <- moveOn$m.it
    k    <- moveOn$k
    z    <- moveOn$z
    Y    <- moveOn$Y
  }

  # ============================================================
  # ============================================================
  # Prepare the imputation settings
  # ============================================================
  # ============================================================
  else {
    if (!is.null(seed)) set.seed(seed) # avoid setting seed by default if it is a continuation

    alg <- algoSelector(algos, postimpute)
    # preimpute <- "RF" #alg$preimpute ## for now, make this global
    impute <- alg$impute
    postimputealgos <- alg$postimpute

    synt <- syntaxProcessing(data, preimpute, impute, ram,
                             matching=matching, miniter, maxiter, max_models,
                             tuning_time, cv, verbosity=verbosity, report, save)
    min_ram <- synt$min_ram
    max_ram <- synt$max_ram
    keep_cv <- synt$keep_cross_validation_predictions
    verbose <- synt$verbose
    debug <- synt$debug
  }

  # disable h2o progress_bar
  #if (!debug) h2o::h2o.no_progress()
  if (!superdebug) h2o::h2o.no_progress()

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

  # Run H2O on the statistics server¤
  # ============================================================
  if (init) {
    #sink(file = report, append = TRUE)
    #message("\n") # for Markdown styling
    capture.output(connection <- init(nthreads = cpu,
                                      min_mem_size = min_ram,
                                      max_mem_size = max_ram,
                                      ignore_config = TRUE,
                                      java = java,
                                      report, debug),
                   file = report, append = TRUE)
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

    dataNA <- VARS$dataNA # the missing data placeholder
    allPredictors <- VARS$allPredictors
    vars2impute <- VARS$vars2impute
    vars2postimpute <- VARS$vars2impute
    storeVars2impute <- vars2impute
    X <- VARS$X
    bdata <- NULL

    # if there is only one variable to impute, there is no need to iterate!
    if (length(vars2impute) < 1) stop("\nthere is nothing to impute!\n")
    else if (length(vars2impute) == 1) {
      if (!is.valid(postimputealgos)) {
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
      if (inherits(preimputed.data, "mlim.mi")) {
        #preimputed.data <- preimputed.data[[1]]
        #stop("use 'mlim.postimpute' function for postimputing multiple imputation datasets\n")
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
                               metrics, tolerance, doublecheck,
                               m, k, X, Y, z, m.it,
                               # loop data
                               vars2impute, vars2postimpute, storeVars2impute,
                               allPredictors, preimpute, impute, postimputealgos,
                               # settings
                               error_metric, FAMILY=FAMILY, cv, tuning_time,
                               max_models,
                               keep_cv,
                               autobalance, balance, seed, save, flush,
                               verbose, debug, report, sleep,
                               # saving settings
                               mem, orderedCols, ignore, maxiter,
                               miniter, matching, ignore.rank,
                               verbosity, error, cpu, max_ram=max_ram, min_ram=min_ram,
                               #??? shutdown has to be fixed in future updates
                               shutdown=FALSE, clean = TRUE,
                               stochastic=stochastic)

    if (m > 1) MI[[m.it]] <- dataLast
    else MI <- dataLast
  }

  message("\n")

  if (shutdown) {
    md.log("shutting down the server", trace=FALSE)
    h2o::h2o.shutdown(prompt = FALSE)
    Sys.sleep(sleep)
  }

  if (m > 1) class(MI) <- "mlim.mi"
  else class(MI) <- c("mlim", "data.frame")

  return(MI)
}




