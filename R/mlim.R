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
#' @param algos character. specify a vector of algorithms to be used
#'        in the process of auto-tuning. the supported main algorithms are
#'        \code{"ELNET"}, \code{"RF"},
#'        \code{"GBM"}, \code{"DL"}, \code{"XGB"} (available for Mac and Linux), and \code{"Ensemble"}.
#'
#'        the default is \code{c("ELNET", "RF")}, which tunes fast.Note that the
#'        choice of algorithms to be trained can largely increase the runtime.
#'        for advice on algorithm selection visit \url{https://github.com/haghish/mlim}.
#'        GBM, DL, XGB, and Ensemble take the full given "tuning_time" (see below) to
#'        tune the best model for imputing he given variable.
#'        if \code{load} is provided, this argument will be ignored.
#' @param preimpute character. specifies the procedure for handling the missing
#'                  data before initiating the procedures. the default procedure
#'                  is "rf", which models the missing data with parallel Random Forest
#'                  model. possible alternative is \code{"mm"}, which carries out
#'                  mean/mode replacement. "mm" is much faster, but will dramatically
#'                  increase the number of required itterations, significantly adding
#'                  to the required computational resources. if your dataset is very
#'                  large, consider imputing it before hand and then passing the
#'                  imputed dataset for optimization (see "preimputed.data")
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
#' @param ignore.rank logical, if FALSE (default), ordinal variables
#'                    are imputed as continuous integers with regression plus matching
#'                    and are reverted to ordinal later again. this procedure is
#'                    recommended. if FALSE, the rank of the categories will be ignored
#'                    the the algorithm will try to optimize for classification accuracy.
#'                    WARNING: the latter often results in very high classification accuracy but at
#'                    the cost of higher rank error. see the "mlim.error" function
#'                    documentation to see how rank error is computed. therefore, if you
#'                    intend to carry out analysis on the rank data as numeric, it is
#'                    recommended that you set this argument to FALSE.
#' @param maxiter integer. maximum number of iterations. the default value is \code{10},
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
#' @param save filename. if a filename is specified, an \code{mlim} object is
#'             saved after the end of each variable imputation. this object not only
#'             includes the imputed dataframe and estimated cross-validation error, but also
#'             includes the information needed for continuing the imputation,
#'             which is very useful feature for imputing large datasets, with a
#'             long runtime. this argument is activated by default and an
#'             mlim object is stored in the local directory named \code{"mlim.rds"}.
#' @param load an object of class "mlim", which includes the data, arguments,
#'                 and settings for re-running the imputation, from where it was
#'                 previously stopped. the "mlim" object saves the current state of
#'                 the imputation and is particularly recommended for large datasets
#'                 or when the user specifies a computationally extensive settings
#'                 (e.g. specifying several algorithms, increasing tuning time, etc.).
#' @param force.load logical (default is TRUE). if TRUE, when loading the mlim class
#'                 object, its preserved settings are used for restoring and saving the
#'                 following itterations. otherwise, if FALSE, the current arguments of
#'                 mlim are used to overpower the settings of the mlim object. the settings
#'                 include the full list of the mlim arguments.
#' @param verbosity character. controls how much information is printed to console.
#'                  the value can be "warn" (default), "info", "debug", or NULL.
#' @param shutdown logical. if TRUE, h2o server is closed after the imputation.
#'                 the default is TRUE
#' @param sleep integer. number of seconds to wait after each interaction with h2o
#'              server. the default is 1 second. larger values might be needed
#'              depending on your computation power or dataset size.
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
#' MLIM <- mlim(irisNA, algos = "GBM", tuning_time=60*15)
#' mlim.error(MLIM, irisNA, iris)
#' }
#' @export


mlim <- function(data = NULL,
                 algos =  "AUTO",
                 preimpute = "rf",
                 preimputed.data = NULL,
                 ignore = NULL,
                 init = TRUE,
                 # multiple imputation settings
                 #m = 1,

                 # computational resources
                 maxiter = 10L,
                 miniter = 2L,
                 cv = 10L,
                 #validation = 0,
                 tuning_time = 180,
                 max_models = NULL, # run all that you can

                 matching = "AUTO",   #EXPERIMENTAL
                 balance = NULL,      #EXPERIMENTAL
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
                 #error_metric  = "RMSE", #??? mormalize it
                 #stopping_metric = "AUTO",
                 #stopping_rounds = 3,
                 #stopping_tolerance=1e-3,

                 # setup the h2o cluster
                 cpu = -1,
                 ram = NULL,
                 flush = FALSE,
                 shutdown = TRUE,
                 sleep = .5,

                 save = NULL,
                 load = NULL,
                 force.load = TRUE,
                 ...
                 ) {




  # improvements for the next release
  # ============================================================
  # instead of using all the algorithms at each iteration, add the
  #    other algorithms when the first algorithm stops being useful.
  #    perhaps this will help optimizing, while reducing the computation burdon
  # h2o DRF does not give OOB error, so initial comparison preimputation is not possible
  #    HOWEVER, I can estimate the CV for the preimputation procedure


  # Simplify the syntax by taking arguments that are less relevant to the majority
  # of the users out
  # ============================================================
  #stopping_metric <- "AUTO"
  #stopping_rounds <- 3
  #stopping_tolerance <- 1e-3
  error_metric  <- "RMSE"
  ignore.rank <- FALSE #EXPERIMENTAL

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
      algos          <- load$algos
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
      cat("'algos', 'ignore', 'save', 'maxiter', 'miniter', 'cv', \n")
      cat("'tuning_time', 'max_models', 'matching', 'weights_column', \n")
      cat("'seed', 'verbosity', 'report', 'flush', 'tolerance', 'cpu', 'ram'\n")

      synt <- syntaxProcessing(data, preimpute, algos, ram,
                               matching=matching, miniter, maxiter, max_models,
                               tuning_time, cv, weights_column,
                               verbosity, report)

      min_ram <- synt$min_ram
      max_ram <- synt$max_ram
      keep_cross_validation_predictions <- synt$keep_cross_validation_predictions
      algos <- synt$algos
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
    metrics <- NULL
    error   <- NULL
    debug   <- FALSE
    verbose <- 0

    synt <- syntaxProcessing(data, preimpute, algos, ram,
                             matching=matching, miniter, maxiter, max_models,
                             tuning_time, cv, weights_column,
                             verbosity, report)

    min_ram <- synt$min_ram
    max_ram <- synt$max_ram
    keep_cross_validation_predictions <- synt$keep_cross_validation_predictions
    algos <- synt$algos
    verbose <- synt$verbose
    debug <- synt$debug
  }

  # disable h2o progress_bar
  if (!debug) h2o::h2o.no_progress()

  # Initialize the Markdown report / log
  # ============================================================
  if (is.null(report)) report <- tempfile()
  md.log("System information",
         file=report, trace=TRUE, sys.info = TRUE,
         date=TRUE, time=TRUE) #, print=TRUE

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
  X <- VARS$X

  # if there is only one variable to impute, there is no need to iterate!
  if (length(vars2impute) == 1) maxiter <- 1

  Features <- checkNconvert(data, vars2impute, ignore,
                            ignore.rank=ignore.rank, report)

  FAMILY<- Features$family
  data  <- Features$data
  mem <- Features$mem
  orderedCols <- Features$orderedCols
  rm(Features)

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



  # update the fresh data
  # ------------------------------------------------------------
  running <- TRUE
  hex <- h2o::as.h2o(data) #ID: data_
  Sys.sleep(sleep)
  hexID <- h2o::h2o.getId(hex)
  md.log(paste("dataset ID:", hexID), trace=FALSE) #, print = TRUE

  Sys.sleep(sleep)
  if (debug) md.log("data was sent to h2o cloud", date=debug, time=debug, trace=FALSE)

  while (running) {

    # update the loop
    k <- k + 1L

    # always print the iteration
    cat("\nIteration ", k, ":\n", sep = "") #":\t"


    md.log(paste("Iteration", k), section="section")

    if (debug) md.log("store last data", trace=FALSE)

    dataLast <- as.data.frame(hex)
    attr(dataLast, "metrics") <- metrics
    attr(dataLast, "rmse") <- error


    #>>if (debug) LASTDATA <<- dataLast

    # .........................................................
    # IMPUTATION LOOP
    # .........................................................
    z <- 0
    for (Y in vars2impute) {
      if (verbose==0) pb <- txtProgressBar(z, length(vars2impute), style = 3)
      if (debug) print(paste0(Y," (RAM = ",memuse::Sys.meminfo()$freeram,")"))

      #print(paste("XXX:   ", X))
      z <- z + 1
      v.na <- dataNA[, Y]

      if (debug) {
        md.log(Y, section="subsection")
        md.log(paste("family:", FAMILY[z]))
      }

      md.log(paste(X, collapse = ","))
      if (length(X) == 0L) {
        print(X)
        if (debug) md.log("uni impute", trace=FALSE)
        #??? change this with a self-written function
        hex[[Y]] <- h2o::as.h2o(missRanger::imputeUnivariate(data[[Y]]))
        Sys.sleep(sleep)
      }
      else {

        if (debug) md.log(paste("X:",paste(setdiff(X, Y), collapse = ", ")), trace=FALSE)
        if (debug) md.log(paste("Y:",paste(Y, collapse = ", ")), trace=FALSE)

        # sort_metric specifications
        # ============================================================
        sort_metric <- "AUTO"
        #if (FAMILY[z] == 'binomial') {
        #  # check if Y is imbalanced
        #  if (is.imbalanced(data[[Y]])) sort_metric <- "AUCPR"
        #  else sort_metric <- "AUC"
        #}
        #else {
        #  sort_metric <- "AUTO"
        #}

        # ------------------------------------------------------------
        # fine-tune a gaussian model
        # ============================================================
        if (FAMILY[z] == 'gaussian' || FAMILY[z] == 'gaussian_integer'
            || FAMILY[z] == 'quasibinomial' ) {
          fit <- h2o::h2o.automl(x = setdiff(X, Y), y = Y,
                            training_frame = hex[which(!v.na), ],
                            sort_metric = sort_metric,
                            project_name = "mlim",
                            include_algos = algos,
                            nfolds = cv,
                            exploitation_ratio = 0.1,
                            max_runtime_secs = tuning_time,
                            max_models = max_models,
                            weights_column = weights_column[which(!v.na)],
                            keep_cross_validation_predictions =
                              keep_cross_validation_predictions,
                            seed = seed
                            #stopping_metric = stopping_metric,
                            #stopping_rounds = stopping_rounds
                            #stopping_tolerance=stopping_tolerance
          )
        }

        # ------------------------------------------------------------
        # fine-tune a classification model
        # ============================================================
        else if (FAMILY[z] == 'binomial' || FAMILY[z] == 'multinomial') {

          # check balance argument (default is FALSE)
          balance_classes <- FALSE
          sort_metric <- "AUC"
          if (Y %in% balance) {
            balance_classes <- TRUE
            sort_metric <- "AUCPR"
          }

          #???
          #if (validation > 0) {
          #  nonMissingObs <- which(!v.na)
          #  vdFrame <- sort(sample(nonMissingObs,
          #                    round(validation * length(nonMissingObs) )))
          #  trainingsample <- sort(setdiff(which(!v.na), vdFrame))
          #}

          fit <- h2o::h2o.automl(x = setdiff(X, Y), y = Y,
                            balance_classes = balance_classes,
                            sort_metric = sort_metric,
                            training_frame = hex[which(!v.na), ],
                            #validation_frame = hex[vdFrame, ],
                            project_name = "mlim",
                            include_algos = algos,
                            nfolds = cv,
                            exploitation_ratio = 0.1,
                            max_runtime_secs = tuning_time,
                            max_models = max_models,
                            weights_column = weights_column[which(!v.na)],
                            keep_cross_validation_predictions =
                              keep_cross_validation_predictions,
                            seed = seed
                            #stopping_metric = stopping_metric,
                            #stopping_rounds = stopping_rounds
                            #stopping_tolerance=stopping_tolerance
          )
        }
        else {
          stop(paste(FAMILY[z], "is not recognized"))
        }

        Sys.sleep(sleep)
        #print(fit@leaderboard)

        if (debug) {
          md.log("model fitted", trace=FALSE)
          md.log("model was executed successfully", trace=FALSE)
        }


        perf <- h2o::h2o.performance(fit@leader)
        Sys.sleep(sleep)

        # update metrics, and if there is an improvement, update the data
        # ------------------------------------------------------------
        roundRMSE <- getDigits(tolerance) + 1
        if (roundRMSE == 1) roundRMSE <- 4

        iterationMetric <- extractMetrics(hex, k, Y, perf, FAMILY[z])

        if (k == 1) {
          ## do not convert pred to a vector. let it be "H2OFrame"
          pred <- h2o::h2o.predict(fit@leader, newdata = hex[which(v.na), X])[,1]
          Sys.sleep(sleep)
          if (debug) md.log("predictions were generated", trace=FALSE)
          hex[which(v.na), Y] <- pred #h2o requires numeric subsetting

          # RAM cleaning-ish, help needed
          h2o::h2o.rm(pred)
          if (debug) md.log("prediction was cleaned", trace=FALSE)
          Sys.sleep(sleep)

          # update the metrics
          metrics <- rbind(metrics, iterationMetric)
        }
        else {

          errPrevious <- min(metrics[metrics$variable == Y, error_metric], na.rm = TRUE)
          errPrevious <- round(errPrevious, digits = 5)
          checkMetric <- iterationMetric[iterationMetric$variable == Y, error_metric]
          checkMetric <- round(checkMetric, digits = 5)


          # >>> this bit looks like I'm paranoid again... improvement needed
          if (!is.null(errPrevious)) {
            if (is.na(errPrevious)) {
              errPrevious <- 1
            }
          }
          else errPrevious <- 1
          # <<<

          errImprovement <- checkMetric - errPrevious
          percentImprove <- (errImprovement / errPrevious)

          if (percentImprove < -tolerance) { #if error decreased
            ## do not convert pred to a vector. let it be "H2OFrame"
            pred <- h2o::h2o.predict(fit@leader, newdata = hex[which(v.na), X])[,1]
            Sys.sleep(sleep)
            if (debug) md.log("predictions were generated", trace=FALSE)
            hex[which(v.na), Y] <- pred #h2o requires numeric subsetting
            Sys.sleep(sleep)
            if (debug) md.log("data was updated in h2o cloud", trace=FALSE)

            # RAM cleaning-ish, help needed
            h2o::h2o.rm(pred)
            if (debug) md.log("prediction was cleaned", trace=FALSE)

            # update the metrics
            metrics <- rbind(metrics, iterationMetric)
          }

          else { #if the error increased
            iterationMetric[, error_metric] <- NA
            metrics <- rbind(metrics, iterationMetric)
            if (!doublecheck) {
              print(vars2impute)
              vars2impute <- setdiff(vars2impute, Y)
            }
          }
        }

        h2o::h2o.rm(fit)
        if (debug) md.log("model was cleaned", trace=FALSE)
        Sys.sleep(sleep)

        # clean h2o memory
        # ------------------------------------------------------------
        #h2o.clean(fit=fit, pred=pred, fun = "erasefit", timeout_secs = 30,
        #          FLUSH = FALSE, retained_elements = c(hexID), md.log = md.log)

        gc()
        gc()
        # tell back-end cluster nodes to do three back-to-back JVM full GCs.
        #h2o:::.h2o.garbageCollect()
        #h2o:::.h2o.garbageCollect()
        #h2o:::.h2o.garbageCollect()
      }

      # Update the predictors during the first iteration
      # ------------------------------------------------------------
      if (preimpute == "iterate" && k == 1L && (Y %in% allPredictors)) {
        X <- union(X, Y)
      }

      # .........................................................
      # POSTIMPUTATION PREPARATION
      # .........................................................
      if (!is.null(save)) {

        postimpute <- list(

          # Data
          # ====
          data=as.data.frame(hex),
          dataLast=dataLast,
          metrics = metrics,
          mem=mem,
          orderedCols=orderedCols,

          # loop data
          # ---------
          k = k, z=z, X=X, Y=Y, vars2impute=vars2impute, FAMILY=FAMILY,

          # settings
          # --------
          algos=algos,
          ignore=ignore,
          save = save,
          maxiter = maxiter,
          miniter = miniter,
          cv = cv,
          tuning_time = tuning_time,
          max_models = max_models,
          matching = matching,
          ignore.rank = ignore.rank,
          weights_column = weights_column,
          seed=seed,
          verbosity=verbosity,
          verbose = verbose,
          debug = debug,
          report=report,
          flush=flush,
          error_metric=error_metric,
          tolerance=tolerance,
          error = error, #PROBABLY NOT NEEDED ???
          #stopping_metric=stopping_metric,
          #stopping_rounds=stopping_rounds,
          #stopping_tolerance=stopping_tolerance,
          cpu = cpu, max_ram=max_ram,
          min_ram = min_ram,

          # save the package version used for the imputation
          pkg=packageVersion("mlim")
        )
        class(postimpute) <- "mlim"

        # update iteration data
        saveRDS(postimpute, save)
      }


      # Flush the Java server to regain RAM
      # ------------------------------------------------------------
      #
      # HELP NEEDED
      #
      # this part is problematic. Contact h2o.ai for further help
      # because removing objects from the server doesn't seem to do
      # much. moreover, '.h2o.garbageCollect()' breaks some of the
      # computations for unknown reasons. for now, I can only use
      # 'gc()' and alternatively, shutdown the Java server and re-run
      # it when the RAM goes below a certain amount... this feature is
      # considered a bad practice and should be improved in future releases
      # ------------------------------------------------------------
      if (flush) {
        currentData <- as.data.frame(hex)
        h2o::h2o.removeAll(timeout_secs = 30)
        Sys.sleep(sleep)
        gc()
        gc()
        #h2o:::.h2o.garbageCollect()
        #h2o:::.h2o.garbageCollect()
        #h2o:::.h2o.garbageCollect()
        Sys.sleep(sleep)
        hex <- h2o::as.h2o(currentData) #ID: data_
        Sys.sleep(sleep)
        hexID <- h2o::h2o.getId(hex)
        if (debug) md.log("flushed", trace=FALSE)
      }

      # update the statusbar
      if (verbose==0) setTxtProgressBar(pb, z)
    }



    # CHECK CRITERIA FOR RUNNING THE NEXT ITERATION
    # --------------------------------------------------------------
    # ??? needs update for binary and multinomial
    #>>if (debug) METER <<- metrics
    if (debug) md.log("evaluating stopping criteria", trace=FALSE)
    SC <- stoppingCriteria(method="varwise_NA", miniter, maxiter,
                           metrics, k, vars2impute,
                           error_metric,
                           tolerance,
                           md.log = report)
    if (debug) {
      md.log(paste("running: ", SC$running), trace=FALSE)
      md.log(paste("Estimated",error_metric,
                   "error:", SC$error), trace=FALSE)
    }
    running <- SC$running
    error <- SC$error
  }

  # ............................................................
  # END OF THE ITERATIONS
  # ............................................................
  if (verbose) cat("\n\n")

  md.log("This is the end, beautiful friend...", trace=FALSE)

  # if the iterations stops on minimum or maximum, return the last data
  if (k == miniter || (k == maxiter && running) || maxiter == 1) {
    md.log("limit reached", trace=FALSE)
    dataLast <- as.data.frame(hex)
    Sys.sleep(sleep)
    attr(dataLast, "metrics") <- metrics
    attr(dataLast, error_metric) <- error
  }
  else {
    md.log("return previous iteration's data", trace=FALSE)
  }

  if (shutdown) {
    md.log("shutting down the server", trace=FALSE)
    h2o::h2o.shutdown(prompt = FALSE)
    Sys.sleep(sleep)
  }

  # ------------------------------------------------------------
  # Auto-Matching specifications
  # ============================================================
  if (matching == "AUTO") {
    z <- 0
    for (Y in vars2impute) {
      z <- z + 1
      v.na <- dataNA[, Y]

      if ((FAMILY[z] == 'gaussian_integer') | (FAMILY[z] == 'quasibinomial')) {
        if (debug) md.log(paste("matching", Y))

        matchedVal <- matching(imputed=dataLast[v.na, Y],
                               nonMiss=unique(dataLast[!v.na,Y]),
                               md.log)
        #print(matchedVal)
        if (!is.null(matchedVal)) dataLast[v.na, Y] <- matchedVal
        else {
          md.log("matching failed", trace=FALSE)
        }
      }
    }
  }

  # ------------------------------------------------------------
  # Revert ordinal transformation
  # ============================================================
  if (!ignore.rank) {
    dataLast[, orderedCols] <-  revert(dataLast[, orderedCols, drop = FALSE], mem)
  }

  return(dataLast)
}




