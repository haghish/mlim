#' @title missing data imputation with automated machine learning
#' @description imputes data.frame with mixed variable types using automated
#'              machine learning (AutoML)
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.init as.h2o h2o.automl h2o.predict h2o.ls
#'             h2o.removeAll h2o.rm h2o.shutdown
#' @importFrom md.log md.log
#' @importFrom VIM kNN
#' @importFrom missRanger missRanger imputeUnivariate
#' @importFrom missForest missForest
#' @importFrom memuse Sys.meminfo
#' @importFrom stats var setNames na.omit
#' @param data a \code{data.frame} or \code{matrix} with missing data
#' @param include_algos character. specify a vector of algorithms to be used
#'        in the process of auto-tuning. the three main algorithms are
#'        \code{"GLM"},
#'        \code{"GBM"},\code{"XGBoost"},
#'
#'        the default is \code{c("GBM")}. the possible algorithms are \code{"GLM"},
#'        \code{"GBM"},\code{"XGBoost"}, \code{"DRF"},
#'        \code{"DeepLearning"}, and \code{"StackedEnsemble")}. Note that the
#'        choice of algorithms to be trained can largely increase the runtime.
#'        for advice on algorithm selection visit \url{https://github.com/haghish/mlim}
#' @param preimpute character. specifies the procedure for handling the missing
#'                  data before initiating the procedures. the default procedure
#'                  is "iterate", which models the missing data with \code{mlim}
#'                  and then adds them as predictors for imputing the other
#'                  variables. This is the slowest procedure, yet the most accurate
#'                  one. You can skip this step by carrying out a quick imputation
#'                  with a fast algorithm. Currently, "missMDA" procedure is
#'                  supported, which carries out a fast imputation via
#'                  \code{missMDA} R package.
#' @param init logical. should h2o Java server be initiated? the default is TRUE.
#'             however, if the Java server is already running, set this argument
#'             to FALSE.
#' @param nthreads integer. launches H2O using all available CPUs or the specified
#'                 number of CPUs.
#' @param max_mem_size character. specifies the minimum size, in bytes, of the
#'                     memory allocation pool to H2O. This value must a multiple
#'                     of 1024 greater than 2MB. Append the letter "m" or "M" to
#'                     indicate megabytes, or "g" or "G" to indicate gigabytes.
#'                     large memory size is particularly advised, especially
#'                     for multicore processes.
#' @param min_mem_size character. specifies the minimum size.
#' @param ignore character vector of column names or index of columns that should
#'               should be ignored in the process of imputation.
#' @param max_model_runtime_secs integer. maximum runtime (in seconds) for imputing
#'                               each variable in each iteration. the default
#'                               is 3600 seconds but for a large dataset, you
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
#' @param matching EXPERIMENTAL. logical. if \code{TRUE}, imputed values are coerced to the
#'                 closest value to the non-missing values of the variable.
#'                 if set to "AUTO", 'mlim' decides whether to match
#'                 or not, based on the variable classes. the default is FALSE.
# @param ordinal_as_integer EXPERIMENTAL. logical, if TRUE, ordinal variables
#                           are imputed as continuous integers (regression) with matching.
#                           if FALSE, they are imputed as categorical, ignoring
#                          their orders.
#' @param maxiter integer. maximum number of iterations. the default value is \code{10},
#'        but it can be reduced to \code{3} (not recommended, see below).
#' @param miniter integer. minimum number of iterations. the default value is
#'                2.
#' @param flush logical (experimental). if TRUE, after each model, the server is
#'              cleaned to retrieve RAM. this feature is in testing mode.
#' @param nfolds logical. specify number of k-fold Cross-Validation (CV). values of
#'               10 or higher are recommended. default is 10.
#' @param iteration_stopping_metric character. specify the minimum improvement
#'                                  in the estimated error to proceed to the
#'                                  following iteration or stop the imputation.
#'                                  the default is 10^-4 for \code{"MAE"}
#'                                  (Mean Absolute Error). this criteria is only
#'                                  applied from the end of the fourth iteration.
#                                  \code{"RMSE"} (Root Mean Square
#                                  Error). other possible values are \code{"MSE"},
#                                  \code{"MAE"}, \code{"RMSLE"}.
#' @param iteration_stopping_tolerance numeric. the minimum rate of improvement
#'                                     in estimated error metric to qualify the
#'                                     imputation for another round of iteration,
#'                                     if the \code{maxiter} is not yet reached.
#'                                     the default value is 50^-3, meaning that
#'                                     in each iteration, the error must be
#'                                     reduced by at least 0.5% of the previous
#'                                     iteration.
#' @param stopping_metric character.
#' @param stopping_rounds integer.
#' @param stopping_tolerance numeric.
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

#' @param report filename. if a filename is specified, the \code{"md.log"} R
#'               package is used to generate a Markdown progress report for the
#'               imputation. the format of the report is adopted based on the
#'               \code{'verbosity'} argument. the higher the verbosity, the more
#'               technical the report becomes. if verbosity equals "debug", then
#'               a log file is generated, which includes time stamp and shows
#'               the function that has generated the message. otherwise, a
#'               reduced markdown-like report is generated.
#' @param save filename. if a filename is specified, an \code{mlim} object is
#'             saved after the end of each iteration. this object not only
#'             includes the imputed dataframe and estimated CV error, but also
#'             includes the information needed for continuing the imputation,
#'             which is very useful feature for imputing large datasets, with a
#'             long runtime. this argument is activated by default and an
#'             mlim object is stored in the local directory named \code{"mlim.rds"}.
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
#' \dontrun{
#' data(iris)
#' irisNA <- missRanger::generateNA(iris, seed = 34)
#'
#' # run ELNET model (recommended)
#' MLIM <- mlim(irisNA)
#' missForest::mixError(MLIM, irisNA, iris)
#'
#' # run GBM model
#' MLIM <- mlim(irisNA, include_algos = "GBM", max_models = 200)
#' missForest::mixError(MLIM, irisNA, iris)
#' }
#' @export


mlim <- function(data,
                 include_algos =  "ELNET", #c("GBM", "StackedEnsemble"),
                 preimpute = "iterate",

                 # multiple imputation settings
                 #m = FALSE, #boot = FALSE, nboot = 0,

                 ignore = NULL,
                 init = TRUE,

                 save = NULL,

                 # computational resources
                 maxiter = 10L,
                 miniter = 2L,
                 nfolds = 10L,
                 max_model_runtime_secs = 3600,
                 max_models = 100, # run all that you can

                 matching = FALSE, #"AUTO", #??? EXPERIMENTAL
                 #ordinal_as_integer = FALSE, #??? DEBUG IT LATER

                 weights_column = NULL,

                 # general setup
                 seed = NULL,
                 verbosity = NULL,
                 report = "mlim.log",


                 # stopping criteria
                 iteration_stopping_metric  = "RMSE", #??? mormalize it
                 iteration_stopping_tolerance = 5e-3,
                 stopping_metric = "AUTO",
                 stopping_rounds = 3,
                 stopping_tolerance=1e-3,

                 # setup the h2o cluster
                 nthreads = -1,
                 max_mem_size = NULL,
                 min_mem_size = NULL,
                 flush = FALSE,
                 shutdown = TRUE,
                 sleep = 1,
                 ...
                 ) {

  # Load the libraries
  # ============================================================
  suppressPackageStartupMessages({requireNamespace("h2o")})
  suppressPackageStartupMessages({requireNamespace("md.log")})
  suppressPackageStartupMessages({requireNamespace("VIM")})
  suppressPackageStartupMessages({requireNamespace("missRanger")})

  # Syntax processing
  # ============================================================
  iterDF  <- NULL
  metrics <- NULL
  error   <- NULL
  debug   <- FALSE
  verbose <- 0

  # examine the data.frame and the arguments
  #??? matching is deactivated
  syntaxProcessing(data, preimpute, include_algos,
                   matching=matching, miniter, maxiter, max_models,
                   max_model_runtime_secs,
                   nfolds, weights_column, report)

  if ("StackEnsemble" %in% include_algos) {
    keep_cross_validation_predictions <- TRUE
  }
  else {
    keep_cross_validation_predictions <- FALSE
  }

  if ("ELNET" %in% include_algos) {
    include_algos[which(include_algos == "ELNET")] <- "GLM"
  }

  # define logging levels and debugging
  if (is.null(verbosity)) verbose <- 0
  else if (verbosity == "warn") verbose <- 1
  else if (verbosity == "info") verbose <- 2
  else if (verbosity == "debug") {
    verbose <- 3
    debug <- TRUE
  }

  # disable h2o progress_bar
  if (!debug) h2o::h2o.no_progress()

  # Initialize the Markdown report / log
  # ============================================================
  if (is.null(report)) report <- tempfile()
  md.log("System information",
         file=report, trace=TRUE, sys.info = TRUE,
         date=TRUE, time=TRUE) #, print=TRUE

  # Run H2O on the statistics server
  # ============================================================
  if (init) {
    sink(file = report, append = TRUE)
    cat("\n") # for Markdown styling
    connection <- init(nthreads = nthreads,
                       min_mem_size = min_mem_size,
                       max_mem_size = max_mem_size,
                       ignore_config = TRUE,
                       report)
    sink()

    sink.reset <- function(){
      for(i in seq_len(sink.number())){
        sink(NULL)
      }
    }
    sink.reset()
    #closeAllConnections()
    print(connection)
  }

  # Identify variables for imputation and their models' families
  # ============================================================
  VARS <- selectVariables(data, ignore, verbose, report)
  dataNA <- VARS$dataNA
  allPredictors <- VARS$allPredictors
  vars2impute <- VARS$vars2impute
  X <- VARS$X
  #>>xxx <<- X
  #>>v2m <<- vars2impute
  #>>vars <<- VARS

  if (debug) {
    #>>var2imp <<- vars2impute
    print(vars2impute)
  }
  #>>DATA1 <<- data
  #??? make fix the ordinal_as_integer argument later on
  Features <- checkNconvert(data, vars2impute, ignore,
                            ordinal_as_integer=FALSE, report)
  #>>FEAT <<- Features
  CLASS <- Features$class
  FAMILY<- Features$family
  data  <- Features$data
  rm(Features)

  # .........................................................
  # PREIMPUTATION
  # .........................................................
  if (preimpute != "iterate") {
    if (tolower(preimpute) == "knn") {
      set.seed(seed)
      data <- VIM::kNN(data, imp_var=FALSE)
      md.log("kNN preimputation is done", date=debug, time=debug, trace=FALSE)
    }
    else if (tolower(preimpute) == "rf") {
      set.seed(seed)
      data <- missForest::missForest(data)$ximp
      md.log("missForest preimputation is done", date=debug, time=debug, trace=FALSE)
    }
    else if (tolower(preimpute) == "ranger") {
      data <- missRanger::missRanger(data, seed = seed)
      md.log("missRanger preimputation is done", date=debug, time=debug, trace=FALSE)
    }
    else if (tolower(preimpute) == "mm") {
      data <- meanmode(data)
      md.log("Mean/Mode preimputation is done", date=debug, time=debug, trace=FALSE)
    }

    # reset the relevant predictors
    X <- allPredictors
  }


  # ............................................................
  # ............................................................
  # ITERATION LOOP
  # ............................................................
  # ............................................................
  k <- 0L
  running <- TRUE
  verboseDigits <- 4L
  error <- setNames(rep(1, length(vars2impute)), vars2impute)
  if (verbose >= 2) {
    cat("\n", abbreviate(vars2impute, minlength = verboseDigits + 2L),
        sep = "\t")
  }

  # update the fresh data
  # ------------------------------------------------------------
  hex <- h2o::as.h2o(data) #ID: data_
  Sys.sleep(sleep)
  hexID <- h2o::h2o.getId(hex)
  md.log(paste("dataset ID:", hexID), trace=FALSE) #, print = TRUE

  Sys.sleep(sleep)
  if (debug) {
    #>>HEX <<- hex
    #>>iDD <<- hexID
    md.log("data was sent to h2o cloud", date=debug, time=debug, trace=FALSE)
  }

  while (running) {

    # update the loop
    k <- k + 1L

    if (verbose) {
      #cat("\nIteration ", k, ":\t", sep = "")
      cat("\nIteration ", k, ":\n", sep = "")
    }

    md.log(paste("Iteration", k), section="section")

    if (debug) md.log("store last data", trace=FALSE)

    dataLast <- as.data.frame(hex)
    attr(dataLast, "metrics") <- metrics
    attr(dataLast, "nrmse") <- error


    #>>if (debug) LASTDATA <<- dataLast

    # .........................................................
    # IMPUTATION LOOP
    # .........................................................
    z <- 0
    for (Y in vars2impute) {
      if (debug) print(memuse::Sys.meminfo()$freeram)

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

        # Auto-Matching specifications
        # ============================================================
        if (matching == "AUTO") {
          if (FAMILY[z] == 'gaussian_integer') matching <- TRUE
          else if (FAMILY[z] == 'quasibinomial') matching <- TRUE
          else matching <- FALSE
          if (debug) md.log(paste("matching:", matching))
        }

        # sort_metric specifications
        # ============================================================
        if (FAMILY[z] == 'binomial') {
          # check if Y is imbalanced
          if (is.imbalanced(data[[Y]])) sort_metric <- "AUCPR"
          else sort_metric <- "AUC"
        }
        else {
          sort_metric <- "AUTO"
        }

        # ------------------------------------------------------------
        # fine-tune a gaussian model
        # ============================================================
        if (FAMILY[z] == 'gaussian' || FAMILY[z] == 'gaussian_integer') {
          fit <- h2o::h2o.automl(x = setdiff(X, Y), y = Y,
                            training_frame = hex[which(!v.na), ],
                            project_name = "mlim",
                            include_algos = include_algos,
                            nfolds = nfolds,
                            exploitation_ratio = 0.1,
                            max_runtime_secs = max_model_runtime_secs,
                            max_models = max_models,
                            weights_column = weights_column[which(!v.na)],
                            keep_cross_validation_predictions =
                              keep_cross_validation_predictions,
                            seed = seed,
                            stopping_metric = stopping_metric,
                            stopping_rounds = stopping_rounds
                            #stopping_tolerance=stopping_tolerance
          )
        }

        # ------------------------------------------------------------
        # fine-tune a classification model
        # ============================================================
        else if (FAMILY[z] == 'binomial' || FAMILY[z] == 'multinomial') {
          fit <- h2o::h2o.automl(x = setdiff(X, Y), y = Y,
                            balance_classes = TRUE,
                            sort_metric = sort_metric,
                            training_frame = hex[which(!v.na), ],
                            project_name = "mlim",
                            include_algos = include_algos,
                            nfolds = nfolds,
                            exploitation_ratio = 0.1,
                            max_runtime_secs = max_model_runtime_secs,
                            max_models = max_models,
                            weights_column = weights_column[which(!v.na)],
                            keep_cross_validation_predictions =
                              keep_cross_validation_predictions,
                            seed = seed,
                            stopping_metric = stopping_metric,
                            stopping_rounds = stopping_rounds
                            #stopping_tolerance=stopping_tolerance
          )
        }

        Sys.sleep(sleep)

        if (debug) {
          #>>FIT <<- fit
          md.log("model fitted", trace=FALSE)
          md.log("model was executed successfully", trace=FALSE)
        }

        ## do not convert pred to a vector. let it be "H2OFrame"
        pred <- h2o::h2o.predict(fit@leader, newdata = hex[which(v.na), X])[,1]
        Sys.sleep(sleep)
        if (debug) md.log("predictions were generated", trace=FALSE)
        perf <- h2o::h2o.performance(fit@leader)
        Sys.sleep(sleep)
        #>>if (debug) perff <<- perf

        # update metrics
        # ------------------------------------------------------------
        metrics <- rbind(metrics, extractMetrics(hex, k, Y, perf, FAMILY[z]))

        if (debug) md.log("performance evaluation", trace=FALSE)

        # ------------------------------------------------------------
        # Matching
        # ============================================================
        if ((matching & FAMILY[z] == 'gaussian') || (matching & FAMILY[z] == 'gaussian_integer')) {
          #PRED <<- as.vector(pred)
          #NONMISS <<- unique(na.omit(data[,Y]))
          matchedVal <- matching(imputed=as.vector(pred),
                           nonMiss=unique(na.omit(data[,Y])),
                           md.log)
          if (!is.null(matchedVal)) hex[which(v.na), Y] <- h2o::as.h2o(matchedVal)
          else {
            md.log("matching failed", trace=FALSE)
            hex[which(v.na), Y] <- pred #h2o requires numeric subsetting
          }

        }
        else {
        #  if (debug) {
            #>>A <<- v.na
            #>>Y <<- Y
            #>>B <<- data[v.na, Y]
            #>>iDD <<- hexID
            #>>PRED <<- pred
            #data[v.na, Y] <- as.vector(pred) #here convert it to a vector
        #  }
          hex[which(v.na), Y] <- pred #h2o requires numeric subsetting
        }

        #hex[which(v.na), Y] <- pred #h2o requires numeric subsetting
        if (debug) md.log("data was updated in h2o cloud", trace=FALSE)
        Sys.sleep(sleep)

        # ??? save models or their predictions or their datasets + errors
        # ------------------------------------------------------------

        # clean h2o memory
        # ------------------------------------------------------------
        #h2o.clean(fit=fit, pred=pred, fun = "erasefit", timeout_secs = 30,
        #          FLUSH = FALSE, retained_elements = c(hexID), md.log = md.log)
        h2o::h2o.rm(pred)
        if (debug) md.log("prediction was cleaned", trace=FALSE)
        Sys.sleep(sleep)
        h2o::h2o.rm(fit)
        if (debug) md.log("model was cleaned", trace=FALSE)
        Sys.sleep(sleep)
        if (debug) md.log("model performance was cleaned", trace=FALSE)

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

    }



    # CHECK CRITERIA FOR RUNNING THE NEXT ITERATION
    # --------------------------------------------------------------
    # ??? needs update for binary and multinomial
    #>>if (debug) METER <<- metrics
    if (debug) md.log("evaluating stopping criteria", trace=FALSE)
    SC <- stoppingCriteria(miniter, maxiter,
                           metrics, k,
                           iteration_stopping_metric,
                           iteration_stopping_tolerance,
                           md.log = report)
    if (debug) {
      md.log(paste("running: ", SC$running), trace=FALSE)
      md.log(paste("Estimated",iteration_stopping_metric,
                   "error:", SC$error), trace=FALSE)
    }
    running <- SC$running
    error <- SC$error



    # prepare mlim object for future imputation in the future
    # --------------------------------------------------------------
    if (!is.null(save)) {
      if (!is.null(iterDF)) {
        currentData <- as.data.frame(hex)
        attr(currentData, "metrics") <- metrics
        iterDF <- append(iterDF, list(currentData))
        rm(currentData)
        gc()
      }
      else {
        currentData <- as.data.frame(hex)
        attr(currentData, "metrics") <- metrics
        iterDF <- list(currentData)
        rm(currentData)
        gc()

      #mlimClass <- list(iteration=iterDF, k = k, include_algos=include_algos,
      #                  ignore=ignore, save = save, iterdata = iterdata,
      #                  maxiter = maxiter, miniter = miniter, nfolds = nfolds,
      #                  max_model_runtime_secs = max_model_runtime_secs,
      #                  max_models = max_models, matching = matching,
      #                  ordinal_as_integer = ordinal_as_integer,
      #                  weights_column = weights_column, seed=seed,
      #                  verbosity=verbosity, report=report,
      #                  iteration_stopping_metric=iteration_stopping_metric,
      #                  iteration_stopping_tolerance=iteration_stopping_tolerance,
      #                  stopping_metric=stopping_metric,stopping_rounds=stopping_rounds,
      #                  stopping_tolerance=stopping_tolerance,
      #                  nthreads = nthreads, max_mem_size=max_mem_size,
      #                  min_mem_size = min_mem_size,
      #
      #                  # save the package version used for the imputation
      #                  pkg=packageVersion("mlim")
      #                  )
      #class(mlimClass) <- "mlim"
      }
      # update iteration data
      saveRDS(iterDF, save)
    }

  }

  # ............................................................
  # END OF THE ITERATIONS
  # ............................................................
  if (verbose) cat("\n\n")

  md.log("This is the end, beautiful friend...", trace=FALSE)

  # if the iterations stops on minimum or maximum, return the last data
  if (k == miniter || (k == maxiter && running)) {
    md.log("limit reached", trace=FALSE)
    dataLast <- as.data.frame(hex)
    Sys.sleep(sleep)
    attr(dataLast, "metrics") <- metrics
    attr(dataLast, iteration_stopping_metric) <- error
  }
  else {
    md.log("return previous iteration's data", trace=FALSE)
  }

  if (shutdown) {
    md.log("shutting down the server", trace=FALSE)
    h2o::h2o.shutdown(prompt = FALSE)
    Sys.sleep(sleep)
  }
  return(dataLast)
}




