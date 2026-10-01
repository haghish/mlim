#' @title iteration_loop
#' @description runs imputation iteration loop to fully impute a dataframe
#' @importFrom utils setTxtProgressBar txtProgressBar capture.output packageVersion
#' @importFrom md.log md.log
#' @importFrom memuse Sys.meminfo
#' @importFrom stats var setNames na.omit
#' @return list
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

iteration_loop <- function(MI, dataNA, preimputed.data, data, bdata, boot, metrics, tolerance,
                           m, k, X, Y, z, m.it,

                           # loop data
                           vars2impute,
                           allPredictors, preimpute, impute,
                           hierarchy = NULL,

                           # settings
                           error_metric, FAMILY, cv, tuning_time,
                           max_models,
                           autobalance,
                           seed, save,
                           verbose, debug, report, sleep,

                           # saving settings
                           mem, orderedCols, ignore, maxiter,
                           miniter, matching, ignore.rank,
                           verbosity, error, cpu, clean,
                           stochastic,
                           running = TRUE) {

  # ------------------------------------------------------------
  # bootrtap
  #
  # Bootstrap from the original dataset, hold the original NA values,
  # and then use the preimputed dataset, and then gradually improve it
  #
  #### PROBLEM
  ############
  #### drop the duplicates because they screw up the k-fold cross-validation.
  #### multiple identical observations might go to train and test datasets.
  #### here I suggest several 'work-in-progress' solutions
  # ============================================================

  # Bootstrap duplicates are represented through observation weights.
  # When autobalance is requested, iterate() combines these multiplicity
  # weights with outcome-balancing weights for multiple imputation.
  # Learner-specific support for these weights is handled inside iterate().

  if (boot) {
    rownames(data) <- 1:nrow(data) #remember the rows that are missing
    sampling_index <- sample(x = nrow(data), size = nrow(data), replace=TRUE)



    ## SOLUTION 1: DROP THE DUPLICATES AND DO UNDERSAMPLING
    ## ----------------------------------------------------
    # bdata <- data[sampling_index, ]
    # bdataNA <- is.na(bdata[, vars2impute, drop = FALSE])
    # bdata <- mlim.preimpute(data=bdata, preimpute=preimpute, seed = NULL)
    # sampling_index <- sampling_index[!duplicated(sampling_index)]
    # bdata <- data[sampling_index, ]
    # bdata[, "mlim_bootstrap_weights_column_"] <- 1
    # bdataNA <- is.na(bdata[, vars2impute, drop = FALSE])

    ## SOLUTION 2: ADD THE DUPLICATES TO THE WEIGHT_COLUMN
    ## ----------------------------------------------------
    dups <- bootstrapWeight(sampling_index)
    bdata <- data[1:nrow(data) %in% dups[,1], ]
    bdataNA <- is.na(bdata[, vars2impute, drop = FALSE])
    message("\n")
    bdata <- mlim.preimpute(data=bdata, preimpute=preimpute, seed = NULL)
    bdata[, "mlim_bootstrap_weights_column_"] <- dups[,2] #OR ALTERNATIVELY #dups[,2] / sum(dups[,2])

    # mlr3 learners require complete predictor matrices. Keep dataNA as the
    # original missingness mask, but initialize the working data before
    # model.matrix() is constructed in iterate().
    data <- mlim.preimpute(data=data, preimpute=preimpute, seed = NULL)

    ## SOLUTION 3: Assign CV folding manually instead of weight_column
    ## ----------------------------------------------------
    # bdata <- data[sampling_index, ]
    # bdataNA <- is.na(bdata[, vars2impute, drop = FALSE])
    # bdata <- mlim.preimpute(data=bdata, preimpute=preimpute, seed = NULL)
    # bdata[, "mlim_bootstrap_fold_assignment_"] <- 0
    # folds <- bootstrapCV(index = sampling_index, cv = cv)
    # for (i in 1:cv) {
    #   indexcv <- sampling_index %in% folds[,i]
    #   bdata[indexcv, "mlim_bootstrap_fold_assignment_"] <- i
    # }
  }

  # update the fresh data
  # ------------------------------------------------------------

  if (debug) md.log("iteration data prepared", date=debug, time=debug, trace=FALSE)

  # define iteration var. this is a vector of varnames that should be imputed
  ITERATIONVARS <- vars2impute

  # Keep the original variable and predictor sets. Multilevel summary
  # variables are regenerated at the start of each global iteration.
  basePredictors <- allPredictors
  baseX <- X
  multilevel_variables <- character(0)

  if (!is.null(hierarchy)) {
    multilevel_source_variables <- setdiff(
      names(data),
      c(
        hierarchy,
        "mlim_bootstrap_weights_column_",
        "mlim_model_weights_column_"
      )
    )
  }
  else {
    multilevel_source_variables <- character(0)
  }

  # ============================================================
  # ============================================================
  # global iteration loop
  # ============================================================
  # ============================================================
  while (running) {

    # ----------------------------------------------------------
    # Regenerate multilevel summary variables
    # ----------------------------------------------------------
    if (!is.null(hierarchy)) {

      # Remove summaries generated in the previous global iteration.
      old_multilevel <- attr(data, "mlim.multilevel.variables")
      if (!is.null(old_multilevel) && length(old_multilevel) > 0L) {
        data <- data[
          , setdiff(names(data), old_multilevel),
          drop = FALSE
        ]
      }

      attr(data, "mlim.hierarchy") <- NULL
      attr(data, "mlim.original.variables") <- NULL
      attr(data, "mlim.multilevel.variables") <- NULL

      data <- mlim.multilevel(
        data = data,
        hierarchy = hierarchy,
        variables = multilevel_source_variables
      )

      multilevel_variables <- attr(data, "mlim.multilevel.variables")

      # Leave-one-out cluster summaries can be undefined for singleton
      # clusters. H2O previously tolerated missing predictors, whereas the
      # current mlr3 learners use complete model matrices. Fill only these
      # generated summary predictors before model fitting.
      if (length(multilevel_variables) > 0L &&
          anyNA(data[, multilevel_variables, drop = FALSE])) {
        data[, multilevel_variables] <- medianmode(
          data[, multilevel_variables, drop = FALSE]
        )
      }

      # Multiple imputation uses a separate bootstrap working dataset.
      if (!is.null(bdata)) {

        old_b_multilevel <- attr(bdata, "mlim.multilevel.variables")

        if (!is.null(old_b_multilevel) &&
            length(old_b_multilevel) > 0L) {
          bdata <- bdata[
            , setdiff(names(bdata), old_b_multilevel),
            drop = FALSE
          ]
        }

        attr(bdata, "mlim.hierarchy") <- NULL
        attr(bdata, "mlim.original.variables") <- NULL
        attr(bdata, "mlim.multilevel.variables") <- NULL

        bdata <- mlim.multilevel(
          data = bdata,
          hierarchy = hierarchy,
          variables = intersect(
            multilevel_source_variables,
            names(bdata)
          ),
          weights = bdata[["mlim_bootstrap_weights_column_"]]
        )

        b_multilevel_variables <- attr(
          bdata,
          "mlim.multilevel.variables"
        )

        if (length(b_multilevel_variables) > 0L &&
            anyNA(bdata[, b_multilevel_variables, drop = FALSE])) {
          bdata[, b_multilevel_variables] <- medianmode(
            bdata[, b_multilevel_variables, drop = FALSE]
          )
        }

      }

      # Make the generated summaries available to every imputation model.
      allPredictors <- unique(
        c(basePredictors, multilevel_variables)
      )

      X <- unique(c(baseX, multilevel_variables))

    }

    # always print the iteration
    message(paste0("\ndata ", m.it, ", iteration ", k, " (RAM = ", memuse::Sys.meminfo()$freeram,")", ":"), sep = "") #":\t"
    md.log(paste("Iteration", k), section="subsection")

    for (Y in ITERATIONVARS[z:length(ITERATIONVARS)]) {
      start <- as.integer(Sys.time())

      # Prepare the progress bar and iteration console text
      # ============================================================
      if (verbose==0) pb <- txtProgressBar((which(ITERATIONVARS == Y))-1, length(vars2impute), style = 3)
      if (verbose!=0) message(paste0("    ",Y))

      it <- NULL
      tryCatch(capture.output(
        it <- iterate(
          MI, dataNA, bdataNA,
          preimputed.data, data, bdata, boot, metrics, tolerance,
          m, k, X, Y, z = which(ITERATIONVARS == Y), m.it,
          # loop data
          ITERATIONVARS, vars2impute,
          allPredictors, preimpute, impute,
          hierarchy = hierarchy,
          # settings
          error_metric, FAMILY = FAMILY, cv, tuning_time,
          max_models,
          autobalance,
          seed, save,
          verbose, debug, report, sleep,
          # saving settings
          mem, orderedCols, ignore, maxiter,
          miniter, matching, ignore.rank,
          verbosity, error, cpu, stochastic)
        , file = report, append = TRUE)
        , error = function(cond) {
          message(paste0("\nReimputing '", Y, "' with the current specified algorithms failed and this variable will be skipped! \nSee error below:"));
          md.log(paste("Reimputing", Y, "failed and the variable will be skipped!"),
                 date = TRUE, time = TRUE, print = TRUE)
          message(cond)

          ### ??? activate the code below if you allow "iterate" preimputation
          ### ??? or should it be ignored...
          # if (preimpute == "iterate" && k == 1L && (Y %in% allPredictors)) {
          #   X <- union(X, Y)
          #   if (debug) md.log("x was updated", date=debug, time=debug, trace=FALSE)
          # }
          return(NULL)
        })



      # If there was no error, update the variables
      # else make sure the model is cleared
      # --------------------------------------------------------------
      #IT <<- it
      if (!is.null(it)) {
        X             <- it$X
        ITERATIONVARS <- it$iterationvars
        metrics       <- it$metrics
        data          <- it$data
        bdata         <- it$bdata

      }

      # log & statusbar
      # --------------------------------------------------------------
      time = as.integer(Sys.time()) - start
      if (debug) md.log(paste("done! after: ", time, " seconds"),
                        date = TRUE, time = TRUE, print = FALSE, trace = FALSE)

      # update the statusbar
      if (verbose==0) setTxtProgressBar(pb, (which(ITERATIONVARS == Y)))
    }

    # CHECK CRITERIA FOR RUNNING THE NEXT ITERATION
    # --------------------------------------------------------------
    if (debug) md.log("evaluating stopping criteria", date=debug, time=debug, trace=FALSE)
    SC <- stoppingCriteria(method="varwise_NA", miniter, maxiter,
                           metrics, k, vars2impute,
                           error_metric,
                           tolerance,
                           md.log = report)
    if (debug) {
      md.log(paste("running: ", SC$running), date=debug, time=debug, trace=FALSE)
      md.log(paste("\nEstimated", error_metric, "error:", SC$error), section="paragraph", date=debug, time=debug, trace=FALSE)
    }

    running <- SC$running
    error <- SC$error

    # update the loop number
    k <- k + 1L
  }

  # ............................................................
  # END OF THE ITERATIONS
  # ............................................................
  if (verbose) message("\n")
  md.log("", section="paragraph", trace=FALSE)

  # # if the iterations stops on minimum or maximum, return the last data
  # if (k == miniter || (k == maxiter && running) || maxiter == 1) {
  ###### ALWAYS RETURN THE LAST DATA. THIS WAS A BUG, REMAINING AFTER I INDIVIDUALIZED IMPUTATION EVALUATION

  # Always return the current R data.frame. mlr3 works directly with R data,
  # so no backend frame conversion is required here.

  if (clean) gc()

  # ------------------------------------------------------------
  # Auto-Matching specifications
  # ============================================================
  if (isTRUE(matching) && isTRUE(stochastic)) {
    mtc <- 0
    for (Y in vars2impute) {
      mtc <- mtc + 1
      v.na <- dataNA[, Y]

      if ((FAMILY[mtc] == 'gaussian_integer') | (FAMILY[mtc] == 'quasibinomial')) {
        if (debug) md.log(paste("matching", Y), section="paragraph")

        matchedVal <- matching(
          imputed = data[v.na, Y],
          observed = unique(data[!v.na, Y])
        )
        #message(matchedVal)
        if (!is.null(matchedVal)) data[v.na, Y] <- matchedVal
        else {
          md.log("matching failed", section="paragraph", trace=FALSE)
        }
      }
    }
  }

  # ------------------------------------------------------------
  # Revert ordinal transformation
  # ============================================================
  if (!ignore.rank) {
    data[, orderedCols] <-  revert(data[, orderedCols, drop = FALSE], mem)
  }

  # Remove derived multilevel predictors before returning the completed data.
  if (!is.null(hierarchy)) {
    multilevel_variables <- attr(
      data,
      "mlim.multilevel.variables"
    )

    if (!is.null(multilevel_variables) &&
        length(multilevel_variables) > 0L) {
      data <- data[
        , setdiff(names(data), multilevel_variables),
        drop = FALSE
      ]
    }

    attr(data, "mlim.hierarchy") <- NULL
    attr(data, "mlim.original.variables") <- NULL
    attr(data, "mlim.multilevel.variables") <- NULL
  }

  attr(data, "metrics") <- metrics
  attr(data, error_metric) <- error

  class(data) <- c("mlim", "data.frame")
  return(dataLast=data)
}
