#' @title iterate
#' @description runs imputation iterations for different settings, both single
#'              imputation and multiple imputation.
#'
#' @details
#' Model fitting and tuning are performed through the 'mlr3' ecosystem.
#' Core task construction, learners, measures, resampling, and model evaluation
#' use 'mlr3'; hyperparameter optimization uses 'mlr3tuning' and
#' 'paradox'; and stacked ensembles use 'mlr3pipelines'.
#'
#' Learner interfaces are supplied by 'mlr3learners' and
#' 'mlr3extralearners'. Depending on the algorithms selected by the user,
#' the corresponding backend packages are loaded conditionally with
#' \code{requireNamespace()': 'glmnet' (ELNET), 'ranger' (RF),
#' 'partykit', 'sandwich', and 'coin' (CRF), 'gbm' (GBM),
#' 'xgboost' (XGB), 'lightgbm' (LGBM), 'catboost' (CAT),
#' 'nnet' (NNET), 'kernlab' (SVM), and 'kknn' (KNN). These
#' backend packages are not imported wholesale because
#' they are only required when their corresponding learner is requested.
#'
#' @importFrom utils packageVersion
#' @importFrom stats model.matrix rnorm
#' @importFrom md.log md.log
#' @importFrom memuse Sys.meminfo
#' @importFrom missRanger imputeUnivariate
#' @importFrom mlr3 TaskClassif TaskRegr lrn msr resample rsmp
#' @importFrom mlr3tuning tune tnr
#' @importFrom paradox p_dbl p_fct p_int ps to_tune
#' @importFrom mlr3pipelines GraphLearner pipeline_stacking
#'
#' @return list
#' @author E. F. Haghish
#' @keywords Internal
#' @noRd

iterate <- function(MI, dataNA, bdataNA,
                    preimputed.data, data, bdata, boot, metrics, tolerance,
                    m, k, X, Y, z, m.it,

                    # loop data
                    ITERATIONVARS, vars2impute,
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
                    verbosity, error, cpu,
                    stochastic
) {

  # Update the report
  # ============================================================
  if (debug) {
    md.log(paste0("\n", Y), section = "paragraph")
    md.log(paste("family:", FAMILY[z]), trace = FALSE)
  }
  else {
    md.log(
      paste(
        "imputing:", Y,
        " family:", FAMILY[z],
        "(RAM =", memuse::Sys.meminfo()$freeram, ")"
      ),
      trace = FALSE
    )
  }

  # Index the missing data
  # ============================================================
  v.na <- dataNA[, Y]
  if (boot) b.na <- bdataNA[, Y]

  # ============================================================
  # If predictors are NULL, randomly fill the missing values.
  # Otherwise continue with model-based imputation.
  # ============================================================
  if (length(X) == 0L) {
    if (debug) {
      md.log("uni impute", date = debug, time = debug, trace = FALSE)
    }
    data[[Y]] <- missRanger::imputeUnivariate(data[[Y]])
  }
  else {
    if (debug) {
      md.log(
        paste("X:", paste(setdiff(X, Y), collapse = ", ")),
        date = debug, time = debug, trace = FALSE
      )
      md.log(
        paste("Y:", paste(Y, collapse = ", ")),
        date = debug, time = debug, trace = FALSE
      )
    }

    predictors <- setdiff(X, Y)
    classification <- FAMILY[z] %in% c("binomial", "multinomial")

    # Build one common design matrix so factor coding is identical for
    # training data and all rows receiving predictions.
    if (boot) {
      design <- stats::model.matrix(
        ~ . - 1,
        data = rbind(
          data[, predictors, drop = FALSE],
          bdata[, predictors, drop = FALSE]
        )
      )

      dataDesign <- as.data.frame(
        design[seq_len(nrow(data)), , drop = FALSE]
      )

      bdataDesign <- as.data.frame(
        design[nrow(data) + seq_len(nrow(bdata)), , drop = FALSE]
      )

      trainDesign <- bdataDesign[which(!b.na), , drop = FALSE]
      target <- bdata[[Y]][!b.na]
    }
    else {
      design <- stats::model.matrix(
        ~ . - 1,
        data = data[, predictors, drop = FALSE]
      )

      dataDesign <- as.data.frame(design)
      bdataDesign <- NULL
      trainDesign <- dataDesign[which(!v.na), , drop = FALSE]
      target <- data[[Y]][!v.na]
    }

    training <- trainDesign
    training[["mlim_target_"]] <- target

    # Apply bootstrap and/or class-balancing weights.
    # ============================================================
    weights <- NULL

    if (boot) {
      weights <- bdata[["mlim_bootstrap_weights_column_"]][!b.na]
    }

    if (classification && autobalance) {
      if (is.null(weights)) {
        weights <- rep(1, length(target))
      }

      weighted_count <- tapply(
        weights,
        as.character(target),
        sum
      )

      if (any(!is.finite(weighted_count)) || any(weighted_count <= 0)) {
        stop(paste("class balancing for variable", Y, "failed"))
      }

      balance_weight <- sum(weighted_count) /
        (length(weighted_count) * weighted_count)

      weights <- weights * as.numeric(
        balance_weight[as.character(target)]
      )
    }

    if (!is.null(weights)) {
      training[["mlim_model_weights_column_"]] <- weights
    }

    # Construct a task for each learner. Learners supporting observation
    # weights receive weights for both fitting and performance estimation.
    # KNN does not support learner weights. It can therefore be used in
    # single imputation, where balancing weights are retained for the
    # performance measure only, but it is skipped during multiple
    # imputation because bootstrap multiplicity weights must affect fitting.
    # ============================================================
    make_task <- function(weight_mode = c("full", "measure")) {
      weight_mode <- match.arg(weight_mode)

      if (classification) {
        task <- mlr3::TaskClassif$new(
          id = Y,
          backend = training,
          target = "mlim_target_"
        )
      }
      else {
        task <- mlr3::TaskRegr$new(
          id = Y,
          backend = training,
          target = "mlim_target_"
        )
      }

      if (!is.null(weights)) {
        roles <- if (weight_mode == "full") {
          c("weights_learner", "weights_measure")
        }
        else {
          "weights_measure"
        }

        task$set_col_roles(
          "mlim_model_weights_column_",
          roles = roles
        )
      }

      task
    }

    # The regression objective is RMSE. For categorical variables, Brier
    # score is used because it evaluates probabilistic predictions and can
    # also be used for stochastic imputation.
    measure <- if (!classification) {
      mlr3::msr("regr.rmse")
    }
    else if (nlevels(target) == 2L) {
      mlr3::msr("classif.bbrier")
    }
    else {
      mlr3::msr("classif.mbrier")
    }

    # Algorithm names were validated by syntaxProcessing().
    # ============================================================
    algorithms <- unique(toupper(impute))
    use_ensemble <- "ENSEMBLE" %in% algorithms
    requested_base_algorithms <- setdiff(algorithms, "ENSEMBLE")
    base_algorithms <- requested_base_algorithms

    # The current mlr3 GBM classification wrapper is two-class only, so GBM
    # should not consume tuning budget for multinomial targets.
    if (classification &&
        nlevels(target) > 2L &&
        "GBM" %in% base_algorithms) {
      if (debug) {
        md.log(
          paste("skipping GBM for multinomial variable", Y),
          date = debug, time = debug, trace = FALSE
        )
      }
      base_algorithms <- setdiff(base_algorithms, "GBM")
    }

    # max_models and tuning_time are total tuning budgets for the current
    # variable. Ensemble stacking is evaluated after the base learners and
    # does not consume this base-learner tuning budget.
    n_algorithms <- length(base_algorithms)

    if (n_algorithms < 1L) {
      stop("no base imputation algorithm was specified")
    }

    if (is.null(max_models)) {
      eval_budget <- rep(NA_integer_, n_algorithms)
    }
    else {
      base_evals <- max_models %/% n_algorithms
      extra_evals <- max_models %% n_algorithms
      eval_budget <- rep(base_evals, n_algorithms)

      if (extra_evals > 0L) {
        eval_budget[seq_len(extra_evals)] <-
          eval_budget[seq_len(extra_evals)] + 1L
      }
    }

    runtime <- max(
      1L,
      as.integer(floor(tuning_time / n_algorithms))
    )

    fit <- NULL
    best_score <- Inf
    best_algorithm <- NULL

    # Tuned, untrained learners retained for optional stacking.
    ensemble_learners <- list()

    require_learner_packages <- function(algorithm) {
      packages <- switch(
        algorithm,
        ELNET = c("mlr3learners", "glmnet"),
        RF = c("mlr3learners", "ranger"),
        CRF = c("mlr3extralearners", "partykit", "sandwich", "coin"),
        # GBM = c("mlr3extralearners", "gbm"), #no longer supported because it doesn't do multiclass classification
        GBM = c("mlr3extralearners", "lightgbm"), #lightGBM is abbreviated as GBM
        XGB = c("mlr3learners", "xgboost"),
        CAT = c("mlr3extralearners", "catboost"),
        NNET = c("mlr3learners", "nnet"),
        SVM = c("mlr3extralearners", "kernlab"),
        KNN = c("mlr3learners", "kknn"),
        character(0)
      )

      missing_packages <- packages[
        !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
      ]

      if (length(missing_packages) > 0L) {
        stop(
          paste0(
            "'", algorithm, "' requires package(s): ",
            paste(missing_packages, collapse = ", "), "."
          )
        )
      }

      invisible(TRUE)
    }

    # Tune each requested base learner and retain the best one.
    # ============================================================
    for (algorithm_index in seq_along(base_algorithms)) {
      algorithm <- base_algorithms[algorithm_index]

      package_ok <- tryCatch(
        {
          require_learner_packages(algorithm)
          TRUE
        },
        error = function(cond) {
          message(conditionMessage(cond))
          FALSE
        }
      )

      if (!package_ok) {
        next
      }

      learner_id <- switch(
        algorithm,
        ELNET = if (classification) "classif.glmnet" else "regr.glmnet",
        RF = if (classification) "classif.ranger" else "regr.ranger",
        CRF = if (classification) "classif.cforest" else "regr.cforest",
        GBM = if (classification) "classif.gbm" else "regr.gbm",
        XGB = if (classification) "classif.xgboost" else "regr.xgboost",
        LGBM = if (classification) "classif.lightgbm" else "regr.lightgbm",
        CAT = if (classification) "classif.catboost" else "regr.catboost",
        NNET = if (classification) "classif.nnet" else "regr.nnet",
        SVM = if (classification) "classif.ksvm" else "regr.ksvm",
        KNN = if (classification) "classif.kknn" else "regr.kknn",
        stop(paste("unsupported algorithm", algorithm))
      )

      learner <- mlr3::lrn(learner_id)

      if (classification) {
        learner$predict_type <- "prob"

        if (nlevels(target) > 2L &&
            !"multiclass" %in% learner$properties) {
          message(
            paste0(
              "\nSkipping '", algorithm, "' for multinomial variable '", Y,
              "' because learner '", learner_id,
              "' does not support multiclass classification."
            )
          )
          next
        }

        if (nlevels(target) == 2L &&
            !"twoclass" %in% learner$properties) {
          message(
            paste0(
              "\nSkipping '", algorithm, "' for binary variable '", Y,
              "' because learner '", learner_id,
              "' does not support two-class classification."
            )
          )
          next
        }
      }

      supports_weights <- "weights" %in% learner$properties

      # Bootstrap multiplicity weights are part of the multiple-imputation
      # design and cannot be ignored. KNN therefore cannot compete in
      # multiple imputation with the current weighted-bootstrap implementation.
      if (boot && !supports_weights) {
        message(
          paste0(
            "\nSkipping '", algorithm, "' for variable '", Y,
            "' because this learner does not support observation weights ",
            "required by the multiple-imputation bootstrap."
          )
        )
        next
      }

      task_for_learner <- if (!is.null(weights) && !supports_weights) {
        make_task("measure")
      }
      else {
        make_task("full")
      }

      # Optional explicit tuning space. This is required for learner
      # parameters whose mlr3 type is not sufficiently specific for
      # paradox::to_tune(), and is also used where an explicit discrete
      # search space is clearer.
      search_space <- NULL

      # ----------------------------------------------------------
      # ELNET: glmnet
      # ----------------------------------------------------------
      if (algorithm == "ELNET") {
        search_space <- paradox::ps(
          alpha = paradox::p_dbl(0, 1),
          lambda = paradox::p_dbl(
            log(1e-5), log(1),
            trafo = exp
          )
        )
      }

      # ----------------------------------------------------------
      # RF: ranger
      # ----------------------------------------------------------
      else if (algorithm == "RF") {
        learner$param_set$values$mtry.ratio <-
          paradox::to_tune(0.1, 1)

        learner$param_set$values$sample.fraction <-
          paradox::to_tune(0.5, 1)

        learner$param_set$values$num.trees <-
          paradox::to_tune(200L, 1000L)

        learner$param_set$values$num.threads <- cpu
      }

      # ----------------------------------------------------------
      # CRF: conditional random forest via partykit::cforest
      # ----------------------------------------------------------
      else if (algorithm == "CRF") {
        learner$param_set$values$mtryratio <-
          paradox::to_tune(0.1, 1)

        learner$param_set$values$ntree <-
          paradox::to_tune(200L, 1000L)

        learner$param_set$values$mincriterion <-
          paradox::to_tune(0.5, 0.99)

        learner$param_set$values$minsplit <-
          paradox::to_tune(10L, 50L)

        learner$param_set$values$cores <- cpu
      }

      # ----------------------------------------------------------
      # GBM: gbm through mlr3extralearners
      # ----------------------------------------------------------
      else if (algorithm == "GBM") {
        learner$param_set$values$n.trees <-
          paradox::to_tune(50L, 500L)

        learner$param_set$values$interaction.depth <-
          paradox::to_tune(1L, 5L)

        learner$param_set$values$n.minobsinnode <-
          paradox::to_tune(5L, 20L)

        learner$param_set$values$shrinkage <-
          paradox::to_tune(1e-3, 0.2, logscale = TRUE)

        learner$param_set$values$bag.fraction <-
          paradox::to_tune(0.5, 1)

        learner$param_set$values$n.cores <- cpu

        if (classification && nlevels(target) > 2L) {
          learner$param_set$values$distribution <- "multinomial"
        }
      }

      # ----------------------------------------------------------
      # XGB: xgboost
      # ----------------------------------------------------------
      else if (algorithm == "XGB") {
        learner$param_set$values$nrounds <-
          paradox::to_tune(100L, 1000L)

        learner$param_set$values$eta <-
          paradox::to_tune(0.01, 0.3, logscale = TRUE)

        learner$param_set$values$max_depth <-
          paradox::to_tune(1L, 10L)

        learner$param_set$values$min_child_weight <-
          paradox::to_tune(1, 20)

        learner$param_set$values$subsample <-
          paradox::to_tune(0.5, 1)

        learner$param_set$values$colsample_bytree <-
          paradox::to_tune(0.5, 1)

        learner$param_set$values$gamma <-
          paradox::to_tune(0, 5)

        learner$param_set$values$nthread <- cpu
      }

      # ----------------------------------------------------------
      # LGBM: LightGBM
      # ----------------------------------------------------------
      else if (algorithm == "LGBM") {
        learner$param_set$values$num_iterations <-
          paradox::to_tune(100L, 1000L)

        learner$param_set$values$learning_rate <-
          paradox::to_tune(0.01, 0.3, logscale = TRUE)

        learner$param_set$values$num_leaves <-
          paradox::to_tune(8L, 128L)

        learner$param_set$values$min_data_in_leaf <-
          paradox::to_tune(5L, 50L)

        learner$param_set$values$feature_fraction <-
          paradox::to_tune(0.5, 1)

        learner$param_set$values$bagging_fraction <-
          paradox::to_tune(0.5, 1)

        learner$param_set$values$lambda_l1 <-
          paradox::to_tune(0, 10)

        learner$param_set$values$lambda_l2 <-
          paradox::to_tune(0, 10)

        learner$param_set$values$bagging_freq <- 1L
        learner$param_set$values$num_threads <- cpu
        learner$param_set$values$verbose <- -1L
      }

      # ----------------------------------------------------------
      # CAT: CatBoost
      # ----------------------------------------------------------
      else if (algorithm == "CAT") {
        learner$param_set$values$iterations <-
          paradox::to_tune(100L, 1000L)

        learner$param_set$values$learning_rate <-
          paradox::to_tune(0.01, 0.3, logscale = TRUE)

        learner$param_set$values$depth <-
          paradox::to_tune(3L, 10L)

        learner$param_set$values$l2_leaf_reg <-
          paradox::to_tune(1e-3, 10, logscale = TRUE)

        learner$param_set$values$random_strength <-
          paradox::to_tune(0, 2)

        learner$param_set$values$rsm <-
          paradox::to_tune(0.5, 1)

        learner$param_set$values$thread_count <- cpu
        learner$param_set$values$logging_level <- "Silent"
        learner$param_set$values$allow_writing_files <- FALSE
        learner$param_set$values$save_snapshot <- FALSE

        if (!is.null(seed)) {
          learner$param_set$values$random_seed <- as.integer(seed)
        }
      }

      # ----------------------------------------------------------
      # NNET: single-hidden-layer neural network
      # ----------------------------------------------------------
      else if (algorithm == "NNET") {
        learner$param_set$values$size <-
          paradox::to_tune(1L, 20L)

        learner$param_set$values$decay <-
          paradox::to_tune(1e-5, 1, logscale = TRUE)

        learner$param_set$values$maxit <- 200L
        learner$param_set$values$MaxNWts <- 50000L
        learner$param_set$values$trace <- FALSE
      }

      # ----------------------------------------------------------
      # SVM: kernel SVM via kernlab::ksvm, radial-basis kernel
      # ----------------------------------------------------------
      else if (algorithm == "SVM") {
        learner$param_set$values$kernel <- "rbfdot"

        learner$param_set$values$C <-
          paradox::to_tune(1e-3, 1e3, logscale = TRUE)

        learner$param_set$values$sigma <-
          paradox::to_tune(1e-4, 1, logscale = TRUE)

        if (!classification) {
          learner$param_set$values$epsilon <-
            paradox::to_tune(0.01, 0.3)
        }
      }

      # ----------------------------------------------------------
      # KNN: kknn
      # ----------------------------------------------------------
      else if (algorithm == "KNN") {
        k_upper <- max(
          1L,
          min(50L, nrow(trainDesign) - 1L)
        )

        search_space <- paradox::ps(
          k = paradox::p_int(1L, k_upper),
          distance = paradox::p_dbl(1, 3),
          kernel = paradox::p_fct(
            levels = c(
              "rectangular",
              "triangular",
              "epanechnikov",
              "gaussian",
              "optimal"
            )
          )
        )

        learner$param_set$values$scale <- TRUE
      }

      # Use the user-supplied seed in learners exposing a conventional
      # 'seed' parameter. CatBoost uses 'random_seed' and is handled above.
      if (!is.null(seed) && "seed" %in% learner$param_set$ids()) {
        learner$param_set$values$seed <- as.integer(seed)
      }

      evals <- if (is.null(max_models)) {
        NULL
      }
      else {
        eval_budget[algorithm_index]
      }

      instance <- tryCatch(
        mlr3tuning::tune(
          tuner = mlr3tuning::tnr("random_search"),
          task = task_for_learner,
          learner = learner,
          resampling = mlr3::rsmp("cv", folds = cv),
          measures = measure,
          search_space = search_space,
          term_evals = evals,
          term_time = runtime,
          store_benchmark_result = FALSE,
          store_models = FALSE
        ),
        error = function(cond) {
          message(
            paste0(
              "\nModel tuning for variable '", Y,
              "' with algorithm '", algorithm,
              "' failed:\n",
              conditionMessage(cond)
            )
          )
          NULL
        }
      )

      if (is.null(instance)) {
        next
      }

      score <- as.numeric(
        instance$result[[measure$id]][1]
      )

      if (!is.finite(score)) {
        next
      }

      # Preserve fixed learner settings such as CPU/thread arguments while
      # replacing all tuning tokens with their selected values.
      final_values <- learner$param_set$values
      tuned_values <- instance$result_learner_param_vals
      final_values[names(tuned_values)] <- tuned_values
      learner$param_set$values <- final_values

      tuned_learner <- learner$clone(deep = TRUE)
      tuned_learner$reset()

      # Only weight-compatible learners can be included in a weighted stack.
      if (is.null(weights) || supports_weights) {
        ensemble_learners[[algorithm]] <- tuned_learner$clone(deep = TRUE)
      }

      if (score < best_score) {
        trained_learner <- tuned_learner$clone(deep = TRUE)
        trained_learner$train(task_for_learner)

        fit <- trained_learner
        best_score <- score
        best_algorithm <- algorithm
      }
    }

    # ------------------------------------------------------------
    # ENSEMBLE: cross-validated stacking through mlr3pipelines
    # ------------------------------------------------------------
    if (use_ensemble && length(ensemble_learners) >= 2L) {
      ensemble_result <- tryCatch(
        {
          if (!requireNamespace("mlr3pipelines", quietly = TRUE)) {
            stop("'ENSEMBLE' requires the 'mlr3pipelines' package.")
          }

          if (!requireNamespace("mlr3learners", quietly = TRUE)) {
            stop("'ENSEMBLE' requires the 'mlr3learners' package.")
          }

          super_learner <- if (classification) {
            sl <- mlr3::lrn(
              "classif.multinom",
              predict_type = "prob",
              trace = FALSE,
              maxit = 200L,
              MaxNWts = 50000L
            )
            sl
          }
          else {
            mlr3::lrn("regr.lm")
          }

          graph <- mlr3pipelines::pipeline_stacking(
            base_learners = unname(ensemble_learners),
            super_learner = super_learner,
            method = "cv",
            folds = cv,
            use_features = FALSE
          )

          ensemble_learner <- mlr3pipelines::GraphLearner$new(
            graph = graph,
            id = paste0("mlim_ensemble_", Y),
            predict_type = if (classification) "prob" else "response"
          )

          ensemble_task <- make_task("full")

          rr <- mlr3::resample(
            task = ensemble_task,
            learner = ensemble_learner,
            resampling = mlr3::rsmp("cv", folds = cv),
            store_models = FALSE
          )

          ensemble_score <- as.numeric(
            rr$aggregate(measure)[[measure$id]]
          )

          list(
            learner = ensemble_learner,
            task = ensemble_task,
            score = ensemble_score
          )
        },
        error = function(cond) {
          message(
            paste0(
              "\nStacked ensemble for variable '", Y,
              "' failed:\n",
              conditionMessage(cond)
            )
          )
          NULL
        }
      )

      if (!is.null(ensemble_result) &&
          is.finite(ensemble_result$score) &&
          ensemble_result$score < best_score) {

        ensemble_result$learner$train(ensemble_result$task)
        fit <- ensemble_result$learner
        best_score <- ensemble_result$score
        best_algorithm <- "ENSEMBLE"
      }
    }
    else if (use_ensemble && length(ensemble_learners) < 2L) {
      message(
        paste0(
          "\nSkipping 'ENSEMBLE' for variable '", Y,
          "' because fewer than two eligible base learners were available."
        )
      )
    }

    if (is.null(fit)) {
      stop(paste("model training for variable", Y, "failed"))
    }

    if (debug && !is.null(best_algorithm)) {
      md.log(
        paste("selected algorithm:", best_algorithm),
        date = debug, time = debug, trace = FALSE
      )
    }

    # Store the CV loss in the same metric structure used by mlim.
    # For classification, the square root of the Brier score is retained
    # as the RMSE-like error used by the iterative stopping procedure.
    if (classification) {
      rmse <- sqrt(best_score)
      perf <- list(
        RMSE = rmse,
        MSE = best_score,
        MAE = NA_real_,
        RMSLE = NA_real_,
        Mean_Residual_Deviance = NA_real_,
        R2 = NA_real_,
        logloss = NA_real_,
        mean_per_class_error = NA_real_,
        AUC = NA_real_,
        pr_auc = NA_real_
      )
    }
    else {
      perf <- list(
        RMSE = best_score,
        MSE = best_score^2,
        MAE = NA_real_,
        RMSLE = NA_real_,
        Mean_Residual_Deviance = NA_real_,
        R2 = NA_real_,
        logloss = NA_real_,
        mean_per_class_error = NA_real_,
        AUC = NA_real_,
        pr_auc = NA_real_
      )
    }

    iterationMetric <- extractMetrics(
      if (is.null(bdata)) data else bdata,
      k, Y, perf, FAMILY[z]
    )

    # Update imputed values on first iteration or after improvement.
    # ============================================================
    accept <- TRUE

    if (k > 1L) {
      errPrevious <- min(
        metrics[metrics$variable == Y, error_metric],
        na.rm = TRUE
      )

      errPrevious <- errPrevious[is.finite(errPrevious)]
      checkMetric <- iterationMetric[
        iterationMetric$variable == Y,
        error_metric
      ]

      if (length(errPrevious) == 0L) {
        percentImprove <- -Inf
      }
      else {
        errPrevious <- min(errPrevious)

        if (errPrevious == 0) {
          percentImprove <- Inf
        }
        else {
          percentImprove <-
            (checkMetric - errPrevious) / errPrevious
        }
      }

      accept <- percentImprove < -tolerance
    }

    if (accept) {
      pred <- fit$predict_newdata(
        dataDesign[which(v.na), , drop = FALSE]
      )

      if (classification) {
        if (stochastic) {
          VEK <- stochasticFactorImpute(
            levels = levels(data[[Y]]),
            probMat = pred$prob[
              , levels(data[[Y]]), drop = FALSE
            ]
          )
        }
        else {
          VEK <- pred$response
        }
      }
      else {
        VEK <- pred$response

        if (stochastic) {
          VEK <- rnorm(
            n = length(VEK),
            mean = VEK,
            sd = iterationMetric[, "RMSE"]
          )
        }
      }

      data[which(v.na), Y] <- VEK

      if (boot) {
        pred <- fit$predict_newdata(
          bdataDesign[which(b.na), , drop = FALSE]
        )

        if (classification) {
          if (stochastic) {
            BEK <- stochasticFactorImpute(
              levels = levels(bdata[[Y]]),
              probMat = pred$prob[
                , levels(bdata[[Y]]), drop = FALSE
              ]
            )
          }
          else {
            BEK <- pred$response
          }
        }
        else {
          BEK <- pred$response

          if (stochastic) {
            BEK <- rnorm(
              n = length(BEK),
              mean = BEK,
              sd = iterationMetric[, "RMSE"]
            )
          }
        }

        bdata[which(b.na), Y] <- BEK
      }

      metrics <- rbind(metrics, iterationMetric)
    }
    else {
      if (debug) {
        md.log(
          "imputation was NOT improved, new values are rejected",
          date = debug, time = debug, trace = FALSE
        )
      }

      iterationMetric[
        iterationMetric$variable == Y,
        error_metric
      ] <- Inf

      metrics <- rbind(metrics, iterationMetric)
    }

    rm(fit)
    gc()
  }

  if (debug) {
    md.log(
      "iteration done",
      date = debug, time = debug, trace = FALSE
    )
  }

  # Update the predictors during the first iteration
  # ------------------------------------------------------------
  if (preimpute == "iterate" &&
      k == 1L &&
      (Y %in% allPredictors)) {

    X <- union(X, Y)

    if (debug) {
      md.log(
        "x was updated",
        date = debug, time = debug, trace = FALSE
      )
    }
  }

  # Save current state
  # ============================================================
  if (!is.null(save)) {
    if (debug) {
      md.log(
        "Saving the status",
        date = debug, time = debug, trace = FALSE
      )
    }

    multilevel_variables <- attr(
      data,
      "mlim.multilevel.variables"
    )

    if (is.null(multilevel_variables)) {
      multilevel_variables <- character(0)
    }

    savestate <- list(

      # Data
      # ----------------------------------
      MI = MI,
      dataNA = dataNA,
      preimputed.data = preimputed.data,
      data = data[
        , setdiff(names(data), multilevel_variables),
        drop = FALSE
      ],
      metrics = metrics,
      mem = mem,
      orderedCols = orderedCols,

      # Loop data
      # ----------------------------------
      m = m,
      k = k,
      z = z,
      X = setdiff(X, multilevel_variables),
      Y = Y,
      m.it = m.it,
      vars2impute = vars2impute,
      FAMILY = FAMILY,
      allPredictors = setdiff(
        allPredictors,
        multilevel_variables
      ),

      # Settings
      # ----------------------------------
      ITERATIONVARS = ITERATIONVARS,
      preimpute = preimpute,
      impute = impute,
      ignore = ignore,
      autobalance = autobalance,
      hierarchy = hierarchy,
      stochastic = stochastic,
      save = save,
      maxiter = maxiter,
      miniter = miniter,
      cv = cv,
      tuning_time = tuning_time,
      max_models = max_models,
      matching = matching,
      ignore.rank = ignore.rank,
      seed = seed,
      verbosity = verbosity,
      verbose = verbose,
      debug = debug,
      report = report,
      error_metric = error_metric,
      tolerance = tolerance,
      error = error,
      cpu = cpu,
      sleep = sleep,

      # Save the package version used for the imputation
      pkg = packageVersion("mlim")
    )

    class(savestate) <- "mlim"
    saveRDS(savestate, save)
  }

  Sys.sleep(sleep)

  if (debug) {
    md.log(
      "saving done! return to the loop",
      date = debug, time = debug, trace = FALSE
    )
  }

  gc()

  return(
    list(
      X = X,
      metrics = metrics,
      iterationvars = ITERATIONVARS,
      data = data,
      bdata = bdata
    )
  )
}
