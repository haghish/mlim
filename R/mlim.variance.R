#' @title Estimate conditional prediction variance for numeric imputations
#' @description Internal two-stage cross-fitted variance model for the mlr3
#'   regression learners. This does not estimate GPBoost/HML uncertainty.
#' @keywords internal
#' @noRd
#'
#' @details
#' The selected mean learner is re-fitted on K-1 folds and predicts the
#' held-out fold. A separate ranger regression models squared out-of-fold
#' residuals as a function of the original design matrix. Its out-of-bag
#' predictions are used to calibrate the predicted variance so that its
#' weighted average matches the original selected learner's CV RMSE squared.
#' The method estimates conditional *prediction MSE*, not necessarily the
#' irreducible conditional noise variance. This distinction matters for MI.
#'
#' Bootstrap multiplicity weights, if supplied, affect the out-of-fold
#' learner refits (through mlr3 weights_learner) and ranger's observation
#' sampling probabilities. Ranger case weights are NOT exact frequency
#' weights for its split criterion, so bootstrap equivalence is approximate.
#' No missing outcomes, current imputations of the target, or prediction-set
#' outcomes are used when constructing variance targets.
mlim.variance.fit <- function(mean_learner, task, x, weights = NULL,
                              cv = 5L, cpu = 1L, global_sd, seed = NULL) {
  if (!requireNamespace("mlr3", quietly = TRUE) ||
      !requireNamespace("ranger", quietly = TRUE)) {
    stop("Conditional variance estimation requires both 'mlr3' and 'ranger'.", call. = FALSE)
  }
  if (!inherits(task, "TaskRegr")) stop("The conditional variance task must be a regression task.", call. = FALSE)
  if (!is.data.frame(x)) x <- as.data.frame(x)
  n <- nrow(x)
  if (n < 20L || ncol(x) == 0L || length(task$row_ids) != n)
    stop("At least 20 observed training rows and one predictor are required.", call. = FALSE)
  if (!is.numeric(global_sd) || length(global_sd) != 1L ||
      !is.finite(global_sd) || global_sd < 0)
    stop("The selected regression learner has no finite CV RMSE.", call. = FALSE)
  if (anyNA(x) || any(!vapply(x, is.numeric, logical(1))))
    stop("Conditional variance requires a complete numeric model matrix.", call. = FALSE)
  if (anyDuplicated(names(x)) || ".mlim_conditional_variance_response_" %in% names(x))
    stop("Conditional variance design matrix has duplicate or reserved column names.", call. = FALSE)
  if (!is.null(weights)) {
    if (length(weights) != n || any(!is.finite(weights)) || any(weights <= 0))
      stop("Invalid positive bootstrap observation multiplicities.", call. = FALSE)
    weights <- as.numeric(weights)
  }
  if (global_sd == 0) {
    # A genuine zero-error model needs no residual draw. Do not fit a forest
    # just to inject spurious noise into a deterministic regression.
    return(list(zero = TRUE, global_sd = 0))
  }
  cv <- min(as.integer(cv), n)
  if (is.na(cv) || cv < 2L) stop("Conditional variance needs at least two CV folds.", call. = FALSE)
  if (is.null(cpu) || !is.finite(cpu) || cpu < 1) cpu <- 1L
  cpu <- as.integer(cpu)

  # Additional CV is necessary because mlr3tuning does not retain row-wise
  # out-of-fold predictions for the selected tuned model.
  learner_oof <- mean_learner$clone(deep = TRUE)
  learner_oof$reset()
  rr <- mlr3::resample(task, learner_oof,
                      mlr3::rsmp("cv", folds = cv),
                      store_models = FALSE, store_backends = FALSE)
  p <- rr$prediction()
  ids <- task$row_ids
  loc <- match(p$row_ids, ids)
  if (length(loc) != n || anyNA(loc) || anyDuplicated(loc))
    stop("Cross-fitting did not produce one prediction for every observed training row.", call. = FALSE)
  y <- as.numeric(task$truth(rows = p$row_ids))
  y_hat <- as.numeric(p$response)
  if (length(y_hat) != n || any(!is.finite(y_hat)) || any(!is.finite(y)))
    stop("Cross-fitted mean predictions were not finite.", call. = FALSE)
  sq_resid <- rep(NA_real_, n)
  sq_resid[loc] <- (y - y_hat)^2
  rm(rr, p, learner_oof, y, y_hat, loc)
  if (any(!is.finite(sq_resid)) || all(sq_resid == 0))
    stop("Cross-fitted squared residuals were nonfinite or all zero.", call. = FALSE)

  # Keep the variance model deliberately conservative: reasonably large leaves
  # and 10% shrinkage of raw predictions toward the scalar CV variance.
  # This is NOT a second hyperparameter tuning procedure.
  response_name <- ".mlim_conditional_variance_response_"
  ranger_data <- x
  ranger_data[[response_name]] <- sq_resid
  forest <- ranger::ranger(
    dependent.variable.name = response_name,
    data = ranger_data,
    num.trees = 250L,
    min.node.size = max(10L, as.integer(floor(n / 25L))),
    num.threads = cpu,
    case.weights = weights,
    seed = if (is.null(seed)) NULL else as.integer(seed),
    write.forest = TRUE,
    keep.inbag = FALSE,
    importance = "none"
  )
  # ranger's OOB prediction is computed without using the target of that row
  # for the given tree; it is more defensible for calibration than in-sample
  # variance-model predictions (though still an approximation).
  oob <- as.numeric(forest$predictions)
  if (length(oob) != n || any(!is.finite(oob)))
    stop("Variance forest did not produce finite OOB predictions.", call. = FALSE)
  target_variance <- global_sd^2
  raw_oob <- pmax(0, oob)
  shrunk_oob <- 0.9 * raw_oob + 0.1 * target_variance
  average_oob <- if (is.null(weights)) mean(shrunk_oob) else stats::weighted.mean(shrunk_oob, weights)
  if (!is.finite(average_oob) || average_oob <= 0)
    stop("Conditional variance calibration denominator is invalid.", call. = FALSE)
  list(
    forest = forest,
    scale = target_variance / average_oob,
    target_variance = target_variance,
    global_sd = global_sd,
    zero = FALSE
  )
}

#' @keywords internal
#' @noRd
mlim.variance.predict_sd <- function(model, newdata, cpu = 1L) {
  if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
  if (nrow(newdata) == 0L) return(numeric(0))
  if (isTRUE(model$zero)) return(rep(0, nrow(newdata)))
  if (is.null(cpu) || !is.finite(cpu) || cpu < 1) cpu <- 1L
  variance_raw <- as.numeric(stats::predict(
    model$forest, data = newdata, num.threads = as.integer(cpu)
  )$predictions)
  if (length(variance_raw) != nrow(newdata) || any(!is.finite(variance_raw)))
    stop("Conditional variance predictions are not finite.", call. = FALSE)
  # Blend towards global RMSE^2; rescale based on weighted OOB predictions.
  variance_conditional <- (0.9 * pmax(0, variance_raw) +
                           0.1 * model$target_variance) * model$scale
  sqrt(pmax(variance_conditional, model$target_variance * 1e-4))
}
