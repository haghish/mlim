#' @title Hierarchical machine-learning backend for mlim (HML)
#' @description Fits a GPBoost boosted-tree model with explicit nested random
#'   intercepts. Used internally by iterate(); no wide reshaping is performed.
#' @details
#'   - HML uses GPBoost's Gaussian or Bernoulli-logit likelihood. Ordinal targets
#'     transformed into numeric by mlim are handled as Gaussian approximations.
#'     Nominal factors with three or more levels are NOT supported by HML.
#'   - Hierarchy IDs enter as grouped random effects, NOT as fixed-effect dummies.
#'   - CV follows the missingness pattern: within-cluster holdouts for ordinary
#'     item nonresponse; nested-unit or top-group holdouts when most missing
#'     records belong to entirely unobserved units/groups. This distinction
#'     matters because the conditional prediction target is different.
#'   - Integer bootstrap multiplicities are expanded into repeated training rows
#'     after each cross-validation split. HML does not rebalance outcome classes:
#'     doing so would distort conditional probabilities used for imputation.
#'   - For stochastic imputation, HML draws from GPBoost predictive covariance
#'     matrices for Gaussian responses, or Gaussian latent random-effect
#'     distributions followed by Bernoulli draws for binary responses. This
#'     replaces GPBoost's sample_posterior option, which does not support
#'     multiple grouped random-effect levels. Nested schools are independent
#'     conditional on fitted parameters; each school is sampled jointly.
#'     Covariance parameters and boosted-tree predictions are treated as fixed
#'     within each draw; this is not guaranteed proper multiple imputation.
#' @keywords internal
#' @noRd

mlim.hml.groups <- function(data, hierarchy) {
  if (is.null(hierarchy) || !length(hierarchy) ||
      !all(hierarchy %in% names(data))) {
    stop("HML requires valid 'hierarchy' columns in the data.")
  }
  if (anyNA(data[, hierarchy, drop = FALSE])) {
    stop("HML does not accept missing hierarchy identifiers.")
  }
  # Encode NESTED group IDs cumulatively. Child '1' in school A must not
  # share its random effect with child '1' in school B.
  key <- lapply(data[hierarchy], as.character)
  out <- lapply(seq_along(hierarchy), function(j) {
    do.call(paste, c(key[seq_len(j)], list(sep = "\037")))
  })
  ans <- do.call(cbind, out)
  colnames(ans) <- hierarchy
  ans
}

# A random-intercept component with a single level is confounded with an
# intercept, while an entirely singleton level is confounded with independent
# observation noise. Neither is estimable as a meaningful grouping variance.
# Detect this using observed target rows, rather than all originally supplied
# rows, so HML uses the SAME effect columns in every CV fit and prediction.
mlim.hml.active_columns <- function(group) {
  if (!is.matrix(group))
    group <- as.matrix(group)
  n <- nrow(group)
  which(vapply(seq_len(ncol(group)), function(j) {
    groups <- length(unique(group[, j]))
    groups >= 2L && groups < n
  }, logical(1)))
}

# Report unidentifiable random-intercept levels once at the start of an mlim
# call, using the ORIGINAL observed/missing mask for every target variable.
# This mirrors fit()'s active-columns rule on its observed training rows.
# Each bootstrap/target fit still recomputes active levels independently;
# only its repetitive console message is suppressed, NOT that check.
mlim.hml.report_hierarchy <- function(data,
                                      hierarchy,
                                      variables,
                                      na_mask,
                                      variable_levels = NULL) {
  if (!length(variables))
    return(invisible(NULL))
  if (is.null(dim(na_mask)) ||
      !all(variables %in% colnames(na_mask))) {
    stop("HML hierarchy report requires a missingness mask for every target.",
         call. = FALSE)
  }

  groups <- mlim.hml.groups(data, hierarchy)
  if (nrow(groups) != nrow(na_mask)) {
    stop("HML hierarchy report: data and missingness mask have different row counts.",
         call. = FALSE)
  }

  dropped_by_target <- lapply(variables, function(target) {
    declared <- if (is.null(variable_levels))
      character(0)
    else
      names(variable_levels)[vapply(variable_levels, function(v)
        target %in% v, logical(1))]
    available <- if (length(declared))
      hierarchy[seq_len(match(declared[1L], hierarchy) - 1L)]
    else
      hierarchy
    # Omitted lower-level effects are intentional, not unidentifiable.
    if (!length(available))
      return(character(0))
    observed <- !as.logical(na_mask[, target])
    target_groups <- groups[observed, available, drop = FALSE]
    if (length(declared))
      target_groups <- unique(target_groups)
    active <- mlim.hml.active_columns(as.matrix(target_groups))
    setdiff(available, colnames(target_groups)[active])
  })
  names(dropped_by_target) <- variables

  count <- vapply(hierarchy, function(level) {
    sum(vapply(dropped_by_target, function(dropped)
      level %in% dropped, logical(1)))
  }, integer(1))
  excluded <- hierarchy[count > 0L]
  if (!length(excluded))
    return(invisible(dropped_by_target))

  # Preserve the familiar one-line message when all targets exclude the same
  # hierarchy level(s). Otherwise identify partial exclusions in the SAME
  # message so users can distinguish target-specific identifiability.
  if (all(count[excluded] == length(variables))) {
    message(
      "HML: excluding unidentifiable random-effect level(s): ",
      paste(excluded, collapse = ", "),
      "."
    )
  } else {
    labels <- vapply(excluded, function(level) {
      targets <- variables[vapply(dropped_by_target, function(dropped)
        level %in% dropped, logical(1))]
      if (length(targets) == length(variables))
        return(level)
      sprintf(
        "%s (%d/%d targets: %s)",
        level,
        length(targets),
        length(variables),
        paste(targets, collapse = ", ")
      )
    }, character(1))
    message(
      "HML: initial hierarchy check - excluding unidentifiable ",
      "random-effect level(s): ",
      paste(labels, collapse = "; "),
      "."
    )
  }

  invisible(dropped_by_target)
}

# Choose validation that resembles the target's observed missingness pattern.
# This heuristic is appropriate for baseline experimentation; users should
# benchmark it against their true missingness mechanism (including MAR/MNAR).
mlim.hml.cv_strategy <- function(data, na_mask, hierarchy) {
  if (!length(hierarchy))
    return("within")
  if (!any(na_mask))
    return("within")
  group <- mlim.hml.groups(data, hierarchy)
  has_observed <- !na_mask
  active <- mlim.hml.active_columns(group[has_observed, , drop = FALSE])
  if (!length(active))
    return("within")  # fit() will give an informative error.
  group <- group[, active, drop = FALSE]
  top <- group[, 1L]
  bottom <- group[, ncol(group)]
  missing_new_top <- mean(!(top[na_mask] %in% unique(top[has_observed])))
  missing_new_bottom <- mean(!(bottom[na_mask] %in% unique(bottom[has_observed])))
  if (missing_new_top >= 0.5)
    return("top")
  if (ncol(group) > 1L &&
      missing_new_bottom >= 0.5)
    return("bottom")
  "within"
}

mlim.hml.fit_one <- function(x,
                             y,
                             group,
                             weight,
                             configuration,
                             binary,
                             cpu) {
  if (!requireNamespace("gpboost", quietly = TRUE)) {
    stop("HML requires the optional 'gpboost' R package.")
  }
  if (nrow(x) != length(y) || nrow(group) != length(y)) {
    stop("HML input dimensions do not match.")
  }
  # GPBoost versions have differed in how booster/Dataset weights interact
  # with GPModel weights. In particular, some builds raise the fatal error
  # "Weights need to be provided to the GPModel() constructor" even when
  # the R constructor receives weights. HML only receives integer bootstrap
  # MULTIPLICITIES, not general sampling/inverse-probability weights.
  #
  # Expand these frequencies into rows rather than supplying weights to
  # GPBoost. This is the actual bootstrap resample: repeats have independent
  # observation errors but share the same grouped random effects. Crucially,
  # mlim.hml.fit() creates CV folds over UNIQUE original records FIRST and
  # calls this function separately for each training fold. Duplicated rows
  # therefore cannot leak into that fold's validation set.
  #
  # Non-integer weights are deliberately rejected instead of silently rounded:
  # those represent a different likelihood/estimand and need explicit support.
  if (!is.null(weight)) {
    weight <- as.numeric(weight)
    if (length(weight) != length(y) || anyNA(weight) ||
        any(!is.finite(weight)) || any(weight <= 0)) {
      stop("HML requires positive, finite bootstrap multiplicity weights.")
    }
    if (any(abs(weight - round(weight)) > 1e-8) ||
        any(weight > .Machine$integer.max)) {
      stop(
        "HML requires integer bootstrap multiplicities; general sampling weights are not implemented."
      )
    }
    # Avoid unnecessary allocation when all rows occur exactly once.
    if (any(weight != 1)) {
      row_index <- rep.int(seq_along(y), times = as.integer(round(weight)))
      x <- x[row_index, , drop = FALSE]
      y <- y[row_index]
      group <- group[row_index, , drop = FALSE]
    }
  }
  likelihood <- if (binary)
    "bernoulli_logit"
  else
    "gaussian"
  # Hold the GPModel seed constant on a numerical retry. GPBoost's default
  # covariance solver may select conjugate gradients for nested grouped random
  # effects. An unstable CG iteration is a *solver* failure, not evidence that
  # the input data have NA/Inf. Cholesky is supported but can use more memory,
  # so it is only attempted if the default solver specifically fails in CG.
  gp_seed <- as.integer(sample.int(.Machine$integer.max, size = 1L))
  params <- list(
    learning_rate = configuration$learning_rate,
    max_depth = as.integer(configuration$max_depth),
    num_leaves = as.integer(configuration$num_leaves),
    min_data_in_leaf = as.integer(configuration$min_data_in_leaf),
    lambda_l2 = configuration$lambda_l2,
    num_threads = as.integer(cpu),
    verbosity = -1L
  )
  fit_solver <- function(matrix_method) {
    if (ncol(group) == 0L) {
      params$objective <- if (binary)
        "binary"
      else
        "regression"
      return(
        gpboost::gpboost(
          data = x,
          label = as.numeric(y),
          params = params,
          nrounds = as.integer(configuration$nrounds),
          verbose = 0L
        )
      )
    }
    gp <- gpboost::GPModel(
      group_data = group,
      likelihood = likelihood,
      matrix_inversion_method = matrix_method,
      num_parallel_threads = as.integer(cpu),
      seed = gp_seed
    )
    # No 'weight' argument is passed: row replication above represents the
    # same integer bootstrap frequencies in the trees and random effects.
    gpboost::gpboost(
      data = x,
      label = as.numeric(y),
      gp_model = gp,
      params = params,
      nrounds = as.integer(configuration$nrounds),
      verbose = 0L
    )
  }
  first <- tryCatch(
    fit_solver("default"),
    error = function(e)
      e
  )
  if (!inherits(first, "error"))
    return(first)
  original_error <- conditionMessage(first)
  if (!grepl("conjugate.gradient|conjugate gradient",
             original_error,
             ignore.case = TRUE)) {
    stop(first)  # Preserve non-CG errors for the existing CV diagnostics.
  }
  message(
    "HML: GPBoost conjugate-gradient solver failed; retrying with ",
    "Cholesky (higher memory use)."
  )
  second <- tryCatch(
    fit_solver("cholesky"),
    error = function(e)
      e
  )
  if (inherits(second, "error")) {
    stop(
      "HML fitting failed with both GPBoost covariance solvers. ",
      "Default solver: ",
      original_error,
      " | Cholesky fallback: ",
      conditionMessage(second),
      call. = FALSE
    )
  }
  second
}

# Draw a multivariate Gaussian vector with GPBoost's fitted predictive
# mean/covariance. Using the *full* covariance instead of separate SDs is
# important: a single school/child random effect correlates missing values.
# A small negative eigenvalue may arise from finite-precision calculations;
# large negative eigenvalues indicate an invalid predictive covariance.
mlim.hml.draw_gaussian <- function(mu, covariance) {
  mu <- as.numeric(mu)
  n <- length(mu)
  if (!n ||
      any(!is.finite(mu)))
    stop("HML: invalid predictive mean.")
  if (n == 1L && length(covariance) == 1L) {
    v <- as.numeric(covariance)
    if (!is.finite(v) || v < -1e-8 * max(1, abs(v))) {
      stop("HML: invalid univariate predictive variance.")
    }
    return(stats::rnorm(1L, mu, sqrt(max(v, 0))))
  }
  if (!is.matrix(covariance) ||
      !identical(dim(covariance), c(n, n)) ||
      any(!is.finite(covariance))) {
    stop("HML: GPBoost did not return a valid predictive covariance matrix.")
  }
  covariance <- (covariance + t(covariance)) / 2
  # Cholesky is more efficient than full eigendecomposition and directly
  # preserves the fitted covariance whenever it is positive definite.
  factor <- tryCatch(
    chol(covariance),
    error = function(e)
      NULL
  )
  if (!is.null(factor)) {
    return(mu + as.numeric(t(factor) %*% stats::rnorm(n)))
  }
  eigen_cov <- eigen(covariance, symmetric = TRUE)
  tolerance <- 1e-8 * max(1, max(abs(eigen_cov$values)))
  if (min(eigen_cov$values) < -tolerance) {
    stop(
      "HML: predictive covariance is not positive semidefinite; ",
      "inspect the fitted GPBoost random-effect model."
    )
  }
  mu + as.numeric(eigen_cov$vectors %*%
                    (sqrt(pmax(
                      eigen_cov$values, 0
                    )) * stats::rnorm(n)))
}

mlim.hml.prediction <- function(model,
                                x,
                                group,
                                binary,
                                stochastic = FALSE,
                                factor_levels = NULL) {
  n <- nrow(x)
  if (!n)
    return(if (binary)
      factor(character(), levels = factor_levels)
      else
        numeric())
  if (!is.matrix(group))
    group <- as.matrix(group)
  if (nrow(group) != n || ncol(group) < 1L) {
    stop("HML: prediction group dimensions do not match the predictor matrix.")
  }

  if (!isTRUE(stochastic)) {
    p <- stats::predict(
      model,
      data = x,
      group_data_pred = group,
      pred_latent = FALSE
    )
    values <- as.numeric(p$response_mean)
    if (length(values) != n || any(!is.finite(values))) {
      stop("HML returned invalid response means.")
    }
    if (binary)
      values <- as.integer(values >= 0.5)
  } else {
    # GPBoost itself cannot sample_posterior() for two or more grouped random
    # effects (the common school/child case). Its covariance predictions *are*
    # supported. We make the draw on the R side, preserving correlation among
    # all missing records in a school and its nested children.
    #
    # The grouping identifiers were cumulatively encoded by mlim.hml.groups(),
    # so distinct top-level groups have no shared random effects. Conditional
    # on model parameters their predictive covariance is block diagonal.
    # Splitting by school also avoids allocating an n_missing^2 matrix.
    values <- if (binary)
      integer(n)
    else
      numeric(n)
    blocks <- split(seq_len(n), group[, 1L], drop = TRUE)
    for (idx in blocks) {
      # Unlike silently replacing joint draws with independent noise, fail
      # explicitly when a single cluster would require excessive dense memory.
      if (length(idx) > 3000L) {
        stop(
          "HML: more than 3000 missing observations belong to one top-level ",
          "cluster; joint covariance sampling needs a scalable algorithm."
        )
      }
      p <- stats::predict(
        model,
        data = x[idx, , drop = FALSE],
        group_data_pred = group[idx, , drop = FALSE],
        pred_latent = binary,
        predict_cov_mat = TRUE,
        sample_posterior = FALSE
      )
      if (binary) {
        # The binary-response distribution is not Gaussian. GPBoost's
        # Laplace-approximated random-effect covariance concerns the latent
        # *logit*, not the Bernoulli response. Draw logits jointly, then draw
        # Bernoulli observations conditional on those correlated logits.
        fixed <- as.numeric(p$fixed_effect)
        random <- as.numeric(p$random_effect_mean)
        if (length(fixed) != length(idx) ||
            length(random) != length(idx)) {
          stop("HML: invalid binary latent predictions from GPBoost.")
        }
        eta <- mlim.hml.draw_gaussian(fixed + random, p$random_effect_cov)
        values[idx] <- stats::rbinom(length(idx),
                                     size = 1L,
                                     prob = stats::plogis(eta))
      } else {
        # Gaussian response covariance includes the estimated residual
        # variance. Do NOT add cross-validation RMSE on top of this draw.
        values[idx] <- mlim.hml.draw_gaussian(p$response_mean, p$response_var)
      }
    }
  }
  if (binary) {
    if (length(factor_levels) != 2L)
      stop("HML requires exactly two factor levels.")
    factor(factor_levels[values + 1L], levels = factor_levels)
  } else {
    values
  }
}

mlim.hml.fit <- function(x,
                         y,
                         hierarchy_data,
                         weights = NULL,
                         max_models = 6L,
                         tuning_time = 300,
                         cv = 5L,
                         cpu = 1L,
                         seed = NULL,
                         cv_strategy = c("within", "bottom", "top")) {
  if (!requireNamespace("gpboost", quietly = TRUE)) {
    stop("Install 'gpboost' to use the HML engine.")
  }
  if (nrow(x) != length(y) || nrow(hierarchy_data) != length(y)) {
    stop("HML training dimensions do not match.")
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop("HML requires a finite, fully preimputed numeric predictor matrix.")
  }
  binary <- is.factor(y)
  if (binary && nlevels(y) != 2L) {
    stop(
      "HML currently supports Gaussian outcomes and binary factors, not multinomial variables."
    )
  }
  if (!binary &&
      !is.numeric(y))
    stop("HML requires a numeric or binary-factor target.")
  if (anyNA(y) ||
      length(y) < 10L)
    stop("HML requires at least ten observed target values.")
  if (binary &&
      any(table(y) < 2L))
    stop("HML requires at least two observed values in each class.")
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  if (!ncol(x)) {
    x <- matrix(
      0,
      nrow = nrow(x),
      ncol = 1L,
      dimnames = list(NULL, "mlim_dummy")
    )
  }
  zero_level <- ncol(hierarchy_data) == 0L
  if (zero_level) {
    group <- matrix(character(0), nrow = nrow(x), ncol = 0L)
    active_hierarchy <- character(0)
  } else {
    all_groups <- mlim.hml.groups(hierarchy_data, names(hierarchy_data))
    active <- mlim.hml.active_columns(all_groups)
    if (!length(active)) {
      stop(
        "HML cannot estimate random effects: every grouping variable is ",
        "constant or has one observation per group among observed target rows. ",
        "Remove observation-level IDs from 'hierarchy', and verify repeated ",
        "measurements or clusters before fitting.",
        call. = FALSE
      )
    }
    group <- all_groups[, active, drop = FALSE]
    active_hierarchy <- colnames(group)
  }
  # Group identifiability remains target-specific (and can change in a
  # bootstrap sample), even though the initial diagnostic is printed once by
  # mlim.hml.report_hierarchy() at the beginning of the imputation run.
  weight <- if (is.null(weights))
    NULL
  else
    as.numeric(weights)
  if (!is.null(weight) && (length(weight) != length(y) ||
                           any(!is.finite(weight)) ||
                           any(weight <= 0))) {
    stop("Invalid HML observation weights.")
  }
  label <- if (binary)
    as.integer(y) - 1L
  else
    as.numeric(y)

  # Conservative, bounded random search; actual fitting can be expensive.
  # Initial candidate is deliberately shallow/regularized for small surveys.
  n <- nrow(x)
  configs <- list(
    list(
      learning_rate = 0.05,
      max_depth = 2L,
      num_leaves = 7L,
      min_data_in_leaf = max(2L, min(10L, n %/% 10L)),
      lambda_l2 = 1,
      nrounds = 100L
    )
  )
  budget <- if (is.null(max_models))
    6L
  else
    min(6L, as.integer(max_models))
  # mlim() already seeds the entire run. Do not re-seed every variable or
  # every imputation: those must receive genuinely different random draws.
  if (budget > 1L) {
    for (j in seq_len(budget - 1L)) {
      depth <- sample(c(1L, 2L, 3L, 4L), 1L)
      configs[[length(configs) + 1L]] <- list(
        learning_rate = sample(c(0.025, 0.05, 0.1), 1L),
        max_depth = depth,
        num_leaves = min(sample(c(
          3L, 7L, 15L, 31L
        ), 1L), 2^depth),
        min_data_in_leaf = min(sample(c(5L, 10L, 20L), 1L), max(2L, n %/% 5L)),
        lambda_l2 = sample(c(0.1, 1, 5), 1L),
        nrounds = sample(c(50L, 100L, 175L), 1L)
      )
    }
  }

  cv_strategy <- match.arg(cv_strategy)
  # Within-cluster folds mimic scattered item nonresponse, allowing random
  # effects to learn from other observed occasions in the same cluster.
  # Bottom / top folds evaluate imputing entirely unobserved lower / upper
  # hierarchy units. This is intentionally NOT one universal CV design.
  block <- if (zero_level)
    seq_len(n)
  else
    if (cv_strategy == "top")
      group[, 1L]
  else
    group[, ncol(group)]
  units <- unique(block)
  if (zero_level)
    cv_strategy <- "within"
  ng <- length(units)
  if (cv_strategy != "within" && ng < 3L)
    stop("HML blocked CV requires at least three observed groups at the blocking level.")
  nfolds <- if (cv_strategy == "within")
    min(as.integer(cv), n)
  else
    min(as.integer(cv), ng)
  if (nfolds < 2L)
    stop("HML requires at least two validation folds.")

  # Randomized fold generation with stratification within hierarchy units.
  # Retry if a fold leaves the binary training sample with only one class.
  for (attempt in seq_len(40L)) {
    if (cv_strategy == "within") {
      folds <- integer(n)
      for (unit in units) {
        eligible <- which(block == unit)
        classes <- if (binary)
          sort(unique(label[eligible]))
        else
          NA_real_
        for (cls in classes) {
          idx <- if (binary)
            eligible[label[eligible] == cls]
          else
            eligible
          offset <- sample.int(nfolds, 1L) - 1L
          numbers <- ((seq_along(idx) - 1L + offset) %% nfolds) + 1L
          # Index-based shuffling avoids sample(x) interpreting a length-one numeric x as 1:x.
          folds[idx] <- numbers[sample.int(length(numbers))]
        }
      }
    } else {
      shuffled <- sample(units, ng, replace = FALSE)
      group_folds <- setNames(rep(seq_len(nfolds), length.out = ng), shuffled)
      folds <- unname(group_folds[block])
    }
    valid_partition <- all(tabulate(folds, nbins = nfolds) > 0L)
    if (binary)
      valid_partition <- valid_partition &&
      all(vapply(seq_len(nfolds), function(f) {
        length(unique(label[folds != f])) == 2L
      }, logical(1)))
    if (valid_partition)
      break
  }
  if (!valid_partition) {
    stop("No valid HML CV partition: check cluster sizes and rare-class distribution.")
  }
  started <- proc.time()[[3L]]
  best <- Inf
  best_config <- NULL
  success <- 0L
  diagnostics <- character(0)
  for (configuration_index in seq_along(configs)) {
    config <- configs[[configuration_index]]
    if (success > 0L &&
        proc.time()[[3L]] - started >= tuning_time)
      break
    losses <- numeric(nfolds)
    ok <- TRUE
    for (f in seq_len(nfolds)) {
      valid <- which(folds == f)
      train <- which(folds != f)
      if (length(valid) == 0L || length(train) < 10L ||
          (binary && length(unique(label[train])) < 2L)) {
        ok <- FALSE
        break
      }
      fitted <- tryCatch(
        mlim.hml.fit_one(
          x[train, , drop = FALSE],
          label[train],
          group[train, , drop = FALSE],
          if (is.null(weight))
            NULL
          else
            weight[train],
          config,
          binary,
          cpu
        ),
        error = function(e) {
          diagnostics <<- c(
            diagnostics,
            sprintf(
              "candidate %d, fold %d (fitting): %s",
              configuration_index,
              f,
              conditionMessage(e)
            )
          )
          NULL
        }
      )
      if (is.null(fitted)) {
        ok <- FALSE
        break
      }
      p <- tryCatch({
        if (zero_level)
          as.numeric(stats::predict(fitted, data = x[valid, , drop = FALSE]))
        else
          as.numeric(
            stats::predict(
              fitted,
              data = x[valid, , drop = FALSE],
              group_data_pred = group[valid, , drop = FALSE],
              pred_latent = FALSE
            )$response_mean
          )
      }, error = function(e) {
        diagnostics <<- c(
          diagnostics,
          sprintf(
            "candidate %d, fold %d (prediction): %s",
            configuration_index,
            f,
            conditionMessage(e)
          )
        )
        NULL
      })
      if (is.null(p) ||
          length(p) != length(valid) || any(!is.finite(p))) {
        if (!is.null(p))
          diagnostics <- c(
            diagnostics,
            sprintf(
              "candidate %d, fold %d: invalid GPBoost prediction length or values",
              configuration_index,
              f
            )
          )
        ok <- FALSE
        break
      }
      losses[f] <- if (binary) {
        mean((pmin(1, pmax(0, p)) - label[valid])^2)
      } else {
        mean((as.numeric(p) - label[valid])^2)
      }
    }
    if (!ok)
      next
    # Weight by number of held-out rows, not the number of clusters.
    fold_n <- tabulate(folds, nbins = nfolds)
    score <- sum(losses * fold_n) / sum(fold_n)
    if (score < best) {
      best <- score
      best_config <- config
    }
    success <- success + 1L
  }
  if (is.null(best_config)) {
    details <- if (length(diagnostics)) {
      paste(utils::head(unique(diagnostics), 5L), collapse = "\n  ")
    } else {
      "No model completed cross-validation; check fold sizes and runtime."
    }
    stop(
      sprintf(
        "HML cross-validation failed (strategy=%s; hierarchy=%s).\n",
        cv_strategy,
        paste(active_hierarchy, collapse = ", ")
      ),
      "Original GPBoost diagnostic(s):\n  ",
      details,
      call. = FALSE
    )
  }
  final <- tryCatch(
    mlim.hml.fit_one(x, label, group, weight, best_config, binary, cpu),
    error = function(e)
      stop(
        "HML final GPBoost fit failed after successful CV: ",
        conditionMessage(e),
        call. = FALSE
      )
  )
  list(
    fit = structure(
      list(
        booster = final,
        binary = binary,
        levels = if (binary)
          levels(y)
        else
          NULL,
        group_names = active_hierarchy,
        no_random_effects = zero_level,
        cv_sd = if (binary)
          NULL
        else
          sqrt(best)
      ),
      class = "mlim.hml"
    ),
    score = if (binary)
      best
    else
      sqrt(best),
    # score is RMSE for Gaussian, Brier for binary, to match iterate().
    cv_folds = nfolds,
    cv_candidates = success,
    cv_strategy = cv_strategy
  )
}

mlim.hml.predict <- function(fit, x, hierarchy_data, stochastic = FALSE) {
  stopifnot(inherits(fit, "mlim.hml"))
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  if (!ncol(x))
    x <- matrix(0, nrow = nrow(x), ncol = 1L)
  if (isTRUE(fit$no_random_effects)) {
    if (!nrow(x))
      return(if (fit$binary)
        factor(character(), levels = fit$levels)
        else
          numeric())
    p <- as.numeric(stats::predict(fit$booster, data = x))
    if (length(p) != nrow(x) || any(!is.finite(p)))
      stop("HML returned invalid boosted-tree predictions.")
    if (fit$binary) {
      value <- if (stochastic)
        stats::rbinom(length(p), 1L, pmin(1, pmax(0, p)))
      else
        as.integer(p >= 0.5)
      return(factor(fit$levels[value + 1L], levels = fit$levels))
    }
    if (stochastic)
      p <- stats::rnorm(length(p), mean = p, sd = fit$cv_sd)
    return(p)
  }
  groups <- mlim.hml.groups(hierarchy_data, names(hierarchy_data))
  if (is.null(fit$group_names) ||
      !all(fit$group_names %in% colnames(groups))) {
    stop(
      "HML prediction cannot recover the random-effect grouping columns ",
      "used during training.",
      call. = FALSE
    )
  }
  groups <- groups[, fit$group_names, drop = FALSE]
  mlim.hml.prediction(
    fit$booster,
    x,
    groups,
    binary = fit$binary,
    stochastic = stochastic,
    factor_levels = fit$levels
  )
}
