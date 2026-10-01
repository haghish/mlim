# evaluation function
# ==================================================
# The function evaluates:
#
# 1. Cell-level imputation accuracy
#    - NRMSE for continuous variables
#    - MPCE for binary/nominal variables
#    - normalized rank error for ordinal variables
#
# 2. Data-structure preservation
#    - absolute relative SD bias for continuous variables
#    - absolute prevalence bias for categorical/ordinal variables
#    - absolute association error for pairs of variables
#
# Association measures:
#    - numeric-numeric: Pearson correlation
#    - numeric-ordinal / ordinal-ordinal: Spearman correlation
#    - numeric-binary: point-biserial correlation
#    - numeric-nominal: correlation ratio (eta)
#    - categorical-categorical: Cramer's V
#
# Lower values indicate better performance for all returned
# structure-preservation error measures.
# ==================================================

evaluate.imputation <- function(imputed, incomplete, complete) {

  # convert mice object to a list of completed datasets
  if (inherits(imputed, "mids")) {
    imputed <- mice::complete(imputed, action = "all", include = FALSE)
  }

  if (!is.list(imputed))
    stop("'imputed' must be a list of completed datasets or a mids object")

  if (anyNA(complete))
    stop("'complete' must not contain missing values")


  # Cell-level imputation accuracy
  # ==================================================

  error <- mlim.error(
    imputed,
    incomplete,
    complete,
    varwise = FALSE,
    transform = NULL,
    ignore.rank = FALSE
  )

  if (is.null(dim(error)))
    error <- matrix(error, nrow = 1, dimnames = list(NULL, names(error)))

  accuracy <- data.frame(
    NRMSE = if ("nrmse" %in% colnames(error)) error[, "nrmse"] else NA,
    MPCE = if ("mpce" %in% colnames(error)) error[, "mpce"] else NA,
    RankError = if ("missrank" %in% colnames(error)) error[, "missrank"] else NA
  )

  accuracy.summary <- data.frame(
    Metric = c("NRMSE", "MPCE", "RankError"),
    Mean = c(
      mean(accuracy$NRMSE, na.rm = TRUE),
      mean(accuracy$MPCE, na.rm = TRUE),
      mean(accuracy$RankError, na.rm = TRUE)
    ),
    SD = c(
      sd(accuracy$NRMSE, na.rm = TRUE),
      sd(accuracy$MPCE, na.rm = TRUE),
      sd(accuracy$RankError, na.rm = TRUE)
    )
  )

  accuracy.summary$Mean[is.nan(accuracy.summary$Mean)] <- NA
  accuracy.summary$SD[is.na(accuracy.summary$SD)] <- NA


  # Data-structure preservation: SD bias
  # ==================================================

  numeric.vars <- names(complete)[sapply(complete, is.numeric)]

  sd.bias <- NULL

  if (length(numeric.vars) > 0) {

    for (i in 1:length(imputed)) {

      for (v in numeric.vars) {

        sd.complete <- sd(complete[[v]])
        sd.imputed <- sd(imputed[[i]][[v]])

        tmp <- data.frame(
          Imputation = i,
          Variable = v,
          SD.complete = sd.complete,
          SD.imputed = sd.imputed,
          Relative.SD.Bias = (sd.imputed - sd.complete) / sd.complete,
          Absolute.Relative.SD.Bias = abs((sd.imputed - sd.complete) / sd.complete)
        )

        sd.bias <- rbind(sd.bias, tmp)
      }
    }
  }


  # Data-structure preservation: category prevalence bias
  # ==================================================

  categorical.vars <- names(complete)[
    sapply(complete, function(x) is.factor(x) || is.ordered(x))
  ]

  prevalence.bias <- NULL

  if (length(categorical.vars) > 0) {

    for (i in 1:length(imputed)) {

      for (v in categorical.vars) {

        lev <- levels(complete[[v]])

        for (l in lev) {

          p.complete <- mean(complete[[v]] == l)
          p.imputed <- mean(imputed[[i]][[v]] == l)

          tmp <- data.frame(
            Imputation = i,
            Variable = v,
            Level = l,
            Prevalence.complete = p.complete,
            Prevalence.imputed = p.imputed,
            Prevalence.Bias = p.imputed - p.complete,
            Absolute.Prevalence.Bias = abs(p.imputed - p.complete)
          )

          prevalence.bias <- rbind(prevalence.bias, tmp)
        }
      }
    }
  }


  # Data-structure preservation: association recovery
  # ==================================================

  association.error <- NULL

  vars <- names(complete)

  if (length(vars) > 1) {

    pairs <- combn(vars, 2, simplify = FALSE)

    for (i in 1:length(imputed)) {

      for (p in pairs) {

        v1 <- p[1]
        v2 <- p[2]

        x.true <- complete[[v1]]
        y.true <- complete[[v2]]

        x.imp <- imputed[[i]][[v1]]
        y.imp <- imputed[[i]][[v2]]

        type1 <- if (is.numeric(x.true)) {
          "numeric"
        } else if (is.ordered(x.true)) {
          "ordinal"
        } else if (is.factor(x.true) && nlevels(x.true) == 2) {
          "binary"
        } else if (is.factor(x.true)) {
          "nominal"
        } else {
          "other"
        }

        type2 <- if (is.numeric(y.true)) {
          "numeric"
        } else if (is.ordered(y.true)) {
          "ordinal"
        } else if (is.factor(y.true) && nlevels(y.true) == 2) {
          "binary"
        } else if (is.factor(y.true)) {
          "nominal"
        } else {
          "other"
        }


        # numeric-numeric
        if (type1 == "numeric" && type2 == "numeric") {

          a.true <- cor(x.true, y.true, method = "pearson")
          a.imp <- cor(x.imp, y.imp, method = "pearson")
          measure <- "Pearson r"


          # numeric-ordinal
        } else if ((type1 == "numeric" && type2 == "ordinal") ||
                   (type1 == "ordinal" && type2 == "numeric")) {

          if (type1 == "ordinal") {
            x.true <- as.numeric(x.true)
            x.imp <- as.numeric(x.imp)
          }

          if (type2 == "ordinal") {
            y.true <- as.numeric(y.true)
            y.imp <- as.numeric(y.imp)
          }

          a.true <- cor(x.true, y.true, method = "spearman")
          a.imp <- cor(x.imp, y.imp, method = "spearman")
          measure <- "Spearman r"


          # ordinal-ordinal
        } else if (type1 == "ordinal" && type2 == "ordinal") {

          a.true <- cor(as.numeric(x.true), as.numeric(y.true), method = "spearman")
          a.imp <- cor(as.numeric(x.imp), as.numeric(y.imp), method = "spearman")
          measure <- "Spearman r"


          # numeric-binary
        } else if ((type1 == "numeric" && type2 == "binary") ||
                   (type1 == "binary" && type2 == "numeric")) {

          if (type1 == "binary") {
            x.true <- as.numeric(x.true) - 1
            x.imp <- as.numeric(x.imp) - 1
          }

          if (type2 == "binary") {
            y.true <- as.numeric(y.true) - 1
            y.imp <- as.numeric(y.imp) - 1
          }

          a.true <- cor(x.true, y.true, method = "pearson")
          a.imp <- cor(x.imp, y.imp, method = "pearson")
          measure <- "Point-biserial r"


          # numeric-nominal or ordinal-nominal: eta
        } else if (
          ((type1 == "numeric" || type1 == "ordinal") && type2 == "nominal") ||
          (type1 == "nominal" && (type2 == "numeric" || type2 == "ordinal"))
        ) {

          if (type1 == "nominal") {

            group.true <- x.true
            group.imp <- x.imp

            value.true <- if (type2 == "ordinal") as.numeric(y.true) else y.true
            value.imp <- if (type2 == "ordinal") as.numeric(y.imp) else y.imp

          } else {

            group.true <- y.true
            group.imp <- y.imp

            value.true <- if (type1 == "ordinal") as.numeric(x.true) else x.true
            value.imp <- if (type1 == "ordinal") as.numeric(x.imp) else x.imp
          }

          grand.true <- mean(value.true)
          grand.imp <- mean(value.imp)

          ss.total.true <- sum((value.true - grand.true)^2)
          ss.total.imp <- sum((value.imp - grand.imp)^2)

          ss.between.true <- sum(
            tapply(value.true, group.true, function(z)
              length(z) * (mean(z) - grand.true)^2)
          )

          ss.between.imp <- sum(
            tapply(value.imp, group.imp, function(z)
              length(z) * (mean(z) - grand.imp)^2)
          )

          a.true <- sqrt(ss.between.true / ss.total.true)
          a.imp <- sqrt(ss.between.imp / ss.total.imp)
          measure <- "Eta"


          # categorical-categorical: Cramer's V
        } else if (
          type1 %in% c("binary", "nominal", "ordinal") &&
          type2 %in% c("binary", "nominal", "ordinal")
        ) {

          tab.true <- table(x.true, y.true)
          tab.imp <- table(x.imp, y.imp)

          chi.true <- suppressWarnings(chisq.test(tab.true, correct = FALSE)$statistic)
          chi.imp <- suppressWarnings(chisq.test(tab.imp, correct = FALSE)$statistic)

          n.true <- sum(tab.true)
          n.imp <- sum(tab.imp)

          k.true <- min(nrow(tab.true) - 1, ncol(tab.true) - 1)
          k.imp <- min(nrow(tab.imp) - 1, ncol(tab.imp) - 1)

          a.true <- if (k.true > 0)
            sqrt(as.numeric(chi.true) / (n.true * k.true)) else NA

          a.imp <- if (k.imp > 0)
            sqrt(as.numeric(chi.imp) / (n.imp * k.imp)) else NA

          measure <- "Cramer's V"

        } else {

          next
        }


        tmp <- data.frame(
          Imputation = i,
          Variable1 = v1,
          Variable2 = v2,
          Measure = measure,
          Association.complete = a.true,
          Association.imputed = a.imp,
          Association.Error = a.imp - a.true,
          Absolute.Association.Error = abs(a.imp - a.true)
        )

        association.error <- rbind(association.error, tmp)
      }
    }
  }


  # summarize data-structure preservation
  # ==================================================

  structure.summary <- data.frame(
    Metric = c(
      "Absolute relative SD bias",
      "Absolute prevalence bias",
      "Absolute association error"
    ),
    Mean = c(
      if (!is.null(sd.bias))
        mean(sd.bias$Absolute.Relative.SD.Bias, na.rm = TRUE) else NA,
      if (!is.null(prevalence.bias))
        mean(prevalence.bias$Absolute.Prevalence.Bias, na.rm = TRUE) else NA,
      if (!is.null(association.error))
        mean(association.error$Absolute.Association.Error, na.rm = TRUE) else NA
    ),
    SD = c(
      if (!is.null(sd.bias))
        sd(sd.bias$Absolute.Relative.SD.Bias, na.rm = TRUE) else NA,
      if (!is.null(prevalence.bias))
        sd(prevalence.bias$Absolute.Prevalence.Bias, na.rm = TRUE) else NA,
      if (!is.null(association.error))
        sd(association.error$Absolute.Association.Error, na.rm = TRUE) else NA
    )
  )


  return(
    list(
      accuracy = accuracy,
      accuracy.summary = accuracy.summary,
      sd.bias = sd.bias,
      prevalence.bias = prevalence.bias,
      association.error = association.error,
      structure.summary = structure.summary
    )
  )
}
