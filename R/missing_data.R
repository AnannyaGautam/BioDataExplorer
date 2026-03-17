#' Summarise Missing Values Across All Variables
#'
#' Returns a per-variable summary of missing data with counts, percentages,
#' and a severity classification.
#'
#' @param data A data.frame or tibble.
#' @return A data.frame with columns: \code{variable}, \code{n_missing},
#'   \code{pct_missing}, \code{n_complete}, \code{severity}.
#' @export
#' @examples
#' df <- data.frame(a = c(1, NA, 3), b = c(NA, NA, 3), c = 1:3)
#' missing_summary(df)
missing_summary <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble.")
  }

  n <- nrow(data)

  result <- purrr::map_dfr(names(data), function(col) {
    n_miss <- sum(is.na(data[[col]]))
    pct_miss <- round(n_miss / n * 100, 2)

    severity <- dplyr::case_when(
      pct_miss == 0 ~ "none",
      pct_miss < 5 ~ "low",
      pct_miss < 20 ~ "moderate",
      pct_miss < 50 ~ "high",
      TRUE ~ "critical"
    )

    data.frame(
      variable = col,
      n_missing = n_miss,
      pct_missing = pct_miss,
      n_complete = n - n_miss,
      severity = severity,
      stringsAsFactors = FALSE
    )
  })

  result[order(-result$pct_missing), ]
}


#' Analyse Missing Data Patterns and Mechanism
#'
#' Performs deeper missing data analysis: pattern detection, co-missingness,
#' and attempts to classify the missingness mechanism (MCAR, MAR, MNAR).
#'
#' @param data A data.frame or tibble.
#' @param test_mcar Logical. Whether to run Little's MCAR test (requires
#'   \code{naniar} package). Default is TRUE.
#' @return A list with components:
#'   \describe{
#'     \item{summary}{Per-variable missing summary}
#'     \item{patterns}{Unique missingness patterns as a matrix}
#'     \item{co_missingness}{Pairwise co-missingness rates}
#'     \item{mcar_test}{Results of Little's MCAR test (if requested)}
#'     \item{does_it_matter}{Per-variable assessment of whether missingness matters}
#'   }
#' @export
missing_pattern_analysis <- function(data, test_mcar = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble.")
  }

  summary_df <- missing_summary(data)

  # --- Missing patterns ---
  miss_matrix <- is.na(data) * 1L
  patterns <- unique(miss_matrix)
  pattern_counts <- as.data.frame(table(apply(miss_matrix, 1, paste, collapse = "")))
  names(pattern_counts) <- c("pattern", "count")
  pattern_counts <- pattern_counts[order(-pattern_counts$count), ]

  # --- Co-missingness (pairwise) ---
  cols_with_missing <- summary_df$variable[summary_df$n_missing > 0]
  co_miss <- NULL
  if (length(cols_with_missing) >= 2) {
    miss_sub <- miss_matrix[, cols_with_missing, drop = FALSE]
    co_miss <- cor(miss_sub)
    co_miss <- round(co_miss, 3)
  }

  # --- Little's MCAR test ---
  mcar_result <- NULL
  if (test_mcar) {
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    if (ncol(numeric_data) >= 2 && any(is.na(numeric_data))) {
      tryCatch({
        mcar_result <- naniar::mcar_test(numeric_data)
      }, error = function(e) {
        mcar_result <<- list(
          note = "MCAR test failed. This can happen with very sparse data.",
          error = conditionMessage(e)
        )
      })
    } else {
      mcar_result <- list(note = "MCAR test requires >= 2 numeric columns with missing values.")
    }
  }

  # --- Does missingness matter? ---
  does_it_matter <- assess_missingness_impact(data, summary_df)

  structure(
    list(
      summary = summary_df,
      n_patterns = nrow(patterns),
      pattern_counts = pattern_counts,
      co_missingness = co_miss,
      mcar_test = mcar_result,
      does_it_matter = does_it_matter
    ),
    class = "bio_missing_analysis"
  )
}


#' Assess whether missingness matters for each variable
#'
#' Uses heuristics and statistical tests to determine if the missing data
#' in each variable is likely to bias analyses.
#'
#' @param data Original data.frame.
#' @param summary_df Missing summary data.frame from \code{missing_summary}.
#' @return A data.frame with variable-level assessments.
#' @keywords internal
assess_missingness_impact <- function(data, summary_df) {
  n <- nrow(data)

  purrr::map_dfr(seq_len(nrow(summary_df)), function(i) {
    var_name <- summary_df$variable[i]
    pct_miss <- summary_df$pct_missing[i]
    n_miss <- summary_df$n_missing[i]

    if (n_miss == 0) {
      return(data.frame(
        variable = var_name,
        matters = FALSE,
        reason = "No missing values",
        recommendation = "No action needed",
        stringsAsFactors = FALSE
      ))
    }

    matters <- TRUE
    reasons <- character(0)
    recommendations <- character(0)

    # Rule 1: Very low missingness is usually ignorable
    if (pct_miss < 2) {
      matters <- FALSE
      reasons <- c(reasons, sprintf("Very low missingness (%.1f%%)", pct_miss))
      recommendations <- c(recommendations, "Safe to use complete cases or simple imputation")
    }

    # Rule 2: Critical missingness — variable may be unusable
    if (pct_miss > 60) {
      matters <- TRUE
      reasons <- c(reasons, sprintf("Critical missingness (%.1f%%) — variable may be unreliable", pct_miss))
      recommendations <- c(recommendations, "Consider dropping this variable or investigating why it is so incomplete")
    }

    # Rule 3: Check if missingness correlates with other variables
    if (n_miss > 0 && pct_miss >= 2 && pct_miss <= 60) {
      miss_indicator <- is.na(data[[var_name]])
      other_numeric <- data[, sapply(data, is.numeric) & names(data) != var_name, drop = FALSE]

      if (ncol(other_numeric) > 0) {
        sig_associations <- 0
        for (other_col in names(other_numeric)) {
          tryCatch({
            tt <- stats::t.test(other_numeric[[other_col]] ~ miss_indicator)
            if (tt$p.value < 0.05) sig_associations <- sig_associations + 1
          }, error = function(e) NULL)
        }

        if (sig_associations > 0) {
          matters <- TRUE
          reasons <- c(reasons, sprintf(
            "Missingness is associated with %d other variable(s) — likely MAR/MNAR", sig_associations
          ))
          recommendations <- c(recommendations, "Use multiple imputation (e.g., mice) rather than complete case analysis")
        }
      }

      if (length(reasons) == 0) {
        reasons <- c(reasons, sprintf("Moderate missingness (%.1f%%) with no detected associations", pct_miss))
        recommendations <- c(recommendations, "Likely MCAR — simple imputation or complete cases acceptable")
        if (pct_miss > 10) matters <- TRUE else matters <- FALSE
      }
    }

    data.frame(
      variable = var_name,
      matters = matters,
      reason = paste(reasons, collapse = "; "),
      recommendation = paste(recommendations, collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
}


#' Suggest Imputation Strategy
#'
#' Based on variable types and missingness patterns, suggests the most
#' appropriate imputation method for each variable.
#'
#' @param data A data.frame or tibble.
#' @return A data.frame with columns: \code{variable}, \code{type},
#'   \code{pct_missing}, \code{suggested_method}, \code{rationale}.
#' @export
suggest_imputation <- function(data) {
  types <- detect_variable_types(data)
  missingness <- missing_summary(data)

  merged <- merge(types[, c("variable", "detected_type")],
                  missingness[, c("variable", "pct_missing")],
                  by = "variable")
  merged <- merged[merged$pct_missing > 0, ]

  if (nrow(merged) == 0) {
    message("No missing values found. No imputation needed.")
    return(invisible(NULL))
  }

  purrr::map_dfr(seq_len(nrow(merged)), function(i) {
    vtype <- merged$detected_type[i]
    pct <- merged$pct_missing[i]
    var_name <- merged$variable[i]

    if (pct > 60) {
      return(data.frame(
        variable = var_name, type = vtype, pct_missing = pct,
        suggested_method = "Consider dropping",
        rationale = "Too much missing data for reliable imputation",
        stringsAsFactors = FALSE
      ))
    }

    method <- dplyr::case_when(
      vtype %in% c("numeric_continuous") && pct < 10 ~ "Mean/median imputation",
      vtype %in% c("numeric_continuous") && pct >= 10 ~ "Multiple imputation (mice/pmm)",
      vtype %in% c("numeric_discrete") && pct < 10 ~ "Mode or rounded median imputation",
      vtype %in% c("numeric_discrete") && pct >= 10 ~ "Multiple imputation (mice/pmm)",
      vtype %in% c("categorical_nominal", "categorical_ordinal") && pct < 10 ~ "Mode imputation",
      vtype %in% c("categorical_nominal", "categorical_ordinal") && pct >= 10 ~ "Multiple imputation (mice/logreg or polyreg)",
      vtype == "binary" && pct < 10 ~ "Mode imputation",
      vtype == "binary" && pct >= 10 ~ "Multiple imputation (mice/logreg)",
      TRUE ~ "Manual review recommended"
    )

    rationale <- dplyr::case_when(
      pct < 5 ~ "Low missingness — simple methods are adequate",
      pct < 20 ~ "Moderate missingness — consider pattern before choosing method",
      TRUE ~ "High missingness — multiple imputation preserves uncertainty"
    )

    data.frame(
      variable = var_name, type = vtype, pct_missing = pct,
      suggested_method = method, rationale = rationale,
      stringsAsFactors = FALSE
    )
  })
}


#' Print method for bio_missing_analysis
#' @param x A bio_missing_analysis object.
#' @param ... Additional arguments (unused).
#' @export
print.bio_missing_analysis <- function(x, ...) {
  cat("=== BioData Explorer: Missing Data Analysis ===\n\n")

  # Summary
  has_missing <- x$summary[x$summary$n_missing > 0, ]
  cat(sprintf("Variables with missing data: %d / %d\n", nrow(has_missing), nrow(x$summary)))
  cat(sprintf("Unique missingness patterns: %d\n\n", x$n_patterns))

  # MCAR test
  if (!is.null(x$mcar_test)) {
    if (inherits(x$mcar_test, "data.frame") || "statistic" %in% names(x$mcar_test)) {
      cat(sprintf("Little's MCAR test: chi-sq = %.2f, df = %d, p = %.4f\n",
                  x$mcar_test$statistic, x$mcar_test$df, x$mcar_test$p.value))
      if (x$mcar_test$p.value < 0.05) {
        cat("  -> Data is likely NOT MCAR (missingness may depend on observed values)\n\n")
      } else {
        cat("  -> No evidence against MCAR (missingness appears random)\n\n")
      }
    } else if (is.list(x$mcar_test) && "note" %in% names(x$mcar_test)) {
      cat(sprintf("MCAR test note: %s\n\n", x$mcar_test$note))
    }
  }

  # Does it matter?
  matters <- x$does_it_matter[x$does_it_matter$matters == TRUE, ]
  if (nrow(matters) > 0) {
    cat("Variables where missingness MATTERS:\n")
    for (i in seq_len(nrow(matters))) {
      cat(sprintf("  %s: %s\n    -> %s\n",
                  matters$variable[i], matters$reason[i], matters$recommendation[i]))
    }
  } else {
    cat("Missingness does not appear to be a concern for any variable.\n")
  }

  invisible(x)
}
