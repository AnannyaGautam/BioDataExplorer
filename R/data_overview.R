#' Generate a Comprehensive Data Overview
#'
#' Provides a high-level summary of a biological dataset including dimensions,
#' variable types, completeness, and basic statistics.
#'
#' @param data A data.frame or tibble containing the biological dataset.
#' @return A list with components: \code{dimensions}, \code{variable_summary},
#'   \code{completeness}, and \code{type_counts}.
#' @export
#' @examples
#' data(iris)
#' overview <- data_overview(iris)
#' print(overview)
data_overview <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble.")
  }

  n_rows <- nrow(data)
  n_cols <- ncol(data)

  # Detect types for each variable
  var_types <- detect_variable_types(data)

  # Missing value summary
  missing <- missing_summary(data)

  # Type counts
  type_counts <- table(var_types$detected_type)

  # Completeness score (% of non-missing cells)
  total_cells <- n_rows * n_cols
  total_missing <- sum(is.na(data))
  completeness_pct <- round((1 - total_missing / total_cells) * 100, 2)

  # Basic stats for numeric columns
  numeric_cols <- var_types$variable[var_types$detected_type %in% c("numeric_continuous", "numeric_discrete")]
  numeric_summary <- NULL
  if (length(numeric_cols) > 0) {
    numeric_summary <- data |>
      dplyr::select(dplyr::all_of(numeric_cols)) |>
      dplyr::summarise(dplyr::across(
        dplyr::everything(),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          min = ~min(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      )) |>
      tidyr::pivot_longer(
        dplyr::everything(),
        names_to = c("variable", "stat"),
        names_sep = "__",
        values_to = "value"
      ) |>
      tidyr::pivot_wider(names_from = stat, values_from = value)
  }

  structure(
    list(
      dimensions = list(rows = n_rows, columns = n_cols),
      variable_types = var_types,
      missing_data = missing,
      completeness_pct = completeness_pct,
      type_counts = as.data.frame(type_counts),
      numeric_summary = numeric_summary
    ),
    class = "bio_data_overview"
  )
}

#' Print method for bio_data_overview
#' @param x A bio_data_overview object.
#' @param ... Additional arguments (unused).
#' @export
print.bio_data_overview <- function(x, ...) {
  cat("=== BioData Explorer: Data Overview ===\n\n")
  cat(sprintf("Dimensions: %d rows x %d columns\n", x$dimensions$rows, x$dimensions$columns))
  cat(sprintf("Overall completeness: %.1f%%\n\n", x$completeness_pct))

  cat("Variable type breakdown:\n")
  for (i in seq_len(nrow(x$type_counts))) {
    cat(sprintf("  %-25s: %d\n", x$type_counts$Var1[i], x$type_counts$Freq[i]))
  }

  # Flag high-missingness variables
  high_miss <- x$missing_data[x$missing_data$pct_missing > 20, ]
  if (nrow(high_miss) > 0) {
    cat("\n!! Variables with >20% missing:\n")
    for (i in seq_len(nrow(high_miss))) {
      cat(sprintf("  %-25s: %.1f%% missing (%d values)\n",
                  high_miss$variable[i], high_miss$pct_missing[i], high_miss$n_missing[i]))
    }
  }

  invisible(x)
}
