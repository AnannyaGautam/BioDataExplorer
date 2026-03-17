#' Detect Variable Types in a Biological Dataset
#'
#' Automatically classifies each column as numeric_continuous, numeric_discrete,
#' categorical_nominal, categorical_ordinal, binary, date_time, identifier, or
#' free_text. Uses heuristics appropriate for biological/clinical data.
#'
#' @param data A data.frame or tibble.
#' @param id_threshold Numeric. If a character/factor column has unique values
#'   exceeding this proportion of total rows, it is flagged as an identifier.
#'   Default is 0.9.
#' @param discrete_threshold Integer. Numeric columns with fewer unique values
#'   than this are classified as discrete. Default is 15.
#' @return A data.frame with columns: \code{variable}, \code{original_class},
#'   \code{detected_type}, \code{n_unique}, \code{example_values}.
#' @export
#' @examples
#' data(iris)
#' detect_variable_types(iris)
detect_variable_types <- function(data, id_threshold = 0.9, discrete_threshold = 15) {
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame or tibble.")
  }

  n <- nrow(data)

  result <- purrr::map_dfr(names(data), function(col_name) {
    x <- data[[col_name]]
    original_class <- paste(class(x), collapse = ", ")
    n_unique <- length(unique(stats::na.omit(x)))
    examples <- paste(utils::head(unique(stats::na.omit(x)), 5), collapse = ", ")

    detected_type <- classify_single_variable(x, n, n_unique, id_threshold, discrete_threshold)

    data.frame(
      variable = col_name,
      original_class = original_class,
      detected_type = detected_type,
      n_unique = n_unique,
      example_values = examples,
      stringsAsFactors = FALSE
    )
  })

  result
}

#' Classify a single variable
#' @keywords internal
classify_single_variable <- function(x, n_rows, n_unique, id_threshold, discrete_threshold) {

  # Check for date/time types
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return("date_time")
  }

  # Check for logical
  if (is.logical(x)) {
    return("binary")
  }

  # Numeric types
  if (is.numeric(x)) {
    if (n_unique == 2) {
      return("binary")
    }
    if (n_unique <= discrete_threshold && all(x == floor(x), na.rm = TRUE)) {
      return("numeric_discrete")
    }
    return("numeric_continuous")
  }

  # Character / Factor types
  if (is.character(x) || is.factor(x)) {
    # Check if it looks like a date
    sample_vals <- as.character(utils::head(stats::na.omit(x), 20))
    date_parse <- tryCatch({
      suppressWarnings(as.Date(sample_vals, tryFormats = c(
        "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d", "%d-%m-%Y"
      )))
    }, error = function(e) rep(NA, length(sample_vals)))
    if (sum(!is.na(date_parse)) > length(sample_vals) * 0.8) {
      return("date_time")
    }

    # Identifier: very high cardinality
    if (n_unique / n_rows > id_threshold) {
      return("identifier")
    }

    # Binary
    if (n_unique == 2) {
      return("binary")
    }

    # Free text: long strings with high cardinality
    avg_nchar <- mean(nchar(as.character(stats::na.omit(x))), na.rm = TRUE)
    if (avg_nchar > 50 && n_unique / n_rows > 0.5) {
      return("free_text")
    }

    # Ordinal detection: check for common ordinal patterns
    lower_vals <- tolower(as.character(unique(stats::na.omit(x))))
    ordinal_patterns <- c(
      "low", "medium", "high",
      "mild", "moderate", "severe",
      "none", "slight", "moderate", "strong",
      "grade i", "grade ii", "grade iii", "grade iv",
      "stage 1", "stage 2", "stage 3", "stage 4",
      "negative", "positive",
      "never", "rarely", "sometimes", "often", "always",
      "poor", "fair", "good", "excellent"
    )
    if (any(lower_vals %in% ordinal_patterns)) {
      return("categorical_ordinal")
    }

    return("categorical_nominal")
  }

  "unknown"
}

#' Override detected variable type
#'
#' Allows users to manually set the type for a specific variable, overriding
#' the auto-detection.
#'
#' @param type_df A data.frame returned by \code{detect_variable_types}.
#' @param variable Character. The variable name to override.
#' @param new_type Character. The new type to assign.
#' @return Updated data.frame with the overridden type.
#' @export
override_variable_type <- function(type_df, variable, new_type) {
  valid_types <- c(
    "numeric_continuous", "numeric_discrete",
    "categorical_nominal", "categorical_ordinal",
    "binary", "date_time", "identifier", "free_text"
  )

  if (!new_type %in% valid_types) {
    stop(sprintf("new_type must be one of: %s", paste(valid_types, collapse = ", ")))
  }

  if (!variable %in% type_df$variable) {
    stop(sprintf("Variable '%s' not found in type_df.", variable))
  }

  type_df$detected_type[type_df$variable == variable] <- new_type
  type_df
}
