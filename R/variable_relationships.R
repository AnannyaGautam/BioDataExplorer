#' Analyse Relationship Between Two Variables
#'
#' Automatically selects the appropriate statistical test and visualisation
#' based on the types of the two variables.
#'
#' @param data A data.frame or tibble.
#' @param var1 Character. Name of the first variable.
#' @param var2 Character. Name of the second variable.
#' @param var_types Optional. Output of \code{detect_variable_types(data)}.
#'   If NULL, types are auto-detected.
#' @return A list with: \code{test_name}, \code{test_result}, \code{effect_size},
#'   \code{plot}, and \code{interpretation}.
#' @export
variable_relationship <- function(data, var1, var2, var_types = NULL) {
  if (!var1 %in% names(data)) stop(sprintf("Variable '%s' not found.", var1))
  if (!var2 %in% names(data)) stop(sprintf("Variable '%s' not found.", var2))

  if (is.null(var_types)) {
    var_types <- detect_variable_types(data)
  }

  type1 <- get_broad_type(var_types$detected_type[var_types$variable == var1])
  type2 <- get_broad_type(var_types$detected_type[var_types$variable == var2])

  # Remove rows where either variable is NA
  complete_data <- data[stats::complete.cases(data[, c(var1, var2)]), ]

  if (nrow(complete_data) < 3) {
    return(list(
      test_name = "Insufficient data",
      test_result = NULL,
      plot = NULL,
      interpretation = "Too few complete observations to test."
    ))
  }

  # Route to the appropriate analysis
  if (type1 == "numeric" && type2 == "numeric") {
    result <- analyse_num_num(complete_data, var1, var2)
  } else if (type1 == "categorical" && type2 == "categorical") {
    result <- analyse_cat_cat(complete_data, var1, var2)
  } else if (type1 == "numeric" && type2 == "categorical") {
    result <- analyse_num_cat(complete_data, numeric_var = var1, cat_var = var2)
  } else if (type1 == "categorical" && type2 == "numeric") {
    result <- analyse_num_cat(complete_data, numeric_var = var2, cat_var = var1)
  } else {
    result <- list(
      test_name = "Not supported",
      test_result = NULL,
      plot = NULL,
      interpretation = sprintf("Analysis not supported for types: %s vs %s", type1, type2)
    )
  }

  result$var1 <- var1
  result$var2 <- var2
  result$n_used <- nrow(complete_data)

  structure(result, class = "bio_var_relationship")
}


#' Get broad type category
#' @keywords internal
get_broad_type <- function(detected_type) {
  if (detected_type %in% c("numeric_continuous", "numeric_discrete")) return("numeric")
  if (detected_type %in% c("categorical_nominal", "categorical_ordinal", "binary")) return("categorical")
  return("other")
}


#' Numeric vs Numeric analysis
#' @keywords internal
analyse_num_num <- function(data, var1, var2) {
  x <- data[[var1]]
  y <- data[[var2]]

  # Correlation
  cor_pearson <- stats::cor.test(x, y, method = "pearson")
  cor_spearman <- stats::cor.test(x, y, method = "spearman")

  # Plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[var1]], y = .data[[var2]])) +
    ggplot2::geom_point(alpha = 0.5, colour = "#2c7bb6") +
    ggplot2::geom_smooth(method = "lm", se = TRUE, colour = "#d7191c", linewidth = 0.8) +
    ggplot2::labs(
      title = sprintf("%s vs %s", var1, var2),
      subtitle = sprintf("Pearson r = %.3f (p = %.4f)", cor_pearson$estimate, cor_pearson$p.value)
    ) +
    ggplot2::theme_minimal()

  # Interpretation
  r <- abs(cor_pearson$estimate)
  strength <- dplyr::case_when(
    r < 0.1 ~ "negligible",
    r < 0.3 ~ "weak",
    r < 0.5 ~ "moderate",
    r < 0.7 ~ "strong",
    TRUE ~ "very strong"
  )
  direction <- ifelse(cor_pearson$estimate > 0, "positive", "negative")
  sig <- ifelse(cor_pearson$p.value < 0.05, "statistically significant", "not statistically significant")

  list(
    test_name = "Correlation (Pearson & Spearman)",
    test_result = list(pearson = cor_pearson, spearman = cor_spearman),
    effect_size = cor_pearson$estimate,
    plot = p,
    interpretation = sprintf(
      "There is a %s %s linear relationship (r = %.3f, p = %.4f), which is %s. Spearman rho = %.3f.",
      strength, direction, cor_pearson$estimate, cor_pearson$p.value, sig, cor_spearman$estimate
    )
  )
}


#' Categorical vs Categorical analysis
#' @keywords internal
analyse_cat_cat <- function(data, var1, var2) {
  tbl <- table(data[[var1]], data[[var2]])

  # Chi-square test
  chi_test <- tryCatch(
    stats::chisq.test(tbl),
    error = function(e) NULL
  )

  # Cramér's V
  cramers_v <- NULL
  if (!is.null(chi_test)) {
    n <- sum(tbl)
    k <- min(nrow(tbl), ncol(tbl))
    cramers_v <- sqrt(chi_test$statistic / (n * (k - 1)))
    names(cramers_v) <- "Cramer's V"
  }

  # Mosaic-style bar plot
  plot_data <- as.data.frame(tbl)
  names(plot_data) <- c("Var1", "Var2", "Freq")

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Var1, y = Freq, fill = Var2)) +
    ggplot2::geom_bar(stat = "identity", position = "fill") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
      title = sprintf("%s vs %s", var1, var2),
      y = "Proportion", x = var1, fill = var2
    ) +
    ggplot2::theme_minimal()

  sig <- if (!is.null(chi_test) && chi_test$p.value < 0.05) "statistically significant" else "not statistically significant"

  list(
    test_name = "Chi-squared test of independence",
    test_result = chi_test,
    effect_size = cramers_v,
    plot = p,
    interpretation = sprintf(
      "The association between %s and %s is %s (Chi-sq = %.2f, p = %.4f, Cramer's V = %.3f).",
      var1, var2, sig,
      ifelse(is.null(chi_test), NA, chi_test$statistic),
      ifelse(is.null(chi_test), NA, chi_test$p.value),
      ifelse(is.null(cramers_v), NA, cramers_v)
    )
  )
}


#' Numeric vs Categorical analysis
#' @keywords internal
analyse_num_cat <- function(data, numeric_var, cat_var) {
  x <- data[[numeric_var]]
  groups <- as.factor(data[[cat_var]])
  n_groups <- nlevels(groups)

  # Choose test
  if (n_groups == 2) {
    test <- stats::t.test(x ~ groups)
    test_name <- "Welch's t-test"
    p_val <- test$p.value
    stat_val <- test$statistic

    # Cohen's d
    group_vals <- split(x, groups)
    m1 <- mean(group_vals[[1]], na.rm = TRUE)
    m2 <- mean(group_vals[[2]], na.rm = TRUE)
    s_pooled <- sqrt((stats::var(group_vals[[1]], na.rm = TRUE) +
                        stats::var(group_vals[[2]], na.rm = TRUE)) / 2)
    effect <- (m1 - m2) / s_pooled
    names(effect) <- "Cohen's d"
  } else {
    test <- stats::aov(x ~ groups, data = data)
    test_summary <- summary(test)
    test_name <- "One-way ANOVA"
    p_val <- test_summary[[1]][["Pr(>F)"]][1]
    stat_val <- test_summary[[1]][["F value"]][1]

    # Eta-squared
    ss_between <- test_summary[[1]][["Sum Sq"]][1]
    ss_total <- sum(test_summary[[1]][["Sum Sq"]])
    effect <- ss_between / ss_total
    names(effect) <- "Eta-squared"
  }

  # Plot: boxplot + jitter
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[cat_var]], y = .data[[numeric_var]],
                                           fill = .data[[cat_var]])) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    ggplot2::geom_jitter(alpha = 0.3, width = 0.15, size = 1) +
    ggplot2::labs(
      title = sprintf("%s by %s", numeric_var, cat_var),
      subtitle = sprintf("%s: p = %.4f", test_name, p_val)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  sig <- ifelse(p_val < 0.05, "statistically significant", "not statistically significant")

  list(
    test_name = test_name,
    test_result = test,
    effect_size = effect,
    plot = p,
    interpretation = sprintf(
      "The difference in %s across %s groups is %s (%s, p = %.4f, effect = %.3f).",
      numeric_var, cat_var, sig, test_name, p_val, effect
    )
  )
}
