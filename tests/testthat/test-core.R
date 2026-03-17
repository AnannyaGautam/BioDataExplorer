library(testthat)

# --- detect_variable_types ---
test_that("detect_variable_types classifies correctly", {
  df <- data.frame(
    id = paste0("S", 1:100),
    age = rnorm(100, 50, 10),
    score = sample(1:5, 100, replace = TRUE),
    sex = sample(c("M", "F"), 100, replace = TRUE),
    severity = sample(c("mild", "moderate", "severe"), 100, replace = TRUE),
    alive = sample(c(TRUE, FALSE), 100, replace = TRUE)
  )

  types <- detect_variable_types(df)

  expect_s3_class(types, "data.frame")
  expect_equal(nrow(types), ncol(df))
  expect_true("detected_type" %in% names(types))

  # Numeric continuous

  expect_equal(types$detected_type[types$variable == "age"], "numeric_continuous")

  # Binary logical
  expect_equal(types$detected_type[types$variable == "alive"], "binary")

  # Binary character (2 levels)
  expect_equal(types$detected_type[types$variable == "sex"], "binary")

  # Identifier (high cardinality)
  expect_equal(types$detected_type[types$variable == "id"], "identifier")
})

test_that("detect_variable_types handles empty data", {
  expect_error(detect_variable_types("not a dataframe"))
})


# --- missing_summary ---
test_that("missing_summary returns correct counts", {
  df <- data.frame(a = c(1, NA, 3, NA, 5), b = c(1, 2, 3, 4, 5), c = rep(NA, 5))

  ms <- missing_summary(df)

  expect_equal(ms$n_missing[ms$variable == "a"], 2)
  expect_equal(ms$n_missing[ms$variable == "b"], 0)
  expect_equal(ms$n_missing[ms$variable == "c"], 5)

  expect_equal(ms$pct_missing[ms$variable == "a"], 40)
  expect_equal(ms$severity[ms$variable == "b"], "none")
  expect_equal(ms$severity[ms$variable == "c"], "critical")
})


# --- data_overview ---
test_that("data_overview returns expected structure", {
  overview <- data_overview(iris)

  expect_s3_class(overview, "bio_data_overview")
  expect_equal(overview$dimensions$rows, 150)
  expect_equal(overview$dimensions$columns, 5)
  expect_equal(overview$completeness_pct, 100)
})


# --- override_variable_type ---
test_that("override_variable_type works", {
  df <- data.frame(x = 1:10, y = letters[1:10])
  types <- detect_variable_types(df)

  updated <- override_variable_type(types, "x", "categorical_ordinal")
  expect_equal(updated$detected_type[updated$variable == "x"], "categorical_ordinal")

  expect_error(override_variable_type(types, "x", "invalid_type"))
  expect_error(override_variable_type(types, "nonexistent", "binary"))
})


# --- suggest_imputation ---
test_that("suggest_imputation returns NULL for complete data", {
  expect_message(suggest_imputation(iris), "No missing values")
})

test_that("suggest_imputation returns suggestions for incomplete data", {
  df <- iris
  df$Sepal.Length[1:5] <- NA
  result <- suggest_imputation(df)

  expect_s3_class(result, "data.frame")
  expect_true("suggested_method" %in% names(result))
  expect_equal(nrow(result), 1)
})
