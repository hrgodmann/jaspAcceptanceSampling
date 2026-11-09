.ssmInputOptions <- function(predictor = FALSE) {
  list(defectCount = "count", sampleSize = "n", lotSize = "s", lotNumber = "lot",
       covariates = if (predictor) "x" else "")
}

.ssmInputData <- function() {
  data.frame(count = c(3, 1, 2), n = c(30, 10, 20), s = c(300, 100, 200),
             lot = c(30, 10, 20), x = c(0.5, -1, 1), row_id = c("third", "first", "second"))
}

test_that("lot sorting is numeric and keeps every selected variable on its original row", {
  readData <- getFromNamespace(".asSSM_readData", "jaspAcceptanceSampling")
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  data <- .ssmInputData()
  data$lot <- c(1e12 + 0.25, -3, 10.5)
  original <- data
  for (predictor in c(FALSE, TRUE)) {
    options <- .ssmInputOptions(predictor)
    sorted <- readData(data, options)
    expect_identical(sorted, data[c(2, 3, 1), , drop = FALSE])
    expect_equal(sorted$lot, c(-3, 10.5, 1e12 + 0.25))
    expect_identical(sorted$row_id, c("first", "second", "third"))
    expect_null(validate(sorted, options))
    expect_identical(data, original)
  }
})

test_that("consistent zero, partial and complete inspection counts are valid in both modes", {
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  data <- data.frame(count = c(0, 0, 2, 4, 0), n = c(0, 10, 2, 5, 0),
                     s = c(10, 10, 10, 5, 0), lot = c(-2, 0.5, 3, 10, 100), x = -2:2)
  for (predictor in c(FALSE, TRUE)) {
    expect_null(validate(data, .ssmInputOptions(predictor)))
    large <- data
    large$count[1] <- large$n[1] <- large$s[1] <- .Machine$integer.max - 1
    expect_null(validate(large, .ssmInputOptions(predictor)))
  }
})

test_that("empty inputs and missing assigned columns return actionable validation messages", {
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  data <- .ssmInputData()
  for (predictor in c(FALSE, TRUE)) {
    options <- .ssmInputOptions(predictor)
    expect_match(validate(NULL, options), "empty", ignore.case = TRUE)
    expect_match(validate(data[FALSE, ], options), "empty", ignore.case = TRUE)
    required <- c("count", "n", "s", "lot", if (predictor) "x")
    for (column in required) {
      missing <- data
      missing[[column]] <- NULL
      expect_match(validate(missing, options), "missing", ignore.case = TRUE)
    }
  }
  inactive <- data
  inactive$x <- NULL
  expect_null(validate(inactive, .ssmInputOptions(FALSE)))
})

test_that("all assigned numeric variables reject missing and nonfinite values", {
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  for (predictor in c(FALSE, TRUE)) {
    columns <- c("count", "n", "s", "lot", if (predictor) "x")
    for (column in columns) {
      for (value in c(NA_real_, NaN, Inf, -Inf)) {
        invalid <- .ssmInputData()
        invalid[[column]][2] <- value
        expect_match(validate(invalid, .ssmInputOptions(predictor)),
                     "missing|finite", ignore.case = TRUE)
      }
    }
  }
})

test_that("character, factor, logical, list and complex columns are not silently coerced", {
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  invalidColumns <- list(c("1", "2", "3"), factor(c(1, 2, 3)), c(TRUE, FALSE, TRUE),
                          list(1, 2, 3), c(1 + 0i, 2 + 0i, 3 + 0i))
  for (predictor in c(FALSE, TRUE)) {
    columns <- c("count", "n", "s", "lot", if (predictor) "x")
    for (column in columns) {
      for (values in invalidColumns) {
        invalid <- .ssmInputData()
        invalid[[column]] <- values
        expect_match(validate(invalid, .ssmInputOptions(predictor)),
                     "numeric|number|real", ignore.case = TRUE)
      }
    }
  }
  inactive <- .ssmInputData()
  inactive$x <- c("not", "a", "predictor")
  expect_null(validate(inactive, .ssmInputOptions(FALSE)))
})

test_that("counts must be exactly whole, nonnegative and below the integer indexing limit", {
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  for (predictor in c(FALSE, TRUE)) {
    options <- .ssmInputOptions(predictor)
    for (column in c("count", "n", "s")) {
      # These tiny fractions previously passed a tolerance and could be truncated by as.integer().
      for (value in c(0.5, 1 - 1e-10, 1 + 1e-10)) {
        invalid <- .ssmInputData()
        invalid[[column]][2] <- value
        expect_match(validate(invalid, options), "whole|integer", ignore.case = TRUE)
      }
      invalid <- .ssmInputData()
      invalid[[column]][2] <- -1
      expect_match(validate(invalid, options), "negative|nonnegative|non-negative", ignore.case = TRUE)
      for (value in c(.Machine$integer.max, .Machine$integer.max + 1)) {
        invalid <- .ssmInputData()
        invalid[[column]][2] <- value
        expect_match(validate(invalid, options), "integer|large|limit|range|below|less|smaller", ignore.case = TRUE)
      }
    }
    invalid <- .ssmInputData()
    invalid$count[2] <- invalid$n[2] + 1
    expect_match(validate(invalid, options), "defect.*sample|count.*inspected", ignore.case = TRUE)
    invalid <- .ssmInputData()
    invalid$n[2] <- invalid$s[2] + 1
    expect_match(validate(invalid, options), "sample.*lot|inspected.*lot", ignore.case = TRUE)
  }
})

test_that("lot identifiers are unique labels rather than integer elapsed-time increments", {
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  for (predictor in c(FALSE, TRUE)) {
    options <- .ssmInputOptions(predictor)
    duplicate <- .ssmInputData()
    duplicate$lot <- c(1.5, 8, 1.5)
    expect_match(validate(duplicate, options), "unique|duplicate|one row|only once", ignore.case = TRUE)
    gaps <- .ssmInputData()
    gaps$lot <- c(-10.5, 0, 1000.25)
    expect_null(validate(gaps, options))
  }
})

test_that("one lot is supported only without a predictor and the predictor must vary", {
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  single <- .ssmInputData()[1, , drop = FALSE]
  expect_null(validate(single, .ssmInputOptions(FALSE)))
  expect_match(validate(single, .ssmInputOptions(TRUE)),
               "two|multiple|more than one", ignore.case = TRUE)
  for (values in list(rep(5, 3), rep(1e-150, 3), rep(0, 3))) {
    data <- .ssmInputData()
    data$x <- values
    expect_match(validate(data, .ssmInputOptions(TRUE)),
                 "vary|variation|constant|standard deviation", ignore.case = TRUE)
  }
})


test_that("predictor assignment alone governs readiness and validation", {
  ready <- getFromNamespace(".asSSM_isReady", "jaspAcceptanceSampling")
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  data <- .ssmInputData()
  data$x <- rep(1, nrow(data))

  options <- .ssmInputOptions(FALSE)
  options$ssm_scaleModel <- "WithPredictor"
  expect_true(ready(options, data))
  expect_null(validate(data, options))

  options$covariates <- "x"
  options$ssm_scaleModel <- "WithoutPredictor"
  expect_true(ready(options, data))
  expect_match(validate(data, options), "vary|variation|constant", ignore.case = TRUE)
  options$defectCount <- ""
  expect_false(ready(options, data))
})


test_that("predictor validation and standardization are invariant to representable unit changes", {
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  design <- getFromNamespace(".asSSM_createPredictorDesign", "jaspAcceptanceSampling")
  options <- .ssmInputOptions(TRUE)
  data <- .ssmInputData()
  data$x <- c(-1, 0, 1)
  expected <- design(data, options)$X
  for (unit in c(1e-150, 1e-10, sqrt(.Machine$double.eps), 1, 1e10, 1e150)) {
    changed <- data
    changed$x <- unit * data$x
    expect_null(validate(changed, options))
    expect_equal(design(changed, options)$X, expected, tolerance = 1e-12)
    changed$x <- changed$x + 10 * unit
    expect_null(validate(changed, options))
    expect_equal(design(changed, options)$X, expected, tolerance = 1e-12)
  }
})

test_that("unstandardizable finite covariates request rescaling instead of claiming no variation", {
  validate <- getFromNamespace(".asSSM_validateInputs", "jaspAcceptanceSampling")
  data <- .ssmInputData()
  for (values in list(c(-1e154, 0, 1e154), c(-1e-200, 0, 1e-200))) {
    data$x <- values
    expect_match(validate(data, .ssmInputOptions(TRUE)), "Rescale|rescale")
  }
})
