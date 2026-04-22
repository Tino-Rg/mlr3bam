library(testthat)

# Classification Tests
test_that("LearnerClassifBam trains and predicts correctly", {
  skip_if_not_installed("mgcv")

  learner = LearnerClassifBam$new()
  task = mlr3::tsk("sonar")

  learner$param_set$set_values(
    k = 5,
    discrete = TRUE,
    nthreads = 1
  )

  # Often throw convergence warnings (like probabilities being 0 or 1).
  # We suppress them to keep tests clean, and only expect "NO ERROR".
  expect_error(suppressWarnings(learner$train(task)), NA)

  pred = learner$predict(task)

  # Validate the structure and type of the prediction object
  expect_true(inherits(pred, "PredictionClassif"))
  expect_equal(pred$task_type, "classif")

  # Ensure no missing values are present in the final predictions
  expect_false(any(is.na(pred$response)))
})

# Regression Tests
test_that("LearnerRegrBam trains and predicts correctly", {
  skip_if_not_installed("mgcv")

  learner = LearnerRegrBam$new()
  task = mlr3::tsk("mtcars")

  # Only select continuous features for this unit test.
  task$select(c("disp", "hp", "drat", "wt", "qsec"))

  learner$param_set$set_values(
    k = 3,
    method = "fREML"
  )

  # Often throw convergence warnings (like probabilities being 0 or 1).
  # We suppress them to keep tests clean, and only expect "NO ERROR".
  expect_error(suppressWarnings(learner$train(task)), NA)

  pred = learner$predict(task)

  # Validate the structure and type of the prediction object
  expect_true(inherits(pred, "PredictionRegr"))
  expect_equal(pred$task_type, "regr")
  expect_true(is.numeric(pred$response))

  # Ensure no missing values are present in the final predictions
  expect_false(any(is.na(pred$response)))
})