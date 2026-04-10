library(testthat)

# Classification Tests
test_that("LearnerClassifBam trains and predicts correctly", {
  skip_if_not_installed("mgcv")

  learner = LearnerClassifBam$new()
  task = mlr3::tsk("sonar")

  # Verify that training executes without errors or warnings
  expect_silent(learner$train(task))

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

  # Verify that training executes without errors or warnings
  expect_silent(learner$train(task))

  pred = learner$predict(task)

  # Validate the structure and type of the prediction object
  expect_true(inherits(pred, "PredictionRegr"))
  expect_equal(pred$task_type, "regr")

  # Ensure no missing values are present in the final predictions
  expect_false(any(is.na(pred$response)))
})