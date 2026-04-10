library(testthat)

# Classification Tests
test_that("LearnerClassifBam trains and predicts correctly", {
  skip_if_not_installed("mgcv")

  learner = LearnerClassifBam$new()
  task = mlr3::tsk("sonar")

  learner$param_set$set_values(
    formula = "Class ~ s(V1, k=5) + s(V2, k=5)",
    discrete = TRUE,
    nthreads = 1
  )

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

  learner$param_set$set_values(
    formula = "mpg ~ s(hp, k=3) + cyl",
    method = "fREML"
  )

  # Verify that training executes without errors or warnings
  expect_silent(learner$train(task))

  pred = learner$predict(task)

  # Validate the structure and type of the prediction object
  expect_true(inherits(pred, "PredictionRegr"))
  expect_equal(pred$task_type, "regr")
  expect_true(is.numeric(pred$response))

  # Ensure no missing values are present in the final predictions
  expect_false(any(is.na(pred$response)))
})