library(testthat)

# Classification Tests
test_that("LearnerClassifBam trains and predicts correctly", {
  skip_if_not_installed("mgcv")

  learner = LearnerClassifBam$new()
  task = mlr3::tsk("sonar")

  learner$param_set$set_values(
    formula = Class ~ s(V1, k = 5) + s(V2, k = 4) + V3,
    method = "fREML"
  )

  expect_error(suppressWarnings(learner$train(task)), NA)

  pred = learner$predict(task)

  expect_true(inherits(pred, "PredictionClassif"))
  expect_equal(pred$task_type, "classif")

  expect_false(any(is.na(pred$response)))
})

# Regression Tests
test_that("LearnerRegrBam trains and predicts correctly", {
  skip_if_not_installed("mgcv")

  learner = LearnerRegrBam$new()
  task = mlr3::tsk("mtcars")

  learner$param_set$set_values(
    formula = mpg ~ s(disp, k = 3) + s(hp, k = 4) + cyl,
    method = "fREML"
  )

  expect_error(suppressWarnings(learner$train(task)), NA)

  pred = learner$predict(task)

  expect_true(inherits(pred, "PredictionRegr"))
  expect_equal(pred$task_type, "regr")
  expect_true(is.numeric(pred$response))

  expect_false(any(is.na(pred$response)))
})