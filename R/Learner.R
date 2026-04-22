#' @title Classification BAM Learner
#' @name mlr_learners_classif.bam
#' @importFrom R6 R6Class
#' @importFrom mlr3 LearnerClassif
#' @export
#' @examples
#' if (requireNamespace("mgcv", quietly = TRUE)) {
#'   task = mlr3::tsk("sonar")
#'
#'   learner = LearnerClassifBam$new()
#'
#'   learner$param_set$set_values(
#'     formula = "Class ~ s(V1, k=5) + V2",
#'     discrete = TRUE,
#'     nthreads = 1
#'   )
#'
#'   learner$train(task)
#'   pred = learner$predict(task)
#'   print(pred)
#' }
LearnerClassifBam <- R6::R6Class(
  "LearnerClassifBam",
  inherit = mlr3::LearnerClassif,

  public = list(
    #' @description
    #' Creates a new instance of this R6 class.
    initialize = function() {
      ps = paradox::ps(
        discrete = paradox::p_lgl(default = TRUE, tags = "train"),
        method = paradox::p_fct(
          levels = c("fREML", "REML", "GCV.Cp"), default = "fREML",
          tags = "train"
        ),
        formula = paradox::p_uty(default = NULL, tags = "train"),
        nthreads = paradox::p_int(lower = 1L, default = 1L, tags = "train"),
        select = paradox::p_lgl(default = FALSE, tags = "train")
      )

      super$initialize(
        id = "classif.bam",
        packages = c("mgcv"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass"),
        label = "Fast Generalized Additive Model (BAM)",
        man = "mlr3bam::mlr_learners_classif.bam"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")

      # Force data to a standard data.frame for mgcv compatibility
      data = as.data.frame(task$data())

      # Convert binary target factor to a 0/1 numeric vector
      # as required by bam()
      data[[task$target_names]] = as.numeric(
        data[[task$target_names]] == task$class_names[2]
      )

      if (is.null(pars$formula)) {
        features = paste(task$feature_names, collapse = " + ")
        form = as.formula(paste(task$target_names, "~", features))
      } else {
        form = as.formula(pars$formula)
      }
      pars$formula = NULL

      mlr3misc::invoke(
        mgcv::bam,
        formula = form,
        data = data,
        family = "binomial",
        .args = pars
      )
    },

    .predict = function(task) {
      # Extract feature data and force standard data.frame format
      newdata = as.data.frame(task$data(cols = task$feature_names))

      model_pred = mlr3misc::invoke(
        predict, self$model, newdata = newdata, type = "response"
      )

      # Ensure predictions are numeric
      # and construct the two-column probability matrix
      model_pred = as.numeric(model_pred)
      model_pred = matrix(c(1 - model_pred, model_pred), ncol = 2)
      colnames(model_pred) = task$class_names

      # Format the output based on the requested prediction type
      if (self$predict_type == "response") {
        # Extract the class with the highest probability
        class_indices = max.col(model_pred, ties.method = "random")
        response = colnames(model_pred)[class_indices]
        list(response = unname(response))
      } else {
        # Return the probability matrix directly
        list(prob = model_pred)
      }
    }
  )
)

#' @title Regression BAM Learner
#' @name mlr_learners_regr.bam
#' @importFrom R6 R6Class
#' @importFrom mlr3 LearnerRegr
#' @export
#' @examples
#' if (requireNamespace("mgcv", quietly = TRUE)) {
#'   task = mlr3::tsk("mtcars")
#'
#'   learner = LearnerRegrBam$new()
#'
#'   learner$param_set$set_values(
#'     formula = "mpg ~ s(hp, k=3) + cyl",
#'     method = "fREML"
#'   )
#'
#'   learner$train(task)
#'
#'   pred = learner$predict(task)
#'   print(pred)
#' }
LearnerRegrBam <- R6::R6Class(
  "LearnerRegrBam",
  inherit = mlr3::LearnerRegr,

  public = list(
    #' @description
    #' Creates a new instance of this R6 class.
    initialize = function() {
      ps = paradox::ps(
        discrete = paradox::p_lgl(default = TRUE, tags = "train"),
        family = paradox::p_fct(
          levels = c("gaussian", "poisson"), default = "gaussian",
          tags = "train"
        ),
        method = paradox::p_fct(
          levels = c("fREML", "REML"), default = "fREML",
          tags = "train"
        ),
        formula = paradox::p_uty(default = NULL, tags = "train"),
        nthreads = paradox::p_int(lower = 1L, default = 1L, tags = "train")
      )

      super$initialize(
        id = "regr.bam",
        packages = c("mgcv"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = c("response"),
        param_set = ps,
        label = "Fast Generalized Additive Model (BAM) Regression"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")

      # Force data to a standard data.frame for mgcv compatibility
      data = as.data.frame(task$data())

      if (is.null(pars$formula)) {
        features = paste(task$feature_names, collapse = " + ")
        form = as.formula(paste(task$target_names, "~", features))
      } else {
        form = as.formula(pars$formula)
      }
      pars$formula = NULL

      # Safely invoke the fitting algorithm
      mlr3misc::invoke(mgcv::bam, formula = form, data = data, .args = pars)
    },

    .predict = function(task) {
      # Extract feature data and force standard data.frame format
      newdata = as.data.frame(task$data(cols = task$feature_names))

      response = mlr3misc::invoke(
        predict, self$model, newdata = newdata, type = "response"
      )

      # Return the formatted response for mlr3
      list(response = unname(response))
    }
  )
)