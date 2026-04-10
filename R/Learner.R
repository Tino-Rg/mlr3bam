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
#'   learner$train(task)
#'
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
      super$initialize(
        id = "classif.bam",
        # Declare required packages for the learner to operate
        packages = c("mlr3learners", "mgcv"), 
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = c("response", "prob"),

        # Define the hyperparameter space (based on mgcv::bam)
        param_set = paradox::ps(
          discrete = paradox::p_lgl(default = TRUE, tags = "train"),
          family = paradox::p_fct(
            levels = c("binomial"),
            default = "binomial",
            tags = "train"
          )
        ),

        # Declare learner capabilities (restricted to binary classification)
        properties = c("twoclass")
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

      # Construct explicit formula to avoid environment scoping issues with '.'
      features = paste(task$feature_names, collapse = " + ")
      target = task$target_names
      form = as.formula(paste(target, "~", features))

      mlr3misc::invoke(mgcv::bam, formula = form, data = data, .args = pars)
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
      super$initialize(
        id = "regr.bam",
        # Declare required packages for the learner to operate
        packages = c("mlr3learners", "mgcv"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = c("response"),

        # Define the hyperparameter space (based on mgcv::bam)
        param_set = paradox::ps(
          discrete = paradox::p_lgl(default = TRUE, tags = "train"),
          family = paradox::p_fct(
            levels = c("gaussian", "poisson"),
            default = "gaussian",
            tags = "train"
          )
        ),

        properties = character(0)
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")

      # Force data to a standard data.frame for mgcv compatibility
      data = as.data.frame(task$data())

      # Construct explicit formula to avoid environment scoping issues with '.'
      features = paste(task$feature_names, collapse = " + ")
      target = task$target_names
      form = as.formula(paste(target, "~", features))

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