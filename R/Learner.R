#' @title Classification BAM Learner
#' @name mlr_learners_classif.bam
#' @importFrom R6 R6Class
#' @importFrom mlr3 LearnerClassif
#' @export
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
            levels = c("binomial", "multinom"),
            default = "binomial",
            tags = "train"
          )
        ),

        # Declare learner capabilities
        properties = c("twoclass", "multiclass")
      )
    }
  ),

  private = list(
    .train = function(task) {
      # Extract training hyperparameters
      pars = self$param_set$get_values(tags = "train")

      # Force multinomial family for multiclass tasks
      if (length(task$class_names) > 2L) {
        pars$family = "multinom"
      }

      # Extract data and formula from the mlr3 task
      data = task$data()
      formula = task$formula()

      # Invoke the fitting algorithm
      mlr3misc::invoke(mgcv::bam, formula = formula, data = data, .args = pars)
    },

    .predict = function(task) {
      # Extract feature data for prediction
      newdata = task$data(cols = task$feature_names)

      # Get raw predictions from the trained model
      model_pred = mlr3misc::invoke(
        predict, self$model, newdata = newdata, type = "response"
      )

      # Convert binary prediction vector to a proper probability matrix for mlr3
      if (is.null(dim(model_pred))) {
        model_pred = matrix(c(1 - model_pred, model_pred), ncol = 2)
        colnames(model_pred) = task$class_names
      }

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