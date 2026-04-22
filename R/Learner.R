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
#'     k = 5,
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
        k = paradox::p_int(lower = -1L, default = -1L, tags = "train"),
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

      # Extract spline dimension parameter 'k'.
      # It is removed from 'pars' as it is injected directly into the formula
      k_val = pars$k
      if (is.null(k_val)) k_val = -1
      pars$k = NULL

      # Dynamic formula construction:
      # In generalized additive models, continuous features are smoothed
      # using s() splines, while discrete/categorical features are included
      # as linear parametric terms.
      feat_types = task$feature_types
      num_feats = feat_types$id[feat_types$type %in% c("numeric", "integer")]
      fct_feats = feat_types$id[feat_types$type %in% c("factor", "logical")]

      if (length(num_feats) > 0) {
        num_terms = sprintf("s(%s, k=%d)", num_feats, k_val)
      } else {
        num_terms = character(0)
      }

      all_terms = c(num_terms, fct_feats)

      if (length(all_terms) == 0) {
        # Fallback for featureless tasks (intercept only)
        form_string = paste(task$target_names, "~ 1")
      } else {
        form_string = paste(
          task$target_names, "~", paste(all_terms, collapse = " + ")
        )
      }

      form = as.formula(form_string)

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
#'     k = 3,
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
        k = paradox::p_int(lower = -1L, default = -1L, tags = "train"),
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

      # Extract spline dimension parameter 'k'.
      # It is removed from 'pars' as it is injected directly into the formula
      k_val = pars$k
      if (is.null(k_val)) k_val = -1
      pars$k = NULL

      # Dynamic formula construction:
      # In generalized additive models, continuous features are smoothed
      # using s() splines, while discrete/categorical features are included
      # as linear parametric terms.
      feat_types = task$feature_types
      num_feats = feat_types$id[feat_types$type %in% c("numeric", "integer")]
      fct_feats = feat_types$id[feat_types$type %in% c("factor", "logical")]

      if (length(num_feats) > 0) {
        num_terms = sprintf("s(%s, k=%d)", num_feats, k_val)
      } else {
        num_terms = character(0)
      }

      all_terms = c(num_terms, fct_feats)

      if (length(all_terms) == 0) {
        # Fallback for featureless tasks (intercept only)
        form_string = paste(task$target_names, "~ 1")
      } else {
        form_string = paste(
          task$target_names, "~", paste(all_terms, collapse = " + ")
        )
      }

      form = as.formula(form_string)

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