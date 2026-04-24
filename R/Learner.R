#' @title Classification Fast Generalized Additive Model Learner
#' @author Tino-Rg
#' @name mlr_learners_classif.bam
#'
#' @description
#' Fast generalized additive models for large datasets (BAM).
#' Calls `mgcv::bam()` from package \CRANpkg{mgcv} with `family` set to `binomial`.
#'
#' @section Formula:
#' A gam formula specific to the task at hand is required for the `formula`
#' parameter (see example and `?mgcv::formula.gam`). Beware, if no formula is provided, a fallback formula is
#' used that will make the model behave like a glm (this behavior is required
#' for the unit tests). Only features specified in the formula will be used,
#' superseding columns with col_roles "feature" in the task.
#'
#'
#' @references
#' `r format_bib("hastie2017generalized", "wood2012mgcv")`
#'
#' @export
#' @examples
#' if (requireNamespace("mgcv", quietly = TRUE)) {
#'   task = mlr3::tsk("sonar")
#'
#'   learner = lrn("classif.bam")
#'
#'   learner$param_set$set_values(
#'     formula = Class ~ s(V1, k = 5) + s(V2, k = 4) + V3,
#'     method = "fREML"
#'   )
#'
#'   learner$train(task)
#'   print(learner$model)
#'
#'   pred = learner$predict(task)
#'   print(pred)
#' }
LearnerClassifBam <- R6::R6Class(
  "LearnerClassifBam",
  inherit = mlr3::LearnerClassif,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = paradox::ps(
        formula = paradox::p_uty(tags = "train"),
        discrete = paradox::p_lgl(default = TRUE, tags = "train"),
        method = paradox::p_fct(
          levels = c("fREML", "REML", "GCV.Cp"), default = "fREML",
          tags = "train"
        ),
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

      data = task$data(cols = c(task$feature_names, task$target_names))

      # On force la famille binomiale pour la classification
      pars$family = "binomial"

      # Fallback vers un modèle linéaire classique si pas de formule
      if (is.null(pars$formula)) {
        formula_str = paste(
          task$target_names,
          "~",
          paste(task$feature_names, collapse = " + ")
        )
        pars$formula = as.formula(formula_str)
      }

      mlr3misc::invoke(mgcv::bam, data = data, .args = pars)
    },

    .predict = function(task) {
      newdata = mlr3extralearners:::ordered_features(task, self)

      model_pred = mlr3misc::invoke(
        predict, self$model, newdata = newdata, type = "response"
      )

      model_pred = as.numeric(model_pred)
      model_pred = matrix(c(1 - model_pred, model_pred), ncol = 2)
      colnames(model_pred) = task$class_names

      if (self$predict_type == "response") {
        class_indices = max.col(model_pred, ties.method = "random")
        response = colnames(model_pred)[class_indices]
        list(response = unname(response))
      } else {
        list(prob = model_pred)
      }
    }
  )
)

#.extralrns_dict$add("classif.bam", LearnerClassifBam)



#' @title Regression Fast Generalized Additive Model Learner
#' @author Tino-Rg
#' @name mlr_learners_regr.bam
#'
#' @description
#' Fast generalized additive models for large datasets (BAM).
#' Calls `mgcv::bam()` from package \CRANpkg{mgcv}.
#'
#' @section Formula:
#' A gam formula specific to the task at hand is required for the `formula`
#' parameter (see example and `?mgcv::formula.gam`). Beware, if no formula is provided, a fallback formula is
#' used that will make the model behave like a glm (this behavior is required
#' for the unit tests). Only features specified in the formula will be used,
#' superseding columns with col_roles "feature" in the task.
#'
#' @references
#' `r format_bib("hastie2017generalized", "wood2012mgcv")`
#'
#' @export
#' @examples
#' if (requireNamespace("mgcv", quietly = TRUE)) {
#'   task = mlr3::tsk("mtcars")
#'
#'   learner = lrn("regr.bam")
#'
#'   learner$param_set$set_values(
#'     formula = mpg ~ s(disp, k = 3) + s(hp, k = 4) + cyl,
#'     method = "fREML"
#'   )
#'
#'   learner$train(task)
#'   print(learner$model)
#'
#'   pred = learner$predict(task)
#'   print(pred)
#' }
LearnerRegrBam <- R6::R6Class(
  "LearnerRegrBam",
  inherit = mlr3::LearnerRegr,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = paradox::ps(
        formula = paradox::p_uty(tags = "train"),
        discrete = paradox::p_lgl(default = TRUE, tags = "train"),
        family = paradox::p_fct(
          levels = c("gaussian", "poisson"), default = "gaussian",
          tags = "train"
        ),
        method = paradox::p_fct(
          levels = c("fREML", "REML"), default = "fREML",
          tags = "train"
        ),
        nthreads = paradox::p_int(lower = 1L, default = 1L, tags = "train")
      )

      super$initialize(
        id = "regr.bam",
        packages = c("mgcv"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = c("response"),
        param_set = ps,
        label = "Fast Generalized Additive Model (BAM) Regression",
        man = "mlr3bam::mlr_learners_regr.bam"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")

      data = task$data(cols = c(task$feature_names, task$target_names))

      if (is.null(pars$formula)) {
        formula_str = paste(
          task$target_names,
          "~",
          paste(task$feature_names, collapse = " + ")
        )
        pars$formula = as.formula(formula_str)
      }

      mlr3misc::invoke(mgcv::bam, data = data, .args = pars)
    },

    .predict = function(task) {
      newdata = mlr3extralearners:::ordered_features(task, self)

      response = mlr3misc::invoke(
        predict, self$model, newdata = newdata, type = "response"
      )

      list(response = unname(response))
    }
  )
)

#.extralrns_dict$add("regr.bam", LearnerRegrBam)
