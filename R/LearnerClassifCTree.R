#' @title Classification Conditional Inference Tree Learner
#'
#' @name mlr_learners_classif.ctree
#'
#' @description
#' Calls [partykit::ctree()] from package \CRANpkg{partykit}.
#'
#' @section Dictionary:
#' This [mlr3::LearnerClassif] can be instantiated via the [dictionary][mlr3misc::Dictionary] [mlr3::mlr_learners] or with the associated sugar function [mlr3::lrn()]:
#' ```
#' mlr_learners$get("classif.ctree")
#' lrn("classif.ctree")
#' ```
#'
#' @references
#' \cite{mlr3learners.ctree}{partykit1}
#' \cite{mlr3learners.ctree}{partykit2}
#'
#' @export
LearnerClassifCTree = R6Class("LearnerClassifCTree", inherit = LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new( # parameter set using the paradox package
        params = list(
        )
      )

      super$initialize(
        id = "classif.ctree",
        packages = "partykit",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = c("response"),
        param_set = ps,
        properties = c("weights", "twoclass", "multiclass")
      )
    }
  ),

  private = list(
    .train = function(task) {
      # pars = self$param_set$get_values(tags = "train")
      f = task$formula()
      data = task$data()
      mlr3misc::invoke(partykit::ctree, formula = f, data = data)
    },

    .predict = function(task) {
      pars = self$param_set$get_values(tags = "predict") # get parameters with tag "predict"
      newdata = task$data(cols = task$feature_names)

      p = mlr3misc::invoke(predict, self$model, newdata = newdata, .args = pars)

      # Return a prediction object with PredictionClassif$new() or PredictionRegr$new()
      PredictionClassif$new(task = task, response = p)
    }
  )
)
