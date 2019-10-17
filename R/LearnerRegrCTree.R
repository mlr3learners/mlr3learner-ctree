#' @title Regression Conditional Inference Tree Learner
#'
#' @format [R6::R6Class] inheriting from [LearnerRegr].
#'
#' @description
#' MISSING.
#'
#' @references
#' Breiman, L. (2001).
#' Random Forests
#' Machine Learning
#' \url{https://doi.org/10.1023/A:1010933404324}
#'
#' @export
LearnerRegrCTree = R6Class("LearnerRegrCTree", inherit = LearnerRegr,
  public = list(
    initialize = function() {
      ps = ParamSet$new( # parameter set using the paradox package
        params = list(
        )
      )

      super$initialize(
        id = "regr.ctree",
        packages = "partykit",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = c("response"),
        param_set = ps,
        properties = c("weights")
      )
    },

    train_internal = function(task) {
      # pars = self$param_set$get_values(tags = "train")
      f = task$formula()
      data = task$data()
      mlr3misc::invoke(partykit::ctree, formula = f, data = data)
    },

    predict_internal = function(task) {
      pars = self$param_set$get_values(tags = "predict") # get parameters with tag "predict"
      newdata = task$data(cols = task$feature_names)

      p = mlr3misc::invoke(predict, self$model, newdata = newdata, .args = pars)

      # Return a prediction object with PredictionClassif$new() or PredictionRegr$new()
      PredictionRegr$new(task = task, response = p)
    }
  )
)
