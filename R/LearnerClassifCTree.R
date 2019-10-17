#' @title Classification Conditional Inference Tree Learner
#'
#' @format [R6::R6Class] inheriting from [LearnerClassif].
#'
#' @description
#' A [LearnerClassif] for a classification random forest implemented in randomForest::randomForest()] in package \CRANpkg{randomForest}.
#'
#' @references
#' Breiman, L. (2001).
#' Random Forests
#' Machine Learning
#' \url{https://doi.org/10.1023/A:1010933404324}
#'
#' @export
LearnerClassifCTree = R6Class("LearnerClassifCTree", inherit = LearnerClassif,
  public = list(
    initialize = function() {
      ps = ParamSet$new( # parameter set using the paradox package
        params = list(
        )
      )

      super$initialize(
        id = "classif.ctree",
        packages = "partykit",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("weights", "twoclass", "multiclass", "importance", "oob_error")
      )
    },

    train_internal = function(task) {
      # pars = self$param_set$get_values(tags = "train")
      formula = task$formula()
      data = task$data(cols = task$feature_names)
      partykit::ctree(formula = formula, data = data)
    },

    predict_internal = function(task) {
      pars = self$param_set$get_values(tags = "predict") # get parameters with tag "predict"
      newdata = task$data(cols = task$feature_names)

      p = invoke(predict, self$model, newdata = newdata,
        type = type, .args = pars)

      # Return a prediction object with PredictionClassif$new() or PredictionRegr$new()
      if (self$predict_type == "response") {
        PredictionClassif$new(task = task, response = p)
      } else {
        PredictionClassif$new(task = task, prob = p)
      }
    }
  )
)
