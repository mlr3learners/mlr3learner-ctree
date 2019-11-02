#' @title Classification Conditional Inference Tree Learner
#'
#' @format [R6::R6Class] inheriting from [LearnerClassif].
#'
#' @description
#' A wrapper around ctree (partykit) to use it within mlr3.
#'
#' @references
#' Torsten Hothorn, Achim Zeileis (2015).
#' partykit: A Modular Toolkit for Recursive Partytioning in R.
#' Journal of Machine Learning Research, 16, 3905-3909.
#' \url{http://jmlr.org/papers/v16/hothorn15a.html}
#'
#' Torsten Hothorn, Kurt Hornik and Achim Zeileis (2006).
#' Unbiased Recursive Partitioning: A Conditional Inference Framework.
#' Journal of Computational and Graphical Statistics, 15(3), 651--674,
#' DOI: 10.1198/106186006X133933
#'
#' @export
LearnerClassifCTree = R6Class("LearnerClassifCTree", inherit = LearnerClassif,
  public = list(
    initialize = function() {
      ps = ParamSet$new( # parameter set using the paradox package
        params = list(
          ParamFct$new("teststat", levels = c("quadratic", "maximum"), default = "quadratic", tags = c("train")),
          ParamFct$new("splitstat", levels = c("quadratic", "maximum"), default = "quadratic", tags = c("train")),
          ParamLgl$new("splittest", default = FALSE, tags = c("train")),
          ParamFct$new("testtype", levels = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"), default = "Bonferroni", tags = c("train")),
          #ParamUty$new("pargs"),
          #ParamInt$new("nmax"),
          ParamDbl$new("alpha", lower = 0, upper = 1, default = 0.05, tags = c("train")),
          ParamDbl$new("mincriterion", lower = 0, tags = c("train")),
          ParamDbl$new("logmincriterion", tags = c("train")),
          ParamDbl$new("minsplit", default = 20, tags = c("train")),
          ParamDbl$new("minbucket", default = 7, tags = c("train")),
          ParamDbl$new("minprob", default = 0.01, tags = c("train")),
          ParamLgl$new("stump", default = FALSE, tags = c("train")),
          ParamLgl$new("lookahead", default = FALSE, tags = c("train")),
          ParamLgl$new("MIA", default = FALSE, tags = c("train")),
          ParamInt$new("nresample", default = 9999, tags = c("train")),
          ParamDbl$new("tol", lower = 0, default = 1.490116e-08, tags = c("train")),
          ParamInt$new("maxsurrogate", default = 0, tags = c("train")),
          ParamLgl$new("numsurrogate", default = FALSE, tags = c("train")),
          ParamInt$new("mtry", lower = 0, special_vals = list(Inf), default = Inf, tags = c("train")),
          ParamInt$new("maxdepth", lower = 0, special_vals = list(Inf), default = Inf, tags = c("train")),
          ParamLgl$new("multiway", default = FALSE, tags = c("train")),
          ParamInt$new("splittry", lower = 0, default = 2, tags = c("train")),
          ParamLgl$new("intersplit", default = FALSE, tags = c("train")),
          ParamLgl$new("majority", default = FALSE, tags = c("train")),
          #ParamLgl$new("caseweights", default = FALSE),
          ParamUty$new("applyfun", tags = c("train")),
          ParamInt$new("cores", default = FALSE, special_vals = list(FALSE), tags = c("train")),
          ParamLgl$new("saveinfo", default = TRUE, tags = c("train")),
          ParamLgl$new("update", default = FALSE, tags = c("train")),
          ParamLgl$new("splitflavour", default = FALSE, tags = c("train"))
        )
      )

      ps$add_dep("nresample", "testtype", CondEqual$new("MonteCarlo"))

      super$initialize(
        id = "classif.ctree",
        packages = "partykit",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = c("response"),
        param_set = ps,
        properties = c("weights", "twoclass", "multiclass")
      )
    },

    train_internal = function(task) {
      pars = self$param_set$get_values(tags = "train")
      f = task$formula()
      data = task$data()
      if (is.null(task$weights)) {
        mlr3misc::invoke(partykit::ctree, formula = f, data = data, .args = pars)
      } else {
        weights = task$weights
        mlr3misc::invoke(partykit::ctree, formula = f, data = data, weights = weights, .args = pars)
      }
    },

    predict_internal = function(task) {
      pars = self$param_set$get_values(tags = "predict") # get parameters with tag "predict"
      newdata = task$data(cols = task$feature_names)

      p = mlr3misc::invoke(predict, self$model, newdata = newdata, .args = pars)

      # Return a prediction object with PredictionClassif$new() or PredictionRegr$new()
      PredictionClassif$new(task = task, response = p)
    }
  )
)
