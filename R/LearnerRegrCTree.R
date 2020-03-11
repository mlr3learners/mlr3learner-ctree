#' @title Regression Conditional Inference Tree Learner
#'
#' @format [R6::R6Class] inheriting from [LearnerRegr].
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
LearnerRegrCTree = R6Class("LearnerRegrCTree", inherit = LearnerRegr,
  public = list(
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamFct$new("teststat", levels = c("quadratic", "maximum"), default = "quadratic", tags = "train"),
          ParamFct$new("splitstat", levels = c("quadratic", "maximum"), default = "quadratic", tags = "train"),
          ParamLgl$new("splittest", default = FALSE, tags = "train"),
          ParamFct$new("testtype", levels = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"), default = "Bonferroni", tags = "train"),
          ParamUty$new("nmax", tags = "train"),
          ParamDbl$new("alpha", lower = 0, upper = 1, default = 0.05, tags = "train"),
          ParamDbl$new("mincriterion", lower = 0, upper = 1, default = 0.95, tags = "train"),
          ParamDbl$new("logmincriterion", tags = "train"),
          ParamInt$new("minsplit", lower = 1L, default = 20L, tags = "train"),
          ParamInt$new("minbucket", lower = 1L, default = 7L, tags = "train"),
          ParamDbl$new("minprob", lower = 0, default = 0.01, tags = "train"),
          ParamLgl$new("stump", default = FALSE, tags = "train"),
          ParamLgl$new("lookahead", default = FALSE, tags = "train"),
          ParamLgl$new("MIA", default = FALSE, tags = "train"),
          ParamInt$new("nresample", lower = 1L, default = 9999L, tags = "train"),
          ParamDbl$new("tol", lower = 0, tags = "train"),
          ParamInt$new("maxsurrogate", lower = 0L, default = 0L, tags = "train"),
          ParamLgl$new("numsurrogate", default = FALSE, tags = "train"),
          ParamInt$new("mtry", lower = 0L, special_vals = list(Inf), default = Inf, tags = "train"),
          ParamInt$new("maxdepth", lower = 0L, special_vals = list(Inf), default = Inf, tags = "train"),
          ParamLgl$new("multiway", default = FALSE, tags = "train"),
          ParamInt$new("splittry", lower = 0L, default = 2L, tags = "train"),
          ParamLgl$new("intersplit", default = FALSE, tags = "train"),
          ParamLgl$new("majority", default = FALSE, tags = "train"),
          ParamLgl$new("caseweights", default = FALSE, tags = "train"),
          ParamUty$new("applyfun", tags = "train"),
          ParamInt$new("cores", special_vals = list(NULL), default = NULL, tags = "train"),
          ParamLgl$new("saveinfo", default = TRUE, tags = "train"),
          ParamLgl$new("update", default = FALSE, tags = "train"),
          ParamLgl$new("splitflavour", default = FALSE, tags = "train")
        )
      )
      ps$add_dep("nresample", "testtype", CondEqual$new("MonteCarlo"))

      super$initialize(
        id = "regr.ctree",
        packages = "partykit",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = "response",
        param_set = ps,
        properties = c("weights")
      )
    }
  ),
  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      f = task$formula()
      data = task$data()

      if ("weights" %in% task$properties) {
        pars$weights = task$weights$weight
      }

      invoke(partykit::ctree, formula = f, data = data, .args = pars)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)

      p = invoke(predict, self$model, newdata = newdata)
      PredictionRegr$new(task = task, response = p)
    }
  )
)
