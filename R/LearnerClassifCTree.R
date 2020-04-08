#' @title Classification Conditional Inference Tree Learner
#'
#' @name mlr_learners_classif.ctree
#'
#' @description
#' Classification conditional inference tree learner.
#' Calls [partykit::ctree()] from package \CRANpkg{partykit}.
#'
#' @templateVar id classif.ctree
#' @template section_dictionary_learner
#'
#' @references
#' \cite{mlr3learners.partykit}{partykit1}
#' \cite{mlr3learners.partykit}{partykit2}
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerClassifCTree = R6Class("LearnerClassifCTree",
  inherit = LearnerClassif,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamFct$new("teststat", levels = c("quadratic", "maximum"),
            default = "quadratic", tags = "train"),
          ParamFct$new("splitstat", levels = c("quadratic", "maximum"),
            default = "quadratic", tags = "train"),
          ParamLgl$new("splittest", default = FALSE, tags = "train"),
          ParamFct$new("testtype", levels = c("Bonferroni", "MonteCarlo",
            "Univariate", "Teststatistic"), default = "Bonferroni",
          tags = "train"),
          ParamUty$new("nmax", tags = "train"),
          ParamDbl$new("alpha", lower = 0, upper = 1, default = 0.05,
            tags = "train"),
          ParamDbl$new("mincriterion", lower = 0, upper = 1, default = 0.95,
            tags = "train"),
          ParamDbl$new("logmincriterion", tags = "train"),
          ParamInt$new("minsplit", lower = 1L, default = 20L, tags = "train"),
          ParamInt$new("minbucket", lower = 1L, default = 7L, tags = "train"),
          ParamDbl$new("minprob", lower = 0, default = 0.01, tags = "train"),
          ParamLgl$new("stump", default = FALSE, tags = "train"),
          ParamLgl$new("lookahead", default = FALSE, tags = "train"),
          ParamLgl$new("MIA", default = FALSE, tags = "train"),
          ParamInt$new("nresample", lower = 1L, default = 9999L,
            tags = "train"),
          ParamDbl$new("tol", lower = 0, tags = "train"),
          ParamInt$new("maxsurrogate", lower = 0L, default = 0L,
            tags = "train"),
          ParamLgl$new("numsurrogate", default = FALSE, tags = "train"),
          ParamInt$new("mtry", lower = 0L, special_vals = list(Inf),
            default = Inf, tags = "train"),
          ParamInt$new("maxdepth", lower = 0L, special_vals = list(Inf),
            default = Inf, tags = "train"),
          ParamLgl$new("multiway", default = FALSE, tags = "train"),
          ParamInt$new("splittry", lower = 0L, default = 2L, tags = "train"),
          ParamLgl$new("intersplit", default = FALSE, tags = "train"),
          ParamLgl$new("majority", default = FALSE, tags = "train"),
          ParamLgl$new("caseweights", default = FALSE, tags = "train"),
          ParamUty$new("applyfun", tags = "train"),
          ParamInt$new("cores", special_vals = list(NULL), default = NULL,
            tags = "train"),
          ParamLgl$new("saveinfo", default = TRUE, tags = "train"),
          ParamLgl$new("update", default = FALSE, tags = "train"),
          ParamFct$new("splitflavour", default = "ctree",
            levels = c("ctree", "exhaustive"), tags = c("train", "control")),
          ParamUty$new("offset", tags = "train"),
          ParamUty$new("cluster", tags = "train"),
          ParamUty$new("scores", tags = "train"),
          ParamLgl$new("doFit", default = TRUE, tags = "train"),
          ParamUty$new("pargs", default = mvtnorm::GenzBretz, tags = "train")
        )
      )
      ps$add_dep("nresample", "testtype", CondEqual$new("MonteCarlo"))

      super$initialize(
        id = "classif.ctree",
        packages = "partykit",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("weights", "twoclass", "multiclass"),
        man = "mlr3learners.partykit::mlr_learners_classif.ctree"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")

      if ("weights" %in% task$properties) {
        pars$weights = task$weights$weight
      }

      invoke(partykit::ctree, formula = task$formula(), data = task$data(),
        .args = pars)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)

      if (self$predict_type == "response") {
        response = invoke(predict, self$model, newdata = newdata,
          type = "response")
        PredictionClassif$new(task = task, response = response)
      } else {
        prob = invoke(predict, self$model, newdata = newdata, type = "prob")
        PredictionClassif$new(task = task, prob = prob)
      }
    }
  )
)
