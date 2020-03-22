#' @title Regression Conditional Random Forest Learner
#'
#' @name mlr_learners_regr.cforest
#'
#' @description
#' Regression conditional random forest learner.
#' Calls [partykit::cforest()] from package \CRANpkg{partykit}.
#'
#' @templateVar id regr.cforest
#' @template section_dictionary_learner
#'
#' @references
#' \cite{mlr3learners.partykit}{partykit1}
#' \cite{mlr3learners.partykit}{partykit2}
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerRegrCForest = R6Class("LearnerRegrCForest", inherit = LearnerRegr,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        # missing: weights (see bottom), subset, offset, cluster, strata (see FIXME: below), na.action, ytrafo, scores
        ParamInt$new("ntree", default = 500L, lower = 1L, tags = "train"),
        # replace and fraction go in perturb (named list)
        ParamLgl$new("replace", default = FALSE, tags = c("train", "perturb")),
        ParamDbl$new("fraction", default = 0.632, lower = 0, upper = 1, tags = c("train", "perturb")),
        ParamInt$new("mtry", lower = 0L, special_vals = list(Inf), tags = "train"), # actually has an adaptive default of "ceiling(sqrt(nvar))"
        ParamUty$new("applyfun", tags = "train"),
        ParamInt$new("cores", default = NULL, special_vals = list(NULL), tags = "train"),
        ParamLgl$new("trace", default = FALSE, tags = "train"),


        # all in ctree_control(); missing: mtry, applyfun, cores (see above, passed directly)
        ParamFct$new("teststat", default = "quadratic", levels = c("quadratic", "maximum"), tags = c("train", "control")),
        ParamFct$new("splitstat", default = "quadratic", levels = c("quadratic", "maximum"), tags = c("train", "control")),
        ParamLgl$new("splittest", default = FALSE, tags = c("train", "control")),
        ParamFct$new("testtype", default = "Univariate", levels = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"), tags = c("train", "control")),
        ParamUty$new("nmax", tags = c("train", "control")),

        # pargs arguments as a list for GenzBretz() within ctree_control; currently not used
        #ParamInt$new("maxpts", default = 25000L, lower = 1L, tags = c("train", "control")),
        #ParamDbl$new("abseps", default = 0.001, lower = 0, tags = c("train", "control")),
        #ParamDbl$new("releps", default = 0, tags = c("train", "control")),

        ParamDbl$new("alpha", default = 0.05, lower = 0, upper = 1, tags = c("train", "control")),
        ParamDbl$new("mincriterion", default = 0, lower = 0, upper = 1, tags = c("train", "control")),
        ParamDbl$new("logmincriterion", default = 0, tags = c("train", "control")),
        ParamInt$new("minsplit", lower = 1L, default = 20L, tags = c("train", "control")),
        ParamInt$new("minbucket", lower = 1L, default = 7L, tags = c("train", "control")),
        ParamDbl$new("minprob", default = 0.01, lower = 0, upper = 1, tags = c("train", "control")),
        ParamLgl$new("stump", default = FALSE, tags = c("train", "control")),
        ParamLgl$new("lookahead", default = FALSE, tags = c("train", "control")),
        ParamLgl$new("MIA", default = FALSE, tags = c("train", "control")),
        ParamInt$new("nresample", default = 9999L, lower = 1L, tags = c("train", "control")),
        ParamDbl$new("tol", default = sqrt(.Machine$double.eps), lower = 0, tags = c("train", "control")),
        ParamInt$new("maxsurrogate", default = 0L, lower = 0L, tags = c("train", "control")),
        ParamLgl$new("numsurrogate", default = FALSE, tags = c("train", "control")),
        ParamInt$new("maxdepth", default = Inf, lower = 0L, special_vals = list(Inf), tags = c("train", "control")),
        ParamLgl$new("multiway", default = FALSE, tags = c("train", "control")),
        ParamInt$new("splittry", default = 2L, lower = 0L, tags = c("train", "control")),
        ParamLgl$new("intersplit", default = FALSE, tags = c("train", "control")),
        ParamLgl$new("majority", default = FALSE, tags = c("train", "control")),
        ParamLgl$new("caseweights", default = TRUE, tags = c("train", "control")),
        ParamLgl$new("saveinfo", default = FALSE, tags = c("train", "control")),
        ParamLgl$new("update", default = FALSE, tags = c("train", "control")),
        ParamFct$new("splitflavour", default = "ctree", levels = c("ctree", "exhaustive"), tags = c("train", "control")),

        # predict; missing FUN and simplify (not needed here)
        ParamLgl$new("OOB", default = FALSE, tags = "predict"),
        ParamLgl$new("simplify", default = TRUE, tags = "predict"),
        ParamLgl$new("scale", default = TRUE, tags = "predict")
        )
      )

      ps$add_dep("nresample", on = "testtype", cond = CondEqual$new("MonteCarlo"))

      # set the cforest specific ctree_control parameters
      ps$values$teststat = "quadratic"
      ps$values$testtype = "Univariate"
      ps$values$mincriterion = 0
      ps$values$saveinfo = FALSE

      super$initialize(
        id = "regr.cforest",
        param_set = ps,
        predict_types = "response", # cforest can also do weights, node, but not really useful here
        feature_types = c("integer", "numeric", "factor", "ordered"),
        properties = c("weights", "oob_error"), # FIXME: importance after bug fixes, strata?
        packages = "partykit",
        man = "mlr3learners.partykit::mlr_learners_regr.cforest"
      )
    },

    # #' @description
    # #' The importance scores are calculated using `partykit::varimp()`.
    # #'
    # #' @return Named `numeric()`.
    # FIXME: argument checking here, move the params into ps?; Achim bug fix needed (sometimes varimp returns a vector of length 0)
    #importance = function(nperm = 1L, OOB = TRUE, risk = "loglik", conditional = FALSE, threshold = .2, applyfun = NULL, cores = NULL, ...) {
    #  if (is.null(self$model)) {
    #    stopf("No model stored")
    #  }
    #  sort(partykit::varimp(object = self$model, nperm = nperm, OOB = OOB, risk = risk, conditional = conditional, threshold = threshold, applyfun = applyfun, cores = cores, ...), decreasing = TRUE)
    #},

    #' @description
    #' The out-of-bag error, calculated using the OOB predictions from `partykit`.
    #'
    #' @return `numeric(1)`.
    oob_error = function() {
      preds = invoke(predict, object = self$model, newdata = NULL, type = "response", OOB = TRUE, FUN = NULL, simplify = TRUE, scale = TRUE)
      mean((self$model$data[[as.character(attr(self$model$data, which = "terms")[[2L]])]] - preds) ^ 2L)
    }
  ),

  private = list(
    .train = function(task) {
      # perturb parameters need special handling; FIXME: easier way?
      pars = self$param_set$get_values(tags = "train")
      pars_control = self$param_set$get_values(tags = "control")
      pars = pars[names(pars) %nin% c("replace", "fraction", names(pars_control))]
      control = invoke(partykit::ctree_control, .args = pars_control)
      perturb = list(replace = FALSE, fraction = 0.632)
      if (!is.null(self$param_set$values$replace)) perturb$replace = self$param_set$values$replace
      if (!is.null(self$param_set$values$fraction)) perturb$fraction = self$param_set$values$fraction

      invoke(partykit::cforest,
        formula = task$formula(),
        data = task$data(),
        weights = task$weights$weight, # weights are handled here
        # FIXME: strata handling
        control = control,
        perturb = perturb,
        .args = pars
      )
    },

    .predict = function(task) {
      pars = self$param_set$get_values(tags = "predict")
      newdata = task$data(cols = task$feature_names)
      preds = invoke(predict, object = self$model, newdata = newdata, type = self$predict_type, .args = pars)
      PredictionRegr$new(task = task, response = preds)
    }
  )
)
