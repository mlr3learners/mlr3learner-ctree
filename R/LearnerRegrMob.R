#' @title Regression Model-based Recursive Partitioning Learner
#'
#' @name mlr_learners_regr.mob
#'
#' @description
#' Regression model-based recursive partitioning.
#' Calls [partykit::mob()] from package \CRANpkg{partykit}.
#' FIXME: this learner is not straightforward to use; needs gallery post or detailed description.
#'
#' @templateVar id regr.mob
#' @template section_dictionary_learner
#'
#' @references
#' \cite{mlr3learners.partykit}{partykit1}
#' \cite{mlr3learners.partykit}{partykit2}
#' \cite{mlr3learners.partykit}{partykit3}
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerRegrMob = R6Class("LearnerRegrMob", inherit = LearnerRegr,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        # missing: subset, na.action, weights (see bottom), offset, cluster
        ParamUty$new("rhs", custom_check = check_character, tags = "train"),
        ParamUty$new("fit", custom_check = function(x) check_function(x, args = c("y", "x", "start", "weights", "offset", "...")), tags = "train"),
        # all in mob_control()
        ParamDbl$new("alpha", default = 0.05, lower = 0, upper = 1, tags = "train"),
        ParamLgl$new("bonferroni", default = TRUE, tags = "train"),
        ParamInt$new("minsize", lower = 1L, tags = "train"), # minsize, minsplit, minbucket are equivalent, actually adaptive default
        ParamInt$new("minsplit", lower = 1L, tags = "train"),
        ParamInt$new("minbucket", lower = 1L, tags = "train"),
        ParamInt$new("maxdepth", default = Inf, lower = 0L, special_vals = list(Inf), tags = "train"),
        ParamInt$new("mtry", default = Inf, lower = 0L, special_vals = list(Inf), tags = "train"),
        ParamDbl$new("trim", default = 0.1, lower = 0, tags = "train"),
        ParamLgl$new("breakties", default = FALSE, tags = "train"),
        ParamUty$new("parm", tags = "train"),
        ParamInt$new("dfsplit", lower = 0L, tags = "train"),
        ParamUty$new("prune", tags = "train"),
        ParamLgl$new("restart", default = TRUE, tags = "train"),
        ParamLgl$new("verbose", default = FALSE, tags = "train"),
        ParamLgl$new("caseweights", default = TRUE, tags = "train"),
        ParamFct$new("ytype", default = "vector", levels = c("vector", "matrix", "data.frame"), tags = "train"),
        ParamFct$new("xtype", default = "matrix", levels = c("vector", "matrix", "data.frame"), tags = "train"),
        ParamUty$new("terminal", default = "object", tags = "train"),
        ParamUty$new("inner", default = "object", tags = "train"),
        ParamLgl$new("model", default = TRUE, tags = "train"),
        ParamFct$new("numsplit", default = "left", levels = c("left", "center"), tags = "train"),
        ParamFct$new("catsplit", default = "binary", levels = c("binary", "multiway"), tags = "train"),
        ParamFct$new("vcov", default = "opg", levels = c("opg", "info", "sandwich"), tags = "train"),
        ParamFct$new("ordinal", default = "chisq", levels = c("chisq", "max", "L2"), tags = "train"),
        ParamInt$new("nrep", default = 10000, lower = 0L, tags = "train"),
        ParamUty$new("applyfun", tags = "train"),
        ParamInt$new("cores", default = NULL, special_vals = list(NULL), tags = "train"),
        # additional arguments passed to fitting function
        ParamUty$new("additional", custom_check = check_list, tags = "train"),
        # the predict function depends on the predict method of the fitting function itself and can be passed via type, see predict.modelparty
        # most fitting functions should not need anything else than the model itself, the newdata, the original task and a predict type
        ParamUty$new("predict_fun", custom_check = function(x) check_function(x, args = c("object", "newdata", "task", ".type")), tags = "predict")
        )
      )

      ps$add_dep("nrep", on = "ordinal", cond = CondEqual$new("L2"))

      super$initialize(
        id = "regr.mob",
        param_set = ps,
        predict_types = c("response", "se"), # depends on the fitting function itself
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"), # depends on the fitting function itself
        properties = "weights",
        packages = "partykit",
        man = "mlr3learners.partykit::mlr_learners_regr.mob"
      )
    }
  ),

  private = list(
    .train = function(task) {
      # FIXME: check if rhs variables are present in data?
      formula = task$formula(self$param_set$values$rhs)
      pars = self$param_set$get_values(tags = "train")
      pars_control = pars[which(names(pars) %in% formalArgs(partykit::mob_control))] 
      pars_additional = self$param_set$get_values(tags = "additional")
      pars = pars[names(pars) %nin% c("rhs", names(pars_control), names(pars_additional))]
      control = invoke(partykit::mob_control, .args = pars_control)
      if ("weights" %in% task$properties) { # weights are handled here
        pars = insert_named(pars, list(weights = task$weights$weight))
      }
      # append the additional parameters to be passed to the fitting function
      pars = append(pars, unlist(unname(pars_additional)))

      # FIXME: contrasts?
      invoke(partykit::mob,
        formula = formula,
        data = task$data(),
        control = control,
        .args = pars
      )
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)
      # type is the type argument passed to predict.modelparty (actually a predict function used to compute the predictions as we want)
      # this should return a two column matrix holding the responses in the first and the standard errors (or NA) in the second column
      # .type is then the actual predict type as set for the learner
      preds = invoke(predict, object = self$model, newdata = newdata, type = self$param_set$values$predict_fun, task = task, .type = self$predict_type)
      if (self$predict_type == "response") {
        PredictionRegr$new(task = task, response = preds[, 1L])
      } else {
        PredictionRegr$new(task = task, response = preds[, 1L], se = preds[, 2L])
      }
    }
  )
)
