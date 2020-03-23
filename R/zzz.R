#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom mlr3 mlr_learners LearnerClassif LearnerRegr
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start
  # get mlr_learners dictionary from the mlr3 namespace
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")

  # add the learner to the dictionary
  x$add("classif.ctree", LearnerClassifCTree)
  x$add("regr.ctree", LearnerRegrCTree)
  x$add("classif.mob", LearnerClassifMob)
  x$add("regr.mob", LearnerRegrMob)
} # nocov end
