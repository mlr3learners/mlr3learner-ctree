library(mlr3learners.partykit)

test_that("regr.ctree", {
  learner = lrn("regr.ctree")
  fun = partykit::ctree
  exclude = c(
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "\nMissing parameters:\n",
    paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})

test_that("regr.ctree_control", {
  learner = lrn("regr.ctree")
  fun = partykit::ctree_control()
  exclude = c(
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "\nMissing parameters:\n",
    paste0("- '", ParamTest$missing, "'", collapse = "\n")))
})
