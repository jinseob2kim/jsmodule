context("Run mklist and mksetdiff function")


test_that("Run mklist and mksetdiff function", {
  varlist <- list(a = LETTERS[1:6], b = letters[1:6])
  var <- c("a", "b", "A", "B")

  expect_is(mklist(varlist, var), "list")
  expect_is(mksetdiff(varlist, var), "list")
})
