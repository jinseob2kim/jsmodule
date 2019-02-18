context("Test module")


test_that("Run Modules", {

  expect_is(coxUI(1), "list")
  expect_warning(coxModule(1, 1, 1, 1, 1))


})
