context("Test module")


test_that("Run Modules", {

  expect_is(coxUI(1), "list")
  expect_warning(coxModule(1, 1, 1, 1, 1))
  expect_is(FilePsInput(1), "list")
  expect_is(csvFileInput(1), "list")

})




