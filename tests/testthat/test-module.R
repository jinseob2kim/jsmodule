context("Test module")


test_that("Run Modules", {

  expect_is(coxUI(1), "list")
  expect_warning(coxModule(1, 1, 1, 1, 1))
  expect_is(FilePsInput(1), "list")
  expect_is(csvFileInput(1), "list")
  expect_is(FileRepeatedInput(1), "list")
  expect_is(FileSurveyInput(1), "list")
  expect_is(FileSurveyInput(1), "list")
  expect_is(ggplotdownUI(1), "list")
  expect_is(kaplanUI(1), "list")
  expect_is(regressModuleUI(1), "list")
  expect_is(tb1moduleUI(1), "list")
  expect_is(tb1simpleUI(1), "list")
  expect_is(ggpairsModuleUI1(1), "list")
  expect_is(ggpairsModuleUI2(1), "list")


})


context("basic")

test_that("shinytest will be passed", {
  skip_on_cran()
  skip_on_travis()
  shinytest::expect_pass(shinytest::testApp("shinytest/basic"))
})

