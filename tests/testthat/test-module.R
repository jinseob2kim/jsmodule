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


test_that("Run shiny: basic", {

  app <- ShinyDriver$new(test_path("apps/basic"))
  widgets <- app$listWidgets()
  inputs <- widgets$input

  ## list widget
  expect_is(widgets, "list")

  expect_equal(
    sort(inputs),
    sort(c("factor_vname", "factor_vname-selectized", "check_subset", "linear-decimal", "linear-regressUI_subcheck", "logistic-decimal",
           "logistic-regressUI_subcheck", "cox-decimal", "cox-subcheck", "ggpairs-theme", "ggpairs-theme-selectized", "kaplan-cumhaz", "kaplan-pval",
           "kaplan-table", "kaplan-subcheck"))
  )


  ## checkboxInput
  expect_true(
    app$findWidget("check_subset")$setValue(TRUE)$getValue()
  )
  expect_false(
    app$findWidget("check_subset")$setValue(FALSE)$getValue()
  )

})


