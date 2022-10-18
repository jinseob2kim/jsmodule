context("Run reclassification")


test_that("Run reclassification", {
  m1 <- glm(vs ~ am + gear, data = mtcars, family = binomial)
  m2 <- glm(vs ~ am + gear + wt, data = mtcars, family = binomial)
  m3 <- glm(vs ~ am + gear + wt + mpg, data = mtcars, family = binomial)
  library(pROC)
  roc1 <- roc(m1$y, predict(m1, type = "response"))
  roc2 <- roc(m2$y, predict(m2, type = "response"))
  roc3 <- roc(m3$y, predict(m3, type = "response"))

  expect_is(reclassificationJS(
    data = mtcars, cOutcome = 8,
    predrisk1 = predict(m1, type = "response"),
    predrisk2 = predict(m2, type = "response"), cutoff = c(0, .20, .40, 1)
  ), "data.frame")

  expect_is(ROC_table(list(roc1, roc2, roc3)), "data.frame")
})
