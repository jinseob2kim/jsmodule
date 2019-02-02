context("Run display2")

mtcars$vs <- as.factor(mtcars$vs)
mtcars$cyl <- as.factor(mtcars$cyl)

test_that("Run regress.display2", {

  model1 <- glm(mpg ~ cyl + disp + vs, data = mtcars)
  expect_is(regress.display2(model1, crude = T, crude.p.value = T, decimal = 3), "display")

  model2 <- glm(mpg ~ cyl, data = mtcars)
  expect_is(regress.display2(model2, crude = T, crude.p.value = T, decimal = 3), "display")

})


test_that("Run logistic.display2", {
  model1 <- glm(am ~ cyl + disp + vs, data = mtcars, family = binomial)
  expect_is(logistic.display2(model1, crude = T, crude.p.value = T, decimal = 3), "display")

  model2 <- glm(am ~ cyl, data = mtcars, family = binomial)
  expect_is(logistic.display2(model2, crude = T, crude.p.value = T, decimal = 3), "display")

})
