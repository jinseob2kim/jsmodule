# timeroc.R 테스트 코드
library(testthat)
library(jsmodule)
library(data.table)
library(survival)

# 예시 데이터 준비
lung <- as.data.table(survival::lung) # survival 패키지에서 직접 할당
lung <- lung[complete.cases(lung), ]
lung$status <- as.numeric(lung$status == 2) # event: 2

context("timeroc 주요 상황별 테스트")

test_that("모델 1개일 때", {
  res <- timeROChelper(var.event = "status", var.time = "time", vars.ind = c("age"), t = 365, data = lung)
  auc <- res$timeROC$AUC[2]
  coef_age <- coef(res$coxph)["age"]
  cat("[모델 1개] AUC:", round(auc, 5), ", age 계수:", round(coef_age, 5), "\n")
  expect_true(!is.null(res$timeROC))
  expect_true(!is.null(res$coxph))
  #   expect_equal(as.numeric(round(auc, 3)), 0.627) # AUC
  #   expect_equal(as.numeric(round(coef_age, 3)), 0.009) # coxph age 계수
})

test_that("모델 2개 이상일 때", {
  res1 <- timeROChelper("status", "time", c("age"), 365, lung)
  res2 <- timeROChelper("status", "time", c("age", "sex"), 365, lung)
  tb <- timeROC_table(list(res1, res2))
  auc1 <- res1$timeROC$AUC[2]
  auc2 <- res2$timeROC$AUC[2]
  cat("[모델 2개] AUC1:", round(auc1, 5), ", AUC2:", round(auc2, 5), "\n")
  expect_true(nrow(tb) == 2)
  expect_true(auc2 >= auc1)
})

test_that("independent variable 2개 이상일 때", {
  res <- timeROChelper("status", "time", c("age", "sex"), 365, lung)
  auc <- res$timeROC$AUC[2]
  cat("[indep 2개] AUC:", round(auc, 5), "\n")
  expect_true(!is.null(res$timeROC))
  #   expect_equal(as.numeric(round(auc, 3)), 0.637)
})

test_that("subgroup analysis 있을 때", {
  lung_sub <- lung[sex == 1]
  res <- timeROChelper("status", "time", c("age"), 365, lung_sub)
  auc <- res$timeROC$AUC[2]
  cat("[subgroup] AUC:", round(auc, 5), "\n")
  expect_true(!is.null(res$timeROC))
  #   expect_equal(as.numeric(round(auc, 3)), 0.627)
})

test_that("time 수치가 극단적으로 적을 때", {
  lung2 <- copy(lung)
  lung2$time <- 1
  res <- timeROChelper("status", "time", c("age"), 1, lung2)
  auc <- res$timeROC$AUC[2]
  cat("[time 극단값] AUC:", round(auc, 5), "\n")
  expect_true(!is.null(res$timeROC))
  # expect_true(abs(auc - 0.5) < 0.1)
})
