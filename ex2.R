library(survival)
lung <- survival::lung
lung$status <- lung$status - 1
list.timeROC <- lapply(list("age", c("age", "sex")), function(x) {
  timeROChelper("status", "time", x, t = 306, data = lung)
})
timeROC_table(list.timeROC)

for (x in list.timeROC) {
  f_str <- paste(deparse(x$coxph$formula), collapse = " ")
  f_str_clean <- gsub("survival::", "", f_str)  # Surv를 글로벌 namespace로
  f <- formula(f_str_clean)

  print(f)

  s <- riskRegression::Score(
    object = list(x$coxph),
    formula = f,
    data = x$data,
    times = x$t,
    metrics = c("AUC", "Brier"),
    summary = "IPA",
    cause = 1
  )
  print(s)
  print(s$AUC$score$AUC)
  print(s$Brier$score[model == "coxph", "Brier"])
}

rhs <- paste(all.vars(list.timeROC[[1]]$coxph$formula)[-1], collapse = " + ")



fit <- coxph(Surv(time, status) ~ age + sex, data = lung, x = TRUE, y = TRUE, model = TRUE)

Score(
  object = list(cox = fit),
  formula = Surv(time, status) ~ age + sex,
  data = lung,
  times = c(100, 200, 300),
  metrics = c("AUC", "Brier"),
  summary = "IPA",
  cause = 1
) %>% print


timeROChelper("status", "time", c("age", "sex"), t = 365, data = lung)




library(survival)
list.timeROC <- lapply(list("age", c("age", "sex")), function(x) {
  timeROChelper("status", "time", x, t = 365, data = lung)
})
timeROC_table(list.timeROC)



dt_data <- as.data.table(pbc)

factor_vars <- names(dt_data)[sapply(dt_data, function(x){length(table(x))}) <= 6]
data<-dt_data[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]

data.label <- jstable::mk.lev(data())


data.km <- data[complete.cases(data[, .SD, .SDcols = unique(unlist(c("id")))])]
# data.km <- data.km[complete.cases(data.km[, .SD, .SDcols = input$event_km ])]
data.km[["spiders"]] <- as.numeric(as.vector(data.km[["spiders"]]))

x <- lapply("id", function(x){
  timeROChelper("spiders", "time", vars.ind = x, t=4691, data= data.km)
})



var.event <- "spiders"
var.time <- "time"
vars.ind <-"sex"
t <- 1000
data <- data.km
design.survey <- NULL
id.cluster <- NULL

data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))
form <- as.formula(paste0(
  "Surv(",
  var.time,
  ",",
  var.event,
  ") ~ ",
  paste(vars.ind, collapse = "+")
))

if (is.null(design.survey)) {
  if (!is.null(id.cluster)) {
    cmodel <- survival::coxph(
      form,
      data = data,
      x = TRUE,
      y = TRUE,
      model = TRUE,
      cluster = data[[id.cluster]]
    )
  } else {
    cmodel <- survival::coxph(
      form,
      data = data,
      x = TRUE,
      y = TRUE,
      model = TRUE
    )
  }
} else {
  cmodel <- survey::svycoxph(
    form,
    design = design.survey,
    x = TRUE,
    y = TRUE,
    model = TRUE
  )
}

T <- cmodel$y[, 1]
delta <- cmodel$y[, 2]
lp <- stats::predict(cmodel, type = "lp")

out <- timeROC::timeROC(
  T = T,
  delta = delta,
  marker = lp,
  cause = 1,
  weighting = "marginal",
  times = t
)

if (out$AUC[2] < 0.5) {
  lp <- -lp
  out <- timeROC::timeROC(
    T = T,
    delta = delta,
    marker = lp,
    cause = 1,
    weighting = "marginal",
    times = t
  )
}

timeROC_table(x)

ListModel <- x


timeROC_table <- function(ListModel, dec.auc = 3, dec.p = 3) {
  concords <- lapply(ListModel, function(x) survival::concordance(x$coxph))
  harrell <- sapply(concords, `[[`, "concordance")
  se1.96 <- qnorm(0.975) * sqrt(sapply(concords, `[[`, "var"))
  harrell.ci <- paste0(round(harrell - se1.96, dec.auc), "-", round(harrell + se1.96, dec.auc))
  harrell <- round(harrell, dec.auc)

  auc_list <- list()
  brier_list <- list()

  for (i in seq_along(ListModel)) {
    if(!is.na(ListModel[[i]]$timeROC$AUC[2])){
      model <- ListModel[[i]]$coxph
      data <- ListModel[[i]]$data
      f <- model$formula
      score <- riskRegression::Score(list(coxph = model), formula = f, data = data, times = ListModel[[i]]$t,
                                     metrics = c("AUC", "Brier"), summary = "IPA", cause = 1)
      # print(score)
      auc_list[[i]] <- round(score$AUC$score$AUC, dec.auc)
      brier_list[[i]] <- round(score$Brier$score[score$Brier$score$model == "coxph", "Brier"], dec.auc)
    }

  }

  if (length(ListModel) == 1) {
    if(length(auc_list)>0 && length(brier_list)>0){
      out <- data.table::data.table(
        "Prediction Model" = "Model 1",
        "Harrell's C-index" = harrell,
        "95% CI" = harrell.ci,
        "AUC" = unlist(auc_list),
        "Brier" = unlist(brier_list)
      )
    }else{
      out <- data.table::data.table(
        "Prediction Model" = "Model 1",
        "Harrell's C-index" = harrell,
        "95% CI" = harrell.ci
      )
    }

  } else {
    harrell.pdiff <- c(NA, sapply(2:length(ListModel), function(i) {
      d <- harrell[i] - harrell[i-1]
      s <- sqrt(se1.96[i]^2 + se1.96[i-1]^2)
      p <- 2 * pnorm(abs(d / s), lower.tail = FALSE)
      ifelse(p < 0.001, "< 0.001", round(p, dec.p))
    }))

    out <- data.table::data.table(
      "Prediction Model" = paste0("Model ", seq_along(ListModel)),
      "Harrell's C-index" = harrell,
      "95% CI" = harrell.ci,
      "P-value for Harrell's C-index Difference" = harrell.pdiff,
      "AUC" = unlist(auc_list),
      "Brier" = unlist(brier_list)
    )
  }

  return(out[])
}

survIDINRI_helper("spiders", "time", "sex", 5000, data.km)
