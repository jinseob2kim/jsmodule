#' @title regress.display2: modified epiDisplay's regress.display function
#' @description regress.display function for reactive data
#' @param regress.model lm object
#' @param alpha alpha, Default: 0.05
#' @param crude crude, Default: FALSE
#' @param crude.p.value crude.p.value, Default: FALSE
#' @param decimal decimal, Default: 2
#' @param simplified simplified, Default: FALSE
#' @return regress table
#' @details regress.display function for reactive data
#' @examples
#' model1 <- glm(mpg ~ cyl + disp + vs, data = mtcars)
#' regress.display2(model1, crude = TRUE, crude.p.value = TRUE, decimal = 3)
#' @rdname regress.display2
#' @export
#' @importFrom stats as.formula lm glm logLik qnorm qt
#' @importFrom epiDisplay tableGlm


regress.display2 <- function (regress.model, alpha = 0.05, crude = FALSE, crude.p.value = FALSE,
                              decimal = 2, simplified = FALSE)
{
  model <- regress.model
  if (length(grep("[$]", attr(model$term, "term.labels"))) >
      0 | length(grep(")", attr(model$term, "term.labels"))) >
      0 | length(model$call) < 3) {
    simplified <- TRUE
    crude <- TRUE
  }
  else {
    factor.with.colon <- NULL
    for (i in 1:(length(attr(model$term, "term.labels")) -
                 1)) {
      factor.with.colon <- c(factor.with.colon, any(grep(":",
                                                         model$xlevels[i])))
    }
    factor.with.colon <- any(factor.with.colon)
    if ((length(grep(":", attr(model$terms, "term.labels"))) >
         1) | factor.with.colon) {
      simplified <- TRUE
      crude <- TRUE
    }
  }
  if (simplified) {
    coeff <- summary(model)$coefficients
    table1 <- cbind(coeff[, 1], (coeff[, 1] - qt((1 - alpha/2),
                                                 summary(model)$df[2]) * coeff[, 2]), (coeff[, 1] +
                                                                                         qt((1 - alpha/2), summary(model)$df[2]) * coeff[,
                                                                                                                                         2]), coeff[, 4])
    colnames(table1) <- c("Coeff", paste("lower0", 100 -
                                           100 * alpha, "ci", sep = ""), paste("upper0", 100 -
                                                                                 100 * alpha, "ci", sep = ""), "Pr>|t|")
  }
  else {
    if (length(class(model)) == 2) {
      if (class(model)[1] != "glm" | class(model)[2] !=
          "lm" | model$family$family != "gaussian") {
        stop("Model not from linear regression")
      }
    }
    else {
      if (length(class(model)) == 1) {
        if (class(model) != "lm") {
          stop("Model not from linear regression")
        }
      }
    }
    var.names <- attr(model$terms, "term.labels")
    if (length(var.names) == 1) {
      crude <- FALSE
    }
    if (crude) {
      reg.ci0 <- NULL
      for (i in 1:length(var.names)) {
        formula0 <- as.formula(paste(names(model$model)[1],
                                     "~", var.names[i]))
        if (any(class(model) == "glm")) {
          model0 <- glm(formula0, weights = model$prior.weights,
                        family = model$family, data = model$model)
        }
        else {
          model0 <- lm(formula0, weights = model$prior.weights,
                       data = model$model)
        }
        coeff.matrix <- (summary(model0))$coefficients[-1,
                                                       ]
        if (length(grep(":", var.names[i])) > 0) {
          var.name.interact <- unlist(strsplit(var.names[i],
                                               ":"))
          if (any(names(model$xlevels) == var.name.interact[1])) {
            level1 <- length(unlist(model$xlevels[var.name.interact[1]])) -
              1
          }
          else {
            level1 <- 1
          }
          if (any(names(model$xlevels) == var.name.interact[2])) {
            level2 <- length(unlist(model$xlevels[var.name.interact[2]])) -
              1
          }
          else {
            level2 <- 1
          }
          dim.coeff <- dim((summary(model0))$coefficients[-1,
                                                          , drop = FALSE])
          coeff.matrix <- matrix(rep(NA, dim.coeff[1] *
                                       dim.coeff[2]), dim.coeff[1], dim.coeff[2])
          coeff.matrix <- coeff.matrix[1:(level1 * level2),
                                       , drop = FALSE]
        }
        reg.ci0 <- rbind(reg.ci0, coeff.matrix)
      }
      reg.ci0 <- rbind(matrix(rep(0, 4), 1, 4), reg.ci0)
      colnames(reg.ci0) <- c("crude.Coeff", paste("lower0",
                                                  100 - 100 * alpha, "ci", sep = ""), paste("upper0",
                                                                                            100 - 100 * alpha, "ci", sep = ""), "crude P value")
      reg.ci0[, 3] <- (reg.ci0[, 1] + qt((1 - alpha/2),
                                         summary(model0)$df[2]) * reg.ci0[, 2])
      reg.ci0[, 2] <- (reg.ci0[, 1] - qt((1 - alpha/2),
                                         summary(model0)$df[2]) * reg.ci0[, 2])
    }
    s1 <- summary(model)
    reg.ci <- s1$coefficients
    colnames(reg.ci) <- c("Coeff", paste("lower", 100 - 100 *
                                           alpha, "ci", sep = ""), paste("upper", 100 - 100 *
                                                                           alpha, "ci", sep = ""), "P(t-test)")
    reg.ci[, 3] <- (reg.ci[, 1] + qt((1 - alpha/2), summary(model)$df[2]) *
                      reg.ci[, 2])
    reg.ci[, 2] <- (reg.ci[, 1] - qt((1 - alpha/2), summary(model)$df[2]) *
                      reg.ci[, 2])
    decimal1 <- ifelse(abs(reg.ci[, 1] - 1) < 0.01, 4, decimal)
    a <- cbind(paste(round(reg.ci[, 1], decimal1), " (",
                     round(reg.ci[, 2], decimal1), ",", round(reg.ci[,
                                                                     3], decimal1), ") ", sep = ""), ifelse(reg.ci[,
                                                                                                                   4] < 0.001, "< 0.001", round(reg.ci[, 4], decimal +
                                                                                                                                                  1)))
    colnames(a) <- c(paste("adj. coeff.(", 100 - 100 * alpha,
                           "%CI)", sep = ""), "P(t-test)")
    if (length(var.names) == 1) {
      colnames(a) <- c(paste("Coeff.(", 100 - 100 * alpha,
                             "%CI)", sep = ""), "P(t-test)")
    }
    rownames.a <- rownames(a)
    if (crude) {
      decimal0 <- ifelse(abs(reg.ci0[, 1] - 1) < 0.01,
                         4, decimal)
      if (crude.p.value) {
        a0 <- cbind(paste(round(reg.ci0[, 1], decimal0),
                          " (", round(reg.ci0[, 2], decimal0), ",", round(reg.ci0[,
                                                                                  3], decimal0), ") ", sep = ""), ifelse(reg.ci0[,
                                                                                                                                 4] < 0.001, "< 0.001", round(reg.ci0[, 4],
                                                                                                                                                              decimal + 1)))
        a <- cbind(a0, a)
        rownames(a) <- rownames.a
        colnames(a) <- c(paste("crude coeff.(", 100 -
                                 100 * alpha, "%CI)", sep = ""), "crude P value",
                         paste("adj. coeff.(", 100 - 100 * alpha, "%CI)",
                               sep = ""), "P(t-test)")
        a[grep(":", rownames(a)), 1:2] <- "-"
      }
      else {
        a <- cbind(paste(round(reg.ci0[, 1], decimal1),
                         " (", round(reg.ci0[, 2], decimal1), ",", round(reg.ci0[,
                                                                                 3], decimal1), ") ", sep = ""), a)
        colnames(a) <- c(paste("crude coeff.(", 100 -
                                 100 * alpha, "%CI)", sep = ""), paste("adj. coeff.(",
                                                                       100 - 100 * alpha, "%CI)", sep = ""), "P(t-test)")
        a[grep(":", rownames(a)), 1] <- "-"
      }
    }
    modified.coeff.array <- a
    table1 <- epiDisplay::tableGlm(model, modified.coeff.array, decimal)
  }
  if (simplified) {
    first.line <- NULL
    last.lines <- NULL
  }
  else {
    outcome.name <- names(model$model)[1]
    first.line <- paste("Linear regression predicting ",
                        outcome.name, sep = "", "\n")
    if (any(class(model) == "glm")) {
      last.lines <- paste("Log-likelihood = ", round(logLik(model),
                                                     decimal + 2), "\n", "No. of observations = ",
                          length(model$y), "\n", "AIC value = ", round(s1$aic,
                                                                       decimal + 2), "\n", "\n", sep = "")
    }
    else {
      last.lines <- paste("No. of observations = ", nrow(model$model),
                          "\n", "\n", sep = "")
    }
  }
  results <- list(first.line = first.line, table = table1,
                  last.lines = last.lines)
  class(results) <- c("display", "list")
  results
}






#' @title logistic.display2: Modified epiDisplay's logistic.display function.
#' @description Modified epiDisplay's logistic.display function for reactive data.
#' @param logistic.model glm object(binomial)
#' @param alpha alpha, Default: 0.05
#' @param crude crude, Default: TRUE
#' @param crude.p.value crude.p.value, Default: FALSE
#' @param decimal decimal, Default: 2
#' @param simplified simplified, Default: FALSE
#' @return logistic table
#' @details Modified epiDisplay's logistic.display function for reactive data.
#' @examples
#' model1 <- glm(am ~ cyl + disp, data = mtcars, family = binomial)
#' logistic.display2(model1, crude = TRUE, crude.p.value = TRUE, decimal = 3)
#' @rdname logistic.display2
#' @export
#' @importFrom stats as.formula lm glm logLik qnorm qt
#' @importFrom epiDisplay tableGlm

logistic.display2 <- function (logistic.model, alpha = 0.05, crude = TRUE, crude.p.value = FALSE,
                              decimal = 2, simplified = FALSE)
{
  model <- logistic.model
  if(length(grep("[$]", attr(model$term, "term.labels"))) > 0
     | length(grep(")", attr(model$term, "term.labels"))) > 0
     | length(model$call) < 3){
    simplified <- TRUE; crude <- TRUE
  }else{
    factor.with.colon <- NULL
    for(i in 1:(length(attr(model$term, "term.labels")))){
      factor.with.colon <- c(factor.with.colon, any(grep(":",model$xlevels[i])))
    }
    factor.with.colon <- any(factor.with.colon)
    if((length(grep(":", attr(model$terms, "term.labels"))) > 1) | factor.with.colon){
      simplified <- TRUE; crude <- TRUE
    }}
  if(simplified){
    coeff <- summary(model)$coefficients[-1,]
    table1 <- cbind(exp(coeff[, 1]),
                    exp(coeff[, 1] - qnorm(1 - alpha/2) * coeff[, 2]),
                    exp(coeff[, 1] + qnorm(1 - alpha/2) * coeff[, 2]),
                    coeff[,4] )
    colnames(table1) <- c("OR", paste("lower", 100 - 100 * alpha,
                                      "ci", sep = ""), paste("upper", 100 - 100 * alpha, "ci",
                                                             sep = ""), "Pr(>|Z|)")
  }else{
    if (length(class(model)) == 1) {
      stop("Model not from logistic regression")
    }
    if (class(model)[1] != "glm" | class(model)[2] != "lm" |
        model$family$family != "binomial") {
      stop("Model not from logistic regression")
    }

    var.names <- attr(model$terms, "term.labels")
    if (length(var.names) == 1) {
      crude <- FALSE
    }
    if (crude) {
      orci0 <- NULL
      for (i in 1:length(var.names)) {
        formula0 <- as.formula(paste(names(model$model)[1],
                                     "~", var.names[i]))
        if(names(model$coefficient)[1]!="(Intercept)"){
          formula0 <- as.formula(paste(names(model$model)[1],
                                       "~", var.names[i], "- 1"))
        }

        model0 <- glm(formula0, weights = model$prior.weights,
                        family = binomial, data = model$model)

        coeff.matrix <- (summary(model0))$coefficients[-1,
                                                       , drop = FALSE]
        if(names(model$coefficient)[1]!="(Intercept)"){
          coeff.matrix <- (summary(model0))$coefficients[,
                                                         , drop = FALSE]
        }
        if (length(grep(":", var.names[i])) > 0) {
          var.name.interact <- unlist(strsplit(var.names[i],
                                               ":"))
          if (any(names(model$xlevels) == var.name.interact[1])) {
            level1 <- length(unlist(model$xlevels[var.name.interact[1]])) -
              1
          }
          else {
            level1 <- 1
          }
          if (any(names(model$xlevels) == var.name.interact[2])) {
            level2 <- length(unlist(model$xlevels[var.name.interact[2]])) -
              1
          }
          else {
            level2 <- 1
          }
          dim.coeff <- dim((summary(model0))$coefficients[-1,
                                                          , drop = FALSE])
          coeff.matrix <- matrix(rep(NA, dim.coeff[1] *
                                       dim.coeff[2]), dim.coeff[1], dim.coeff[2])
          coeff.matrix <- coeff.matrix[1:(level1 * level2),
                                       , drop = FALSE]
        }
        orci0 <- rbind(orci0, coeff.matrix)
      }
      if(names(model$coefficient)[1]=="(Intercept)"){
        orci0 <- rbind(matrix(rep(0, 4), 1, 4), orci0)
      }
      colnames(orci0) <- c("crudeOR", paste("lower0", 100 -
                                              100 * alpha, "ci", sep = ""), paste("upper0", 100 -
                                                                                    100 * alpha, "ci", sep = ""), "crude P value")
      orci0[, 3] <- exp(orci0[, 1] + qnorm(1 - alpha/2) * orci0[,
                                                                2])
      orci0[, 2] <- exp(orci0[, 1] - qnorm(1 - alpha/2) * orci0[,
                                                                2])
      orci0[, 1] <- exp(orci0[, 1])
    }
    s1 <- summary(model)
    orci <- s1$coefficients
    colnames(orci) <- c("OR", paste("lower", 100 - 100 * alpha,
                                    "ci", sep = ""), paste("upper", 100 - 100 * alpha, "ci",
                                                           sep = ""), "P(Wald's test)")
    orci[, 3] <- exp(orci[, 1] + qnorm(1 - alpha/2) * orci[,
                                                           2])
    orci[, 2] <- exp(orci[, 1] - qnorm(1 - alpha/2) * orci[,
                                                           2])
    orci[, 1] <- exp(orci[, 1])

    decimal1 <- ifelse(abs(orci[, 1] - 1) < 0.01, 4, decimal)
    a <- cbind(paste(round(orci[, 1], decimal1), " (", round(orci[,
                                                                  2], decimal1), ",", round(orci[, 3], decimal1), ") ",
                     sep = ""), ifelse(orci[, 4] < 0.001, "< 0.001", round(orci[,
                                                                                4], decimal + 1)))
    colnames(a) <- c(paste("adj. OR(", 100 - 100 * alpha, "%CI)",
                           sep = ""), "P(Wald's test)")
    if (length(var.names) == 1) {
      colnames(a) <- c(paste("OR(", 100 - 100 * alpha, "%CI)",
                             sep = ""), "P(Wald's test)")
    }
    rownames.a <- rownames(a)
    if (crude) {
      decimal0 <- ifelse(abs(orci0[, 1] - 1) < 0.01, 4, decimal)
      if (crude.p.value) {
        a0 <- cbind(paste(round(orci0[, 1], decimal0), " (",
                          round(orci0[, 2], decimal0), ",", round(orci0[,
                                                                        3], decimal0), ") ", sep = ""), ifelse(orci0[,
                                                                                                                     4, drop = FALSE] < 0.001, "< 0.001", round(orci0[,
                                                                                                                                                                      4, drop = FALSE], decimal + 1)))
        a <- cbind(a0, a)
        rownames(a) <- rownames.a
        colnames(a) <- c(paste("crude OR(", 100 - 100 * alpha,
                               "%CI)", sep = ""), "crude P value", paste("adj. OR(",
                                                                         100 - 100 * alpha, "%CI)", sep = ""), "P(Wald's test)")
        a[grep(":", rownames(a)), 1:2] <- "-"
      }
      else {
        a <- cbind(paste(round(orci0[, 1], decimal1), " (",
                         round(orci0[, 2], decimal1), ",", round(orci0[,
                                                                       3], decimal1), ") ", sep = ""), a)
        colnames(a) <- c(paste("crude OR(", 100 - 100 * alpha,
                               "%CI)", sep = ""), paste("adj. OR(", 100 - 100 *
                                                          alpha, "%CI)", sep = ""), "P(Wald's test)")
        a[grep(":", rownames(a)), 1] <- "-"
      }
    }
    modified.coeff.array <- a
    table1 <- epiDisplay::tableGlm(model, modified.coeff.array, decimal)
  }
  if(simplified) {
    first.line <- NULL
    last.lines <- NULL
  }else{
    outcome.name <- names(model$model)[1]
    if (!is.null(attr(model$data, "var.labels"))) {
      outcome.name <- attr(model$data, "var.labels")[attr(model$data,
                                                          "names") == names(model$model)[1]]
    }
    else {
      outcome.name <- names(model$model)[1]
    }
    outcome.name <- ifelse(outcome.name == "", names(model$model)[1],
                           outcome.name)
    if (crude) {
      if (attr(model0$term, "dataClasses")[1] == "logical") {
        outcome.lab <- paste(outcome.name, "\n")
      }
      else {
        if ((attr(model$term, "dataClasses")[1] == "numeric") |
            (attr(model$term, "dataClasses")[1] == "integer")) {
          outcome.levels <- levels(factor(model$model[,
                                                      1]))
          outcome.lab <- paste(outcome.name, outcome.levels[2],
                               "vs", outcome.levels[1], "\n")
        }
      }
    }
    if (attr(model$term, "dataClasses")[1] == "factor") {
      outcome.lab <- paste(names(model$model)[1], ":", levels(model$model[,
                                                                          1])[2], "vs", levels(model$model[, 1])[1], "\n")
    }
    else {
      outcome.lab <- paste(outcome.name, "\n")
    }
    first.line <-paste("\n", "Logistic regression predicting ", outcome.lab,
                       sep = "")
    last.lines <- paste("Log-likelihood = ", round(logLik(model), decimal + 2), "\n",
                        "No. of observations = ", length(model$y), "\n",
                        "AIC value = ", round(s1$aic, decimal + 2), "\n","\n", sep = "")
  }
  results <- list(first.line=first.line, table=table1, last.lines=last.lines)
  class(results) <- c("display", "list")
  results
}






