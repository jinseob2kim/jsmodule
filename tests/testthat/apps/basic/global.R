library(shiny);library(ggplot2);library(DT);library(data.table);library(shinycustomloader)
library(jstable);library(jskm);library(jsmodule)
nfactor.limit = 20

change.vnlist = list(c(" ", "_"), c("=<", "_le_"), c("=>", "_ge_"), c("=", "_eq_"), c("\\(", "_open_"), c("\\)", "_close_"), c("%", "_percent_"), c("-", "_"), c("/", "_"),
                     c("\r\n", "_"), c(",", "_comma_"))

out <- data.table(mtcars)


## Initial variable name
for (x in change.vnlist){
  names(out) <- gsub(x[1], x[2], names(out))
}
numstart.vnum <- suppressWarnings(sapply(names(out),function(x){!is.na(as.numeric(substr(x, 1,1)))}))
names(out)[numstart.vnum] <- paste("n_", names(out)[numstart.vnum], sep = "")

## factor variable
factor_vars <- names(out)[out[, lapply(.SD, class) %in% c("factor", "character")]]
out[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
conti_vars <- setdiff(names(out), factor_vars)
nclass <- unlist(out[, lapply(.SD, function(x){length(unique(x))}), .SDcols = conti_vars])
#except_vars <- names(nclass)[ nclass== 1 | nclass >= 10]
add_vars <- names(nclass)[nclass >= 1 &  nclass <= 5]

data.list <- list(data = out, factor_original = factor_vars, conti_original = conti_vars, factor_adds_list = names(nclass)[nclass <= nfactor.limit], factor_adds = add_vars)
