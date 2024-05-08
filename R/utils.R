#' @title mk.lev2: level generating function
#' @description make level for sav files with labels pre defined from SPSS
#' @param out.old raw data
#' @param out.label pre-defined label data
#' @return out.label data labels updated
#' @importFrom data.table data.table :=
#' @importFrom DT  %>%
mk.lev2 <- function(out.old, out.label) {
  . <- variable <- val_label <- level <- NULL
  label.value <- sapply(out.old, function(x) attr(x, "labels")) %>% .[!sapply(., is.null)] %>% sapply(function(x){dd <- names(x);names(dd) <- x;return(dd)})
  label.variable <- sapply(out.old, function(x) attr(x, "label")) %>% .[!sapply(., is.null)]

  for (v in names(label.variable)){
    out.label[variable == v, var_label := label.variable[v]]
  }
  for (v in names(label.value)){
    if (is.factor(out.old[[v]])){
      out.label[variable == v, val_label := label.value[[v]][level]]
    }
  }

  return(out.label[])
}


