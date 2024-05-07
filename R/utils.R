#' @title mk.lev2: level generating function
#' @description make level for sav files with labels pre defined from SPSS
#' @param out.old raw data
#' @param out.label pre-defined label data
#' @return out.label data labels updated
#' @importFrom data.table data.table
mk.lev2 <- function(out.old, out.label) {

  for (i in 1:length(colnames(out.old))) {

    spss.labels <- attr(out.old[[i]], "labels")
    if (!is.null(spss.labels)) {
      index <- which(out.label$variable == colnames(out.old)[i], arr.ind = T)

      cnt <- length(unname(spss.labels))

      out.part <- data.table(
        variable = rep(colnames(out.old)[i], cnt),
        class = rep("factor", cnt),
        level = unname(spss.labels),
        var_label = rep(colnames(out.old)[i], cnt),
        val_label = names(spss.labels)
      )

      ## label exist, data not exist
      if(max(index)+1 <= nrow(out.label)){
        out.label <- rbind(
          out.label[c(1:(min(index) - 1)), ],
          out.part,
          out.label[c((max(index) + 1):nrow(out.label)), ]
        )
      } else{
        out.label <- rbind(
          out.label[c(1:(min(index) - 1)), ],
          out.part
        )
      }

    }
  }

  return(out.label)
}
