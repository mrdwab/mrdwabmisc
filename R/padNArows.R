#' Pad rows of a \code{data.frame} with \code{NA}
#' 
#' A helper function for \code{\link{CBIND}} to pad a \code{data.frame} with \code{NA} to make \code{\link{cbind}} work as expected.
#' 
#' @param mydata The input \code{data.frame}
#' @param rowsneeded The desired number of rows.
#' @return A \code{data.frame} with rows of \code{NA} values appended to the bottom.
#' @author Ananda Mahto
#' @examples
#' 
#' mydf <- data.frame(a = 1:3, b = c("a", "b", "c"))
#' padNArows(mydf, 10)
#' 
#' \dontshow{rm(mydf)}
#' 
padNArows <- function(mydata, rowsneeded) {
  temp1 = names(mydata)
  rowsneeded = rowsneeded - nrow(mydata)
  temp2 = setNames(data.frame(
    matrix(rep(NA, length(temp1) * rowsneeded),
           ncol = length(temp1))), temp1)
  rbind(mydata, temp2)
}
