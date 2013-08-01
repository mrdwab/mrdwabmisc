#' Append \code{data.frame}s by row, even when columns differ
#' 
#' The default \code{\link{rbind}} function will produce an error if you
#' attempt to use it on \code{\link{data.frame}s} with differing numbers of
#' columns. The \code{\link{RBIND}} function appends a list of
#' \code{data.frame}s together by row, filling missing columns with \code{NA}.
#' 
#' 
#' @param datalist A \code{list} of \code{data.frame}s which need to be
#' appended together by row.
#' @param keep.rownames Logical. Should the original \code{\link{rownames}} be
#' retained? Defaults to \code{TRUE}.
#' @author Ananda Mahto
#' @seealso \code{\link{rbind}} and \code{\link{cbind}} for other base R
#' functions to combine \code{data.frame}s;
#' \code{\link[plyr:rbind.fill]{rbind.fill}} for a function with almost
#' identical functionality (does not preserve the \code{rownames});
#' \code{\link{CBIND}}.
#' @examples
#' 
#' ## Make up some data
#' x <- data.frame(a = 1:2, b = 2:3, c = 3:4, d = 4:5,
#'         row.names = c("row_1", "another_row1"))
#' y <- data.frame(a = c(10, 20), b = c(20, 30), c = c(30, 40),
#'         row.names=c("row_2", "another_row2"))
#' z <- data.frame(a = c(11, 21), b = c(22, 32), d = c(33, 43),
#'         row.names = c("row_3", "another_row3"))
#' xx <- data.frame(a = 1:2, b = 3:4)
#' yy <- data.frame(a = 5:6, b = 7:8)
#' zz <- data.frame(a = 9:10, b = 11:12)
#' zz2 <- data.frame(a = 9:10, w = 11:12)
#' temp1 <- list(x, y, z)
#' temp2 <- list(xx, yy, zz)
#' temp3 <- list(xx, yy, zz2)
#' temp4 <- list(x, y, z, xx, yy, zz, zz2)
#' 
#' ## Apply the function
#' RBIND(temp1)
#' RBIND(temp1, keep.rownames = FALSE)
#' RBIND(temp2)
#' RBIND(temp3)
#' RBIND(temp4)
#' RBIND(temp4, keep.rownames = FALSE)
#' \dontshow{
#' rm(x, y, z, xx, yy, zz, zz2, temp1, temp2, temp3, temp4)
#' }
#' 
#' @export RBIND
RBIND <- function(datalist, keep.rownames = TRUE) {
  Len <- sapply(datalist, ncol)
  if (all(diff(Len) == 0)) {
    temp <- names(datalist[[1]])
    if (all(sapply(datalist, function(x) names(x) %in% temp))) tryme <- "basic"
    else tryme <- "complex"
  } 
  else tryme <- "complex"
  almost <- switch(
    tryme,
    basic = { do.call("rbind", datalist) },
    complex = {
      Names <- unique(unlist(lapply(datalist, names)))
      NROWS <- c(0, cumsum(sapply(datalist, nrow)))
      NROWS <- paste(NROWS[-length(NROWS)]+1, NROWS[-1], sep=":")
      out <- lapply(1:length(datalist), function(x) {
        emptyMat <- matrix(NA, nrow = nrow(datalist[[x]]), ncol = length(Names))
        colnames(emptyMat) <- Names
        emptyMat[, match(names(datalist[[x]]), 
                         colnames(emptyMat))] <- as.matrix(datalist[[x]])
        emptyMat
      })
      do.call("rbind", out)
    })
  Final <- as.data.frame(almost, row.names = 1:nrow(almost))
  Final <- data.frame(lapply(Final, function(x) type.convert(as.character(x))))
  if (isTRUE(keep.rownames)) {
    row.names(Final) <- make.unique(unlist(lapply(datalist, row.names)))
  } 
  Final
}
