#' cbind \code{data.frames} with Different Number of Rows
#' 
#' \code{\link{cbind}} does not work when trying to combine \code{data.frame}s
#' with differing numbers of rows. This function takes a \code{list} of
#' \code{data.frame}s, identifies how many extra rows are required to make
#' \code{cbind} work correctly, and does the combining for you.
#' 
#' The \code{\link{CBIND}} function also works with nested lists by first
#' "flattening" them using the \code{\link{LinearizeNestedList}} function by
#' Akhil S Bhel.
#' 
#' @param datalist A \code{list} of \code{data.frame}s that you want to combine
#' by columns.
#' @author Ananda Mahto
#' @seealso \code{\link{cbind}}, \code{\link[gdata:cbindX]{cbindX}},
#' \code{\link{LinearizeNestedList}}
#' @examples
#' 
#' # Example data
#' df1 <- data.frame(A = 1:5, B = letters[1:5])
#' df2 <- data.frame(C = 1:3, D = letters[1:3])
#' df3 <- data.frame(E = 1:8, F = letters[1:8], G = LETTERS[1:8])
#' 
#' CBIND(list(df1, df2, df3))
#' 
#' # Nested lists:
#' test1 <- list(list(df1, df2, df3), df1)
#' str(test1)
#' 
#' CBIND(test1)
#' \dontshow{rm(df1, df2, df3, test1)}
#' 
#' @export CBIND
CBIND <- function(datalist) {
  datalist <- LinearizeNestedList(datalist)
  nrows <- max(sapply(datalist, nrow))
  do.call(cbind, lapply(datalist, padNArows, rowsneeded = nrows))
}
