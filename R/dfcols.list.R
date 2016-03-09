#' Convert the Columns of a \code{data.frame} to a \code{list}
#' 
#' Sometimes, it is useful to have the columns of a \code{data.frame} as
#' separate list items or vectors.  \code{\link{unlist}} is useful for creating
#' a single vector, but not for creating multiple vectors. The
#' \code{\link{dfcols.list}} function is a simple convenience function that
#' allows for such transformations.
#' 
#' 
#' @param data The input \code{data.frame}
#' @param vectorize Logical. Should the function return a list of single-column
#' \code{data.frame}s, or a simple \code{\link{vector}} of values? Defaults to
#' \code{TRUE}.
#' @author Ananda Mahto
#' @examples
#' 
#' dat <- data.frame(A = c(1:2), B = c(3:4), C = c(5:6))
#' dfcols.list(dat)
#' dfcols.list(dat, vectorize = FALSE)
#' \dontshow{rm(dat)}
#' 
#' @export dfcols.list
dfcols.list <- function(data, vectorize = TRUE) {
  if (isTRUE(vectorize)) as.list(data)
  else setNames(lapply(names(data), function(x) data[x]), names(data))
}
