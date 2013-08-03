#' "Expand" the rows of a dataset
#' 
#' Replicates each row in a dataset either by a count column specified within the dataset or by a value or vector of values specified by the user.
#' 
#' @param dataset The input \code{data.frame}.
#' @param count The vector of counts or the column that contains the counts.
#' @param count.is.col Logical. Is the \code{count} value a column in the \code{data.frame}? Defaults to \code{TRUE}.
#' @return A \code{data.frame}.
#' @author Ananda Mahto
#' @seealso \code{\link{rep}}
#' @examples
#' 
#' mydf <- data.frame(x = c("a", "b", "q"), y = c("c", "d", "r"), count = c(2, 5, 3))
#' mydf
#' expandrows(mydf, "count")
#' expandrows(mydf, count = 3)
#' expandrows(mydf, count = 3, count.is.col = FALSE)
#' expandrows(mydf, count = c(1, 5, 9), count.is.col = FALSE)
#' 
#' \dontshow{rm(mydf)}
#' 
expandrows <- function(dataset, count, count.is.col = TRUE) {
  if (!isTRUE(count.is.col)) {
    if (length(count) == 1) {
      dataset[rep(rownames(dataset), each = count), ]
    } else {
      if (length(count) != nrow(dataset)) {
        stop("Expand vector does not match number of rows in data.frame")
      }
      dataset[rep(rownames(dataset), count), ]
    }
  } else {
    dataset[rep(rownames(dataset), dataset[[count]]), 
            setdiff(names(dataset), names(dataset[count]))]
  }
}
