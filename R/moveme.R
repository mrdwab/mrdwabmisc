#' Reorder elements in a character vector
#' 
#' Reorders elements in a character vector in a "natural language" manner.
#' 
#' This can be a useful function for reordering the columns of a \code{data.frame} or \code{data.table} in a convenient manner. In such cases, the \code{invec} would be \code{names(your_data_frame)}. When using \code{data.table}s, remember to use \code{setcolorder} to avoid copying.
#' 
#' @param invec The input character vector.
#' @param movecommand The command that describes how you want to move the items around. See "Details".
#' @return The input vector reordered according to the instructions in \code{movecommand}.
#' @details The \code{movecommand} argument is specified in the form of \code{"a, b before f"}. The positions to move are:
#' \itemize{
#' \item \strong{first}: move the specified items to the first postion.
#' \item \strong{last}: move the specified items to the last position.
#' \item \strong{before}: move the specified items before the value mentioned.
#' \item \strong{after}: move the specified items after the value mentioned.
#' }
#' Multiples are allowed:
#' \itemize{
#' \item Specify multiple values to be moved by separating them with a comma.
#' \item Chain multiple move commands by separating them with a semicolon.
#' }
#' @author Ananda Mahto
#' @seealso \code{\link{append}}
#' @references This function was written as an answer to the following Stack
#' Overflow question: \url{http://stackoverflow.com/q/12544888/1270695}
#' @examples
#'
#' myvec <- letters[1:10]
#' moveme(myvec, "a last; b, e, g before d; c first; h after j")
#' 
#' \dontshow{rm(myvec)}
#' 
moveme <- function(invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], ",|\\s+"), 
                        function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  
  myVec <- invec
  
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp)-1
      } else if (A == "after") {
        after <- match(ba, temp)
      }    
    } else if (A == "first") {
      after <- 0
    } else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}
