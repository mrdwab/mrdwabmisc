#' "Unlist" a \code{list} of \code{data.frame}s to your workspace
#' 
#' Many people like the convenience that a \code{list} of \code{data.frame}s
#' offer; however, some would prefer to have each \code{data.frame} as a
#' separate object in their workspace. This function "unlists" a \code{list} of
#' \code{data.frame}s, creating objects named after the \code{list} and the
#' list item's names (or index position, if names are not available).
#' 
#' 
#' @param mylist The name of your \code{list} object
#' @author Ananda Mahto
#' @seealso \code{\link{unlist}}
#' @examples
#' 
#' ## Some example data
#' ## List with named items
#' qwerty <- list(A = data.frame(A = 1:2, B = 3:4),
#'       B = data.frame(C = 5:6, D = 7:8))
#' 
#' ## List with unnamed items
#' ytrewq <- list(data.frame(A = 1:2, B = 3:4),
#'       data.frame(C = 5:6, D = 7:8))
#' 
#' ls(pattern = "qwer|ytre")
#' 
#' unlistDF(qwerty)
#' unlistDF(ytrewq)
#' 
#' ls(pattern = "qwer|ytre")
#' \dontshow{rm(list = ls(pattern = "qwer|ytre"))}
#' 
#' @export unlistDF
unlistDF <- function(mylist) {
  dname <- deparse(substitute(mylist))
  if (is.null(names(mylist))) prefix <- paste0(dname, "_", 1:length(mylist))
  else prefix <- paste0(dname, "_", names(mylist))
  for (i in 1:length(mylist)) assign(prefix[i], mylist[[i]], envir=.GlobalEnv)
}
