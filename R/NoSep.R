#' Split basic alphanumeric strings which have no separators
#' 
#' Used to split strings like "Abc8" into "Abc" and "8".
#' 
#' 
#' @param data The vector of strings to be split.
#' @param charfirst Is the string constructed with characters at the start or
#' numbers? Defaults to \code{TRUE}.
#' @return A \code{data.frame} with two columns, \code{VAR} and \code{.time_1}.
#' @note This is mostly a helper function for the \code{\link{Stacked}} and
#' \code{\link{Reshape}} functions.
#' @author Ananda Mahto
#' @seealso \code{\link{strsplit}}
#' @examples
#' 
#' x <- paste0("Var", LETTERS[1:3], 1:3)
#' NoSep(x)
#' 
#' y <- paste0(1:3, "Var", LETTERS[1:3])
#' NoSep(y, charfirst = FALSE)
#' 
#' \dontshow{rm(x, y)}
#' 
#' @export NoSep
NoSep <- function(data, charfirst = TRUE) {
  if (isTRUE(charfirst)) {
    Pattern <- "([[:alpha:]]+)([[:digit:]]+)"
    Names <- c("VAR", ".time_1")
  } else {
    Pattern <- "([[:digit:]]+)([[:alpha:]]+)"
    Names <- c(".time_1", "VAR")
  }
  setNames(data.frame(gsub(Pattern, "\\1", data), 
                      gsub(Pattern, "\\2", data)), Names)
}
