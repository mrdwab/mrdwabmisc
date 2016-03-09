#' @import digest
NULL

#' @import data.table
NULL

#' @import splitstackshape
NULL

getDots <- function(...) sapply(substitute(list(...))[-1], deparse)
NULL