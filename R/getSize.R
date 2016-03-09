#' Get the Size of Multiple Objects in Your Workspace
#' 
#' This is a convenience wrapper around \code{\link{object.size}} to get the
#' sizes of multiple objects in your workspace. By default, it will list all
#' the objects in your workspaces, but a specific pattern to match can also be
#' specified.
#' 
#' 
#' @param pattern The pattern to be used by \code{\link{ls}}. Defaults to
#' \code{"*"}, meaning to match anything.
#' @param sort.by Should the output be sorted by object size (\code{"size"}) or
#' name (\code{"name"})? Defaults to \code{sort.by = "size"}.
#' @author Ananda Mahto
#' @seealso \code{\link{ls}}, \code{\link{object.size}}
#' @examples
#' 
#' AA <- rnorm(10000)
#' AB <- rnorm(100)
#' CB <- rnorm(50000)
#' 
#' getSize()
#' getSize("*B", "name")
#' getSize("*B", "size")
#' getSize("^A", "name")
#' getSize("^A", "size")
#' 
#' \dontshow{rm(AA, AB, CB)}
#' 
#' @export getSize
getSize <- function (pattern = NULL, sort.by = "size") {
  temp <- sapply(ls(pattern = ifelse(is.null(pattern), "*", pattern), 
                    envir = .GlobalEnv), function(x) {
                      object.size(get(x, envir = .GlobalEnv))
                    })
  switch(sort.by,
         size = sort(temp),
         name = temp,
         stop("sort.by must be either 'size' or 'name'"))
}



