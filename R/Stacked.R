#' \code{stack} columns from a wide form to a long form
#' 
#' A helper function for \code{\link{Reshape}} to stack groups of wide columns
#' into a long form which can then be \code{\link[data.table:merge]{merge}}d
#' together.
#' 
#' 
#' @param data The source \code{data.frame}.
#' @param id.vars The variables that serve as unique identifiers.
#' @param var.stubs The prefixes of the variable groups.
#' @param sep The character that separates the "variable name" from the "times"
#' in the wide \code{data.frame}.
#' @param keep.all Logical. Should all the variables from the source
#' \code{data.frame} be kept (\code{keep.all = TRUE}) or should the resulting
#' \code{\link[data.table:data.table]{data.table}} comprise only columns for
#' the \code{id.vars}, \code{var.stubs}, and "times" (\code{keep.all = FALSE}).
#' Other variables are \emph{recycled} to appropriate length.
#' @param \dots Further arguments to \code{\link{NoSep}} in case the separator
#' is of a different form.
#' @return A \code{list} of \code{data.table}s with one \code{data.table} for
#' each "var.stub". The \code{\link[data.table:key]{key}} is set to the
#' \code{id.vars} and \code{.time_#} vars.
#' @note This function is ideally used within \code{\link{Reshape}} but might
#' have its own application elsewhere.
#' @author Ananda Mahto
#' @seealso \code{\link{stack}}, \code{\link[reshape2:melt]{melt}} from
#' "reshape2".
#' @examples
#' 
#' set.seed(1)
#' mydf <- data.frame(id_1 = 1:6, id_2 = c("A", "B"), varA.1 = sample(letters, 6),
#'                  varA.2 = sample(letters, 6), varA.3 = sample(letters, 6),
#'                  varB.2 = sample(10, 6), varB.3 = sample(10, 6),
#'                  varC.3 = rnorm(6))
#' mydf
#' Stacked(mydf, id.vars = c("id_1", "id_2"),
#'        var.stubs = c("varA", "varB", "varC"),
#'        sep = "\\.")
#' 
#' ## Different name structure
#' names(mydf) <- sub("\\.", "", names(mydf))
#' mydf
#' Stacked(mydf, id.vars = c("id_1", "id_2"),
#'        var.stubs = c("varA", "varB", "varC"),
#'        sep = "NoSep")
#' 
#' \dontshow{rm(mydf)}
#' 
#' @export Stacked
Stacked <- function(data, id.vars, var.stubs, sep, keep.all = TRUE, ...) {
  require(data.table)
  vGrep <- Vectorize(grep, "pattern", SIMPLIFY = FALSE)
  data <- FacsToChars(data)
  temp1 <- vGrep(var.stubs, names(data))
  temp2 <- suppressWarnings(lapply(temp1, function(y) {
    X <- data[y]
    t1 <- stack(X)
    if (sep == "NoSep") {
      splitcols <- NoSep(t1$ind, ...)
    } else {
      splitcols <- do.call(rbind.data.frame, 
              strsplit(as.character(t1$ind), split = sep))
      names(splitcols) <- 
        c("VAR", paste(".time", 1:(length(splitcols)-1), sep = "_"))
    }
    names(t1)[1] <- as.character(splitcols$VAR[1])
    t1$ind <- NULL
    timevars <- grep(".time_", names(splitcols), value = TRUE)
    if (isTRUE(keep.all)) {
      t2 <- data.table(
        cbind(data[id.vars], 
              splitcols[!names(splitcols) %in% 
                          setdiff(names(splitcols), timevars)], 
              t1, data[setdiff(seq_along(data), 
                               c(match(id.vars, names(data)),
                                 unlist(temp1)))]), 
        key = c(id.vars, timevars))
    } else {
      t2 <- data.table(
        cbind(data[id.vars], 
              splitcols[!names(splitcols) %in% 
                          setdiff(names(splitcols), timevars)], t1), 
        key = c(id.vars, timevars))
    }
    t2
  }))
  temp2
}
