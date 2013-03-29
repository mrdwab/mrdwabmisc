#'Reshape unbalanced wide data to a semi-long data form
#'
#'Base R's \code{\link{reshape}} function cannot easily handle unbalanced wide
#'data.  \code{\link[reshape2:melt]{melt}}, from "reshape2" puts everything
#'into a very long \code{data.frame}. This function returns a semi-long
#'\code{data.frame} after automatically "balancing" the wide measures.
#'
#'
#'@param data The source \code{data.frame}
#'@param id.vars The variables that should be treated as id variables
#'@param sep The separator used. Defaults to \code{.}
#'@param dropNA Logical. If all the measure variables for some cases results in
#'a row of \code{NA} values, shoud that row be dropped? Defaults to \code{TRUE}
#'@note Attributes from the output of \code{reshape} are retained, allowing you
#'to easily re-convert to the wide form, if necessary.
#'@author Ananda Mahto
#'@seealso \code{\link{reshape}}, \code{\link[reshape2:melt]{melt}}
#'@examples
#'
#'data(concatenated)
#'temp <- head(do.call(cbind,
#'           c(concat.test[1],
#'             lapply(1:(ncol(concat.test)-1),
#'             function(x) {
#'             splitchars = c(",", ",", ";")
#'             concat.split(concat.test[-1][x], 1,
#'                          splitchars[x],
#'                          drop.col=TRUE)
#'                          }))))
#'temp
#'
#'\dontrun{
#'## Because the data are unbalanced, we can't just use this
#'reshape(temp, direction = "long", idvar = "Name",
#'      varying = 2:ncol(temp), sep = "_")
#'}
#'
#'unbalancedReshape(temp, id.vars = "Name", sep = "_")
#'
#'\dontrun{
#'## Compare with melt from reshape2
#'library(reshape2)
#'melt(temp, id.vars = "Name")
#'}
#'
unbalancedReshape <- function(data, id.vars, sep = ".", dropNA = TRUE) {
  if (is.numeric(id.vars)) id.vars <- names(data)[id.vars]
  Varying <- setdiff(names(data), id.vars)
  Check <- table(sapply(strsplit(Varying, sep, fixed = TRUE), `[[`, 1))
  Mode <- ifelse(all(Check == max(Check)), "balanced", "unbalanced")
  OUT <- switch(
    Mode,
    unbalanced = {
      X1 <- Check[Check != max(Check)]
      X2 <- unlist(lapply(seq_along(X1), 
                          function(x) paste(names(X1)[x], 
                                            (X1[x]+1):max(Check), 
                                            sep = sep)))
      X3 <- data.frame(
        data, setNames(
          as.data.frame(matrix(NA, ncol = length(X2))), X2))
      final <- reshape(X3, direction = "long", idvar = id.vars, 
                       sep = sep, varying = c(Varying, X2))
      rownames(final) <- NULL
      final
    },
    balanced = {
      final <- reshape(data, direction = "long", idvar = id.vars,
                       sep = sep, varying = Varying)
      rownames(final) <- NULL
      final
    })
  if (isTRUE(dropNA)) {
    OUT[!apply(OUT[setdiff(names(OUT), c(id.vars, "time"))], 1, 
               function(x) all(is.na(x))), ]
  }
  else OUT
}
