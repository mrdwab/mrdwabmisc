#' Pad rows of a \code{data.frame} with \code{NA}
#' 
#' A helper function for \code{\link{CBIND}} to pad a \code{data.frame} with
#' \code{NA} to make \code{\link{cbind}} work as expected.
#' 
#' 
#' @param mydata The input \code{data.frame}
#' @param rowsneeded The desired number of rows.
#' @return A \code{data.frame} with rows of \code{NA} values appended to the
#' bottom.
#' @author Ananda Mahto
#' @examples
#' 
#' mydf <- data.frame(a = 1:3, b = c("a", "b", "c"))
#' mrdwabmisc:::padNArows(mydf, 10)
#' 
#' \dontshow{rm(mydf)}
#' 
padNArows <- function(mydata, rowsneeded) {
  temp1 = names(mydata)
  rowsneeded = rowsneeded - nrow(mydata)
  temp2 = setNames(data.frame(
    matrix(rep(NA, length(temp1) * rowsneeded),
           ncol = length(temp1))), temp1)
  rbind(mydata, temp2)
}
NULL


#' List of First Names and Surnames to Generate Random Names
#'
#' A list of first names and surnames by gender and category. This is the 
#' default dataset used by the \code{\link{RandomNames}} function.
#'
#' @format A list of three \code{data.frame}s.
#' \describe{
#'   \item{surnames}{A \code{data.frame} of surnames and categories (common, 
#'   average, and rare).}
#'   \item{malenames}{A \code{data.frame} of male names and categories (common, 
#'   average, and rare).}
#'   \item{femalenames}{A \code{data.frame} of female names and categories 
#'   (common, average, and rare).}
#' }
#' @source \emph{Genealogy Data: Frequently Occurring Surnames from Census
#' 1990--Names Files}: \url{http://www.census.gov/genealogy/www/data/1990surnames/names_files.html}
"CensusNames1990"
NULL