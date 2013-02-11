#'mrdwabmisc
#'
#'Miscellaneous R functions, some utility, and others to clean and organize data.
#'
#'\tabular{ll}{ Package: \tab mrdwabmisc\cr Type: \tab Package\cr Version: \tab
#'1.0\cr Date: \tab 2013-01-22\cr License: \tab GPL-2\cr }
#'
#'@name mrdwabmisc-package
#'@aliases mrdwabmisc-package mrdwabmisc
#'@docType package
#'@author Ananda Mahto
#'
#'Maintainer: Ananda Mahto <mrdwab@@gmail.com>
#'@seealso \code{\link[AMsnippets:AMsnippets-package]{AMsnippets}} contains a related set of utility functions, but the packages do not depend on each other.
#'@keywords package
#'@examples
#'
#'## concat.split
#'data(concatenated)
#'head(concat.test)
#'head(concat.split(concat.test, "Likes", drop.col = TRUE))
#'\dontshow{rm(concat.test)}
#'
#'## sample.size
#'sample.size(population = 300)
#'sample.size(population = 300, c.lev = 97)
#'
#'## stringseed.sampling
#'stringseed.sampling("Santa Barbara", 1920, 100)
#'
#'## table2df
#'table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph),
#'          as.multitable = TRUE, direction = "wide")[[1]]
#'head(table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph)))
#'
NULL

#'Example dataset with concatenated cells
#'
#'This is a sample dataset to demonstrate the different features of the \code{\link{concat.split}} function. 
#'
#' @docType data 
#' @keywords datasets 
#' @name concat.test 
#' @aliases concat.test concatenated
#' @usage data(concatenated) 
#' @format A data.frame in which many columns contain concatenated cells
NULL
NULL

#'List of first names and surnames to generate random names
#'
#'This is the default dataset used by the \code{\link{RandomNames}} function. 
#'
#' @docType data 
#' @keywords datasets 
#' @name CensusNames1990 
#' @aliases CensusNames1990 CensusNames
#' @usage data(CensusNames) 
#' @format A list of first names and surnames, split by gender and how common the first names are. 
#' @references \emph{Genealogy Data: Frequently Occurring Surnames from Census 1990--Names Files}: \url{http://www.census.gov/genealogy/www/data/1990surnames/names_files.html}
NULL
