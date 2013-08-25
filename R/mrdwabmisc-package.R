

#' List of first names and surnames to generate random names
#' 
#' This is the default dataset used by the \code{\link{RandomNames}} function.
#' 
#' 
#' @name CensusNames1990
#' @aliases CensusNames CensusNames1990
#' @docType data
#' @format A list of first names and surnames, split by gender and how common
#' the first names are.
#' @references \emph{Genealogy Data: Frequently Occurring Surnames from Census
#' 1990--Names Files}:
#' \url{http://www.census.gov/genealogy/www/data/1990surnames/names_files.html}
#' @keywords datasets
NULL





#' mrdwabmisc
#' 
#' Miscellaneous R functions, some utility, and others to clean and organize
#' data.
#' 
#' \tabular{ll}{ Package: \tab mrdwabmisc\cr Type: \tab Package\cr Version:
#' \tab 0.2.0\cr Date: \tab 2013-08-14\cr License: \tab GPL-3\cr }
#' 
#' @name mrdwabmisc-package
#' @aliases mrdwabmisc mrdwabmisc-package
#' @docType package
#' @author Ananda Mahto
#' 
#' Maintainer: Ananda Mahto <ananda@@mahto.info>
#' @keywords package
#' @examples
#' 
#' ## sample.size
#' sample.size(population = 300)
#' sample.size(population = 300, c.lev = 97)
#' 
#' ## stringseed.sampling
#' stringseed.sampling("Santa Barbara", 1920, 100)
#' 
#' ## table2df
#' table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph),
#'     as.multitable = TRUE, direction = "wide")[[1]]
#' head(table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph)))
#' ## aggregate2
#' aggregate2(ToothGrowth, "len", ".", c("sum", "mean"))
#' 
#' ## CBIND
#' df1 <- data.frame(A = 1:5, B = letters[1:5])
#' df2 <- data.frame(C = 1:3, D = letters[1:3])
#' df3 <- data.frame(E = 1:8, F = letters[1:8], G = LETTERS[1:8])
#' #'CBIND(list(df1, df2, df3))
#' 
#' ## makemeNA
#' # Some sample data
#' temp <- data.frame(
#' V1 = c(1:3),
#' V2 = c(1, "*", 3),
#' V3 = c("a", "*", "c"),
#' V4 = c(".", "*", "3"))
#' temp
#' makemeNA(temp, c("*", "."))
#' 
#' \dontshow{rm(df1, df2, df3, temp)}
#' 
NULL



