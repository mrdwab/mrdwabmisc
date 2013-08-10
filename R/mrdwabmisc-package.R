

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





#' Example dataset with concatenated cells
#' 
#' This is a sample dataset to demonstrate the different features of the
#' \code{\link{concat.split}} function.
#' 
#' 
#' @name concat.test
#' @aliases concatenated concat.test
#' @docType data
#' @format A data.frame in which many columns contain concatenated cells
#' @keywords datasets
NULL





#' mrdwabmisc
#' 
#' Miscellaneous R functions, some utility, and others to clean and organize
#' data.
#' 
#' \tabular{ll}{ Package: \tab mrdwabmisc\cr Type: \tab Package\cr Version:
#' \tab 1.0\cr Date: \tab 2013-01-22\cr License: \tab GPL-2\cr }
#' 
#' @name mrdwabmisc-package
#' @aliases mrdwabmisc mrdwabmisc-package
#' @docType package
#' @author Ananda Mahto
#' 
#' Maintainer: Ananda Mahto <mrdwab@@gmail.com>
#' @keywords package
#' @examples
#' 
#' ## concat.split
#' data(concatenated)
#' head(concat.test)
#' head(concat.split(concat.test, "Likes", drop = TRUE))
#' \dontshow{rm(concat.test)}
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
#' ## FacsToChars
#' dat <- data.frame(title = c("title1", "title2", "title3"),
#'          author = c("author1", "author2", "author3"),
#'          customerID = c(1, 2, 1))
#' str(dat)
#' FacsToChars(dat, overwrite = TRUE)
#' str(dat)
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
#' \dontshow{rm(dat, df1, df2, df3, temp)}
#' 
NULL



