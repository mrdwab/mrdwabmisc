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

#' Extract all names from a \code{data.frame} other than the one listed
#' 
#' A convenience function for \code{setdiff(names(data),
#' -some_vector_of_names-)}.
#' 
#' 
#' @param data The input \code{data.frame}.
#' @param toremove The \code{names} you want to exclude.
#' @return A character vector of the remaining names.
#' @author Ananda Mahto
#' @seealso \code{\link{setdiff}}
#' @examples
#' 
#' mydf <- data.frame(a = 1:2, b = 3:4, c = 5:6)
#' mrdwabmisc:::othernames(mydf, "a")
#' 
#' \dontshow{rm(mydf)}
#' 
othernames <- function(data, toremove) {
  setdiff(names(data), names(data[toremove]))
}
NULL

#' Read concatenated character vectors into a \code{data.frame}
#' 
#' A helper function for the \code{\link{concat.split}} function ("condensed"
#' structure).
#' 
#' 
#' @param data The input data.
#' @param col.prefix The desired column prefix for the output
#' \code{data.frame}.
#' @param sep The character that acts as a delimiter.
#' @return A \code{data.frame}
#' @author Ananda Mahto
#' @seealso \code{read.table}
#' @examples
#' 
#' vec <- c("a,b", "c,d,e", "f, g", "h, i, j,k")
#' mrdwabmisc:::read.concat(vec, "var", ",")
#' 
#' \dontshow{rm(vec)}
#' 
read.concat <- function(data, col.prefix, sep) {
  if (!is.character(data)) data <- as.character(data)
  t1 <- read.table(text = data, sep = sep, fill = TRUE,
                   row.names = NULL, header = FALSE,
                   blank.lines.skip = FALSE, 
                   strip.white = TRUE)
  names(t1) <- paste(col.prefix, seq(ncol(t1)), sep = "_")
  t1
}
NULL

#' Create a binary matrix from a list of values
#' 
#' Create a binary matrix from a list of values
#' 
#' This is primarily a helper function for the \code{\link{concat.split}}
#' function when creating the "expanded" structure. The input is anticipated to
#' be a \code{list} of values obtained using \code{\link{strsplit}}.
#' 
#' @param listOfValues A \code{list} of input values to be inserted in a
#' matrix.
#' @param fill The initializing fill value for the empty matrix.
#' @return A \code{matrix}.
#' @author Ananda Mahto
#' @seealso \code{strsplit}, \code{\link{valueMat}}
#' @examples
#' 
#' invec <- c("1,2,4,5,6", "1,2,4,5,6", "1,2,4,5,6",
#'            "1,2,4,5,6", "1,2,5,6", "1,2,5,6")
#' A <- strsplit(invec, ",")
#' mrdwabmisc:::binaryMat(A)
#' mrdwabmisc:::binaryMat(A, "ZZZ")
#' 
#' \dontshow{rm(invec, A)}
#' 
binaryMat <- function(listOfValues, fill = NA) {
  listOfValues <- lapply(listOfValues, as.numeric)
  ncol <- max(unlist(listOfValues))
  nrow <- length(listOfValues)
  m <- matrix(fill, nrow = nrow, ncol = ncol)
  for (i in 1:nrow) {
    m[i, listOfValues[[i]]] <- 1
  }
  m
}
NULL

#' Create a binary matrix from a list of values
#' 
#' Create a binary matrix from a list of values
#' 
#' This is primarily a helper function for the \code{\link{concat.split}}
#' function when creating the "expanded" structure. The input is anticipated to
#' be a \code{list} of values obtained using \code{\link{strsplit}}.
#' 
#' @param listOfValues A \code{list} of input values to be inserted in a
#' matrix.
#' @param fill The initializing fill value for the empty matrix.
#' @return A \code{matrix}.
#' @author Ananda Mahto
#' @seealso \code{strsplit}, \code{\link{binaryMat}}
#' @examples
#' 
#' invec <- c("1,2,4,5,6", "1,2,4,5,6", "1,2,4,5,6",
#'            "1,2,4,5,6", "1,2,5,6", "1,2,5,6")
#' A <- strsplit(invec, ",")
#' mrdwabmisc:::valueMat(A)
#' mrdwabmisc:::valueMat(A, "ZZZ")
#' 
#' \dontshow{rm(invec, A)}
#' 
valueMat <- function(listOfValues, fill = NA) {
  listOfValues <- lapply(listOfValues, as.numeric)
  ncol <- max(unlist(listOfValues))
  nrow <- length(listOfValues)
  m <- matrix(fill, nrow = nrow, ncol = ncol)
  for (i in 1:nrow) {
    m[i, listOfValues[[i]]] <- listOfValues[[i]]
  }
  m
}
NULL
