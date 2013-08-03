\name{expandrows}
\alias{expandrows}
\title{"Expand" the rows of a dataset}
\usage{
  expandrows(dataset, count, count.is.col = TRUE)
}
\arguments{
  \item{dataset}{The input \code{data.frame}.}

  \item{count}{The vector of counts or the column that
  contains the counts.}

  \item{count.is.col}{Logical. Is the \code{count} value a
  column in the \code{data.frame}? Defaults to
  \code{TRUE}.}
}
\value{
  A \code{data.frame}.
}
\description{
  Replicates each row in a dataset either by a count column
  specified within the dataset or by a value or vector of
  values specified by the user.
}
\examples{
mydf <- data.frame(x = c("a", "b", "q"), y = c("c", "d", "r"), count = c(2, 5, 3))
mydf
expandrows(mydf, "count")
expandrows(mydf, count = 3)
expandrows(mydf, count = 3, count.is.col = FALSE)
expandrows(mydf, count = c(1, 5, 9), count.is.col = FALSE)

\dontshow{rm(mydf)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{rep}}
}

