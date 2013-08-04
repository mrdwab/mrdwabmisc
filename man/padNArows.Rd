\name{padNArows}
\alias{padNArows}
\title{Pad rows of a \code{data.frame} with \code{NA}}
\usage{
  padNArows(mydata, rowsneeded)
}
\arguments{
  \item{mydata}{The input \code{data.frame}}

  \item{rowsneeded}{The desired number of rows.}
}
\value{
  A \code{data.frame} with rows of \code{NA} values
  appended to the bottom.
}
\description{
  A helper function for \code{\link{CBIND}} to pad a
  \code{data.frame} with \code{NA} to make
  \code{\link{cbind}} work as expected.
}
\examples{
mydf <- data.frame(a = 1:3, b = c("a", "b", "c"))
padNArows(mydf, 10)

\dontshow{rm(mydf)}
}
\author{
  Ananda Mahto
}

