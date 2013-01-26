\name{row.extractor}
\alias{row.extractor}
\title{Extract min/median/max/quantile rows from a \code{data.frame}}
\usage{
  row.extractor(data, extract.by, what = "all")
}
\arguments{
  \item{data}{The source \code{data.frame}.}

  \item{extract.by}{The column which will be used as the
  reference for extraction; can be specified either by the
  column number or the variable name.}

  \item{what}{Options are \code{"min"} (for all rows
  matching the minimum value), \code{"median"} (for the
  median row or rows), \code{"max"} (for all rows matching
  the maximum value), or \code{"all"} (for min, median, and
  max); alternatively, a numeric vector can be specified
  with the desired quantiles, for instance \code{c(0, .25,
  .5, .75, 1)}.}
}
\description{
  The \code{\link{row.extractor}} function takes a
  \code{data.frame} and extracts rows with the min, median,
  or max values of a given variable, or extracts rows with
  specific quantiles of a given variable.
}
\examples{
# Make up some data
set.seed(1)
dat = data.frame(V1 = 1:50, V2 = rnorm(50),
                V3 = round(abs(rnorm(50)), digits=2),
                V4 = sample(1:30, 50, replace=TRUE))
# Get a sumary of the data
summary(dat)

# Get the rows corresponding to the 'min', 'median', and 'max' of 'V4'
row.extractor(dat, 4)

# Get the 'min' rows only, referenced by the variable name
row.extractor(dat, "V4", "min")

# Get the 'median' rows only. Notice that there are two rows
#    since we have an even number of cases and true median
#    is the mean of the two central sorted values
row.extractor(dat, "V4", "median")

# Get the rows corresponding to the deciles of 'V3'
row.extractor(dat, "V3", seq(0.1, 1, 0.1))
}
\author{
  Ananda Mahto
}
\references{
  \itemize{ \item \code{which.quantile} function by
  cbeleites:
  \url{http://stackoverflow.com/users/755257/cbeleites}
  \item See:
  \url{http://stackoverflow.com/q/10256503/1270695} }
}
\seealso{
  \code{\link{min}}, \code{\link{max}},
  \code{\link{median}}, \code{\link{which.min}},
  \code{\link{which.max}}, \code{\link{quantile}}
}

