\name{aggregate2}
\alias{aggregate2}
\title{Perform multiple aggregation functions on grouped data}
\usage{
  aggregate2(data, aggs, ids, funs = NULL, ...)
}
\arguments{
  \item{data}{Your \code{data.frame}}

  \item{aggs}{The variables that need to be aggregated,
  specified as a character vector.}

  \item{ids}{The variables that serve as grouping
  variables, specified as a character vector.}

  \item{funs}{The functions that you want to apply,
  specified as a character vector.}

  \item{\dots}{Further arguments to \code{aggregate}.
  Really only useful for the \code{subset} argument.}
}
\description{
  Base R's \code{\link{aggregate}} function allows you to
  specify multiple functions when aggregating. However, the
  output of such commands is a \code{data.frame} where the
  aggregated "columns" are actually matrices.
  \code{\link{aggregate2}} is a basic wrapper around
  \code{aggregate} that outputs a regular \code{data.frame}
  instead.
}
\note{
  This function essentially constructs a \code{formula}
  that can be used with \code{aggregate} and keeps track of
  the names of the aggregation functions you have applied
  to create new variable names. This function is not very
  useful when the output of \code{FUN} would already output
  a matrix (for example, if \code{FUN = fivenum} or
  \code{FUN = summary}). In such cases, it is recommended
  to use base R's \code{aggregate} with a \code{do.call}.
  For example: \code{do.call("data.frame", aggregate(. ~
  Species, iris, summary))}.
}
\examples{
# One-to-one, two functions
(temp1a <- aggregate(weight ~ feed, data = chickwts,
             function(x) cbind(mean(x), sum(x))))
str(temp1a)
(temp1b <- aggregate2(chickwts, "weight", "feed", c("mean", "sum")))
str(temp1b)

# Many-to-many, two functions
(temp2a <- aggregate(cbind(ncases, ncontrols) ~ alcgp + tobgp, data = esoph,
             function(x) cbind(sum(x), mean(x))))
str(temp2a)
(temp2b <- aggregate2(esoph, c("ncases", "ncontrols"),
              c("alcgp", "tobgp"), c("sum", "mean")))
str(temp2b)

# Dot notation
(temp3a <- aggregate(len ~ ., data = ToothGrowth,
             function(x) cbind(sum(x), mean(x))))
str(temp3a)
(temp3b <- aggregate2(ToothGrowth, "len", ".", c("sum", "mean")))
str(temp3b)
\dontshow{rm(temp1a, temp1b, temp2a, temp2b, temp3a, temp3b)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{aggregate}}
}

