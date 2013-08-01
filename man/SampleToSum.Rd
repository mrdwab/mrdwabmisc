\name{SampleToSum}
\alias{SampleToSum}
\title{Draw a random sample that sums to a specified amount}
\usage{
  SampleToSum(Target = 100, VecLen = 10, InRange = 1:100,
    Tolerance = 2, writeProgress = NULL)
}
\arguments{
  \item{Target}{The desired sum of all the samples}

  \item{VecLen}{How many numbers should be in your
  resulting vector?}

  \item{InRange}{What is the acceptable range of values to
  be sampled from?}

  \item{Tolerance}{What is the maximum difference allowed
  between the target and the sum? Set to "0" to match the
  target exactly. In general, the difference is within 5
  anyway, which is reasonable.}

  \item{writeProgress}{If you want a log-file to be written
  that includes \emph{all} the variations tried before
  arriving at a vector that satisfies all the user's
  conditions, specify the output file name (quoted) with
  this argument. Note that in some cases, this might be
  quite a large file with tens-of-thousands of lines!}
}
\description{
  This function creates a random sample of numbers drawn
  from a specified range which sum to a specified amount.
}
\note{
  This function can be notoriously slow, particularly if
  your range is too narrow and your tolerance is too high.
}
\examples{
set.seed(1)
SampleToSum()
SampleToSum(Tolerance = 0)
replicate(5,
 SampleToSum(Target = 1376,
             VecLen = 13,
             InRange = 10:200,
             Tolerance = 0),
 simplify = FALSE)
replicate(5,
 SampleToSum(Target = 1376,
             VecLen = 13,
             InRange = 10:200),
 simplify = FALSE)
}
\author{
  Ananda Mahto
}
\references{
  This function was written as a response to the following
  Stack Overflow question:
  \url{http://stackoverflow.com/q/14684539/1270695}
}
\seealso{
  \code{\link{sample}}, \code{\link{runif}}
}

