\name{round2}
\alias{round2}
\title{Round numbers the way you learned in school}
\usage{
  round2(x, digits = 0)
}
\arguments{
  \item{x}{The number (or vector of numbers) that needs
  rounding.}

  \item{digits}{The number of decimal places in the
  output.}
}
\description{
  The \code{\link{round2}} function rounds numbers in the
  way you probably learned in school, that is, round up to
  the next number for values of 5 and above.
}
\details{
  To reduce bias in rounding, R's \code{\link{round}}
  function uses a "round-to-even" approach. Still, many
  people are surprised when they find that R's \code{round}
  function will return the same value for \code{round(1.5)}
  and \code{round(2.5)}. This function uses the rounding
  approach found in most school lessons and in software
  like Excel to make the results comparable.
}
\examples{
input <- seq(from = 0.5, by = 1, length.out = 10)
round(input)
round2(input)
round(input/10, digits = 1)
round2(input/10, digits = 1)
\dontshow{rm(input)}
}
\author{
  Unknown (see "References")
}
\references{
  Function originally found in an anonymous comment at the
  Statistically Significant blog. See
  \url{http://www.webcitation.org/68djeLBtJ}
}
\seealso{
  \code{\link{round}}
}

