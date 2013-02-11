\docType{package}
\name{mrdwabmisc-package}
\alias{mrdwabmisc}
\alias{mrdwabmisc-package}
\title{mrdwabmisc}
\description{
  Miscellaneous R functions, some utility, and others to
  clean and organize data.
}
\details{
  \tabular{ll}{ Package: \tab mrdwabmisc\cr Type: \tab
  Package\cr Version: \tab 1.0\cr Date: \tab 2013-01-22\cr
  License: \tab GPL-2\cr }
}
\examples{
## concat.split
data(concatenated)
head(concat.test)
head(concat.split(concat.test, "Likes", drop.col = TRUE))
\dontshow{rm(concat.test)}

## sample.size
sample.size(population = 300)
sample.size(population = 300, c.lev = 97)

## stringseed.sampling
stringseed.sampling("Santa Barbara", 1920, 100)

## table2df
table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph),
         as.multitable = TRUE, direction = "wide")[[1]]
head(table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph)))
}
\author{
  Ananda Mahto

  Maintainer: Ananda Mahto <mrdwab@gmail.com>
}
\seealso{
  \code{\link[AMsnippets:AMsnippets-package]{AMsnippets}}
  contains a related set of utility functions, but the
  packages do not depend on each other.
}
\keyword{package}

