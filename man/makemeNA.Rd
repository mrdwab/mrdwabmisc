\name{makemeNA}
\alias{makemeNA}
\title{Make certain values in a \code{data.frame} \code{NA}}
\usage{
  makemeNA(mydf, NAStrings, fixed = TRUE,
    overwrite = FALSE)
}
\arguments{
  \item{mydf}{A \code{data.frame} in which some values need
  to be converted to \code{NA}}

  \item{NAStrings}{The values which have been used to
  represent \code{NA}}

  \item{fixed}{Logical. Is the \code{NAStrings} argument a
  fixed character (or vector of characters) or a regular
  expression? Defaults to \code{TRUE}.}

  \item{overwrite}{Logical. Should the current object be
  overwritten? Defaults to \code{FALSE}}
}
\description{
  Sometimes, after having read in data, one needs to
  replace certain values by \code{\link{NA}}. One approach
  is to use \code{mydf[mydf == "some-character"] <- NA}.
  However, in many cases that results in a
  \code{data.frame} where variables which should be numeric
  end up as characters or factors if the NA string was a
  character to begin with. This function is a convenience
  wrapper around \code{\link{type.convert}} to address such
  problems.
}
\examples{
# Some sample data
temp <- data.frame(
V1 = c(1:3),
V2 = c(1, "*", 3),
V3 = c("a", "*", "c"),
V4 = c(".", "*", "3"))
temp
str(temp)

temp1 <- makemeNA(temp, c("*", "."))
temp1
str(temp1)

# Can make anything NA. Useful for -999 type of NA values
makemeNA(temp, "1")
\dontshow{rm(temp, temp1)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{type.convert}}
}

