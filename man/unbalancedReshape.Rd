\name{unbalancedReshape}
\alias{unbalancedReshape}
\title{Reshape unbalanced wide data to a semi-long data form}
\usage{
  unbalancedReshape(data, id.vars, sep = ".",
    dropNA = TRUE)
}
\arguments{
  \item{data}{The source \code{data.frame}}

  \item{id.vars}{The variables that should be treated as id
  variables}

  \item{sep}{The separator used. Defaults to \code{.}}

  \item{dropNA}{Logical. If all the measure variables for
  some cases results in a row of \code{NA} values, shoud
  that row be dropped? Defaults to \code{TRUE}}
}
\description{
  Base R's \code{\link{reshape}} function cannot easily
  handle unbalanced wide data.
  \code{\link[reshape2:melt]{melt}}, from "reshape2" puts
  everything into a very long \code{data.frame}. This
  function returns a semi-long \code{data.frame} after
  automatically "balancing" the wide measures.
}
\note{
  Attributes from the output of \code{reshape} are
  retained, allowing you to easily re-convert to the wide
  form, if necessary.
}
\examples{
data(concatenated)
temp <- head(do.call(cbind,
            c(concat.test[1],
              lapply(1:(ncol(concat.test)-1),
              function(x) {
              splitchars = c(",", ",", ";")
              concat.split(concat.test[-1][x], 1,
                           splitchars[x],
                           drop.col=TRUE)
                           }))))
temp

\dontrun{
## Because the data are unbalanced, we can't just use this
reshape(temp, direction = "long", idvar = "Name",
       varying = 2:ncol(temp), sep = "_")
}

unbalancedReshape(temp, id.vars = "Name", sep = "_")

\dontrun{
## Compare with melt from reshape2
library(reshape2)
melt(temp, id.vars = "Name")
}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{reshape}}, \code{\link[reshape2:melt]{melt}}
}

