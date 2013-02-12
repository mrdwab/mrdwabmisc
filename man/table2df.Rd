\name{table2df}
\alias{table2df}
\title{Convert \code{table} objects to \code{data.frame}s}
\usage{
  table2df(mytable, as.multitable = FALSE,
    direction = "wide")
}
\arguments{
  \item{mytable}{The \code{table} object you want to
  convert into a \code{data.frame}. This can be an object
  in your workspace, or you can make the call to
  \code{table}, \code{ftable}, or \code{xtabs} as the
  \code{mytable} argument to this function.}

  \item{as.multitable}{Logical; defaults to \code{FALSE}.
  Some methods, for instance \code{xtabs} and \code{table},
  will create an array of tables as the output when more
  than two variables are being tabulated. \itemize{ \item
  If \code{as.multitable} is \code{TRUE}, the function will
  return a list of \code{data.frame}s. \item If
  \code{as.multitable} is \code{FALSE}, the function will
  convert the object to an \code{ftable} object before
  performing the transformation. }}

  \item{direction}{Can be either \code{"long"} or
  \code{"wide"}. \itemize{ \item If \code{"long"}, the
  frequencies will all be tabulated into a single column.
  This is the same behavior you will generally get if you
  used \code{as.data.frame} on a \code{table} object. \item
  If "wide", the tabular format is retained. }}
}
\description{
  The \code{\link{table2df}} function takes an object of
  "class" \code{\link{table}}, \code{\link{ftable}}, and
  \code{\link{xtabs}} and converts them to
  \code{\link{data.frame}}s, while retaining as many of the
  name details as possible.
}
\examples{
# Make up some data:
set.seed(1)
handedness <- data.frame(
  gender = sample(c("Female", "Male", "Unknown"), 200, replace = TRUE),
  handedness = sample(c("Right", "Left", "Ambidextrous"),
                      200, replace = TRUE, prob = c(.7, .2, .1)),
  fav.col = sample(c("Red", "Orange", "Yellow", "Green", "Blue",
                   "Indigo", "Violet", "Black", "White"),
                   200, replace = TRUE),
  fav.shape = sample(c("Triangle", "Circle", "Square", "Pentagon", "Hexagon",
                     "Oval", "Octagon", "Rhombus", "Trapezoid"),
                     200, replace = TRUE),
  computer = sample(c("Win", "Mac", "Lin"), 200, replace = TRUE,
                    prob = c(.5, .25, .25)))
# Preview the data
list(head(handedness), tail(handedness))

# A very basic table
HT1 <- with(handedness, table(gender, handedness))
HT1
table2df(HT1)
#'table2df(HT1, direction = "long")

# Another basic table
HT2 <- with(handedness, table(fav.col, computer))
HT2
table2df(HT2)

# This will create multiple tables, one for each possible computer value
HT3 <- with(handedness, table(gender, fav.col, computer))
HT3

# Default settings
table2df(HT3)

# As a list of data.frames
table2df(HT3, as.multitable = TRUE)

# As above, but with the output in long format
#   Only showing the first three lines of each data.frame
lapply(table2df(HT3, as.multitable = TRUE, direction = "long"), head, 3)

# Applied to an ftable
HT4 <- ftable(handedness,
              col.vars="fav.col",
              row.vars=c("gender", "computer"))
HT4
table2df(HT4)

# Applied to a single-row table
table2df(xtabs(breaks ~ wool, warpbreaks))

## ======================================= ##
## ========== OTHER EXAMPLES ============= ##

\dontrun{
table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph))
table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph),
         direction = "long")
table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph),
         as.multitable = TRUE, direction = "long")
table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph),
         as.multitable = TRUE, direction = "wide")
}

\dontshow{rm(HT1, HT2, HT3, HT4, handedness)}
}
\author{
  Ananda Mahto
}
\references{
  The \code{expand.grid} method for remaking the columns
  from an \code{ftable} was described by Kohske at
  \url{http://stackoverflow.com/a/6463137/1270695}.
}
\seealso{
  \code{\link{table}}, \code{\link{ftable}},
  \code{\link{xtabs}}
}

