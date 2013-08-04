\name{concat.split}
\alias{concat.split}
\title{Split concatenated cells in a \code{data.frame}}
\usage{
  concat.split(data, split.col, sep = ",",
    structure = "compact", mode = NULL, drop.col = FALSE,
    fixed = FALSE, fill = NA)
}
\arguments{
  \item{data}{The source data.frame}

  \item{split.col}{The variable that needs to be split; can
  be specified either by the column number or the variable
  name.}

  \item{sep}{The character separating each value (defaults
  to ",").}

  \item{structure}{Can be either "compact", "expanded", or
  "list". Defaults to "compact". See Details.}

  \item{mode}{Can be either binary or value (where binary
  is default and it recodes values to 1 or NA, like Boolean
  data, but without assuming 0 when data is not available).
  This setting only applies when structure = "expanded"; a
  warning message will be issued if used with other
  structures.}

  \item{drop.col}{Logical (whether to remove the original
  variable from the output or not). Defaults to
  \code{FALSE}.}

  \item{fixed}{Is the input for the \code{sep} value
  \emph{fixed}, or a \emph{regular expression}? See
  Details.}
}
\description{
  The \code{concat.split} function takes a column with
  multiple values, splits the values into a \code{list} or
  into separate columns, and returns a new
  \code{data.frame}.
}
\details{
  \emph{structure} \itemize{ \item \code{"compact"} creates
  as many columns as the maximum length of the resulting
  split. This is the most useful general-case application
  of this function. \item When the input is numeric,
  \code{"expanded"} creates as many columns as the maximum
  value of the input data. This is most useful when
  converting to \code{mode = "binary"}. \item \code{"list"}
  creates a single new column that is structurally a
  \code{\link{list}} within a \code{\link{data.frame}}. }
  \emph{fixed} \itemize{ \item When \code{structure =
  "expanded"} or \code{structure = "list"}, it is possible
  to supply a a regular expression containing the
  characters to split on.  For example, to split on
  \code{","}, \code{";"}, or \code{"|"}, you can set
  \code{sep = ",|;|\|"} or \code{sep = "[,;|]"}, and
  \code{fixed = FALSE} to split on any of those
  characters.}
}
\note{
  If using \code{structure = "compact"}, the value for
  \code{sep} can only be a single character. See the
  "Advanced Usage" example of how to specify multiple
  characters for batch conversion of columns.
}
\examples{
## Load some data
temp <- head(concat.test)

# Split up the second column, selecting by column number
concat.split(temp, 2)

# ... or by name, and drop the offensive first column
concat.split(temp, "Likes", drop.col = TRUE)

# The "Hates" column uses a different separator
concat.split(temp, "Hates", sep = ";", drop.col = TRUE)

\dontrun{
# You'll get a warning here, when trying to retain the original values
concat.split(temp, 2, mode = "value", drop.col = TRUE)
}

# Try again. Notice the differing number of resulting columns
concat.split(temp, 2, structure = "expanded",
mode = "value", drop.col = TRUE)

# Let's try splitting some strings... Same syntax
concat.split(temp, 3, drop.col = TRUE)

# Split up the "Likes column" into a list variable; retain original column
head(concat.split(concat.test, 2, structure = "list", drop.col=FALSE))

# View the structure of the output to verify
# that the new column is a list; note the
# difference between "Likes" and "Likes_list".
str(concat.split(temp, 2, structure = "list", drop.col=FALSE))
}
\author{
  Ananda Mahto
}
\references{
  \itemize{ \item See
  \url{http://stackoverflow.com/q/10100887/1270695} \item
  The \code{"condensed"} setting was inspired by an answer
  from David Winsemius to a question at Stack Overflow.
  See: \url{http://stackoverflow.com/a/13924245/1270695} }
}
\seealso{
  \code{\link{concat.split.condensed}},
  \code{\link{concat.split.expanded}},
  \code{\link{concat.split.list}},
  \code{\link{concat.split.multiple}}
}

