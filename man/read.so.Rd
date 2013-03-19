\name{read.so}
\alias{read.so}
\title{Read displayed text at Stack Overflow}
\usage{
  read.so(sep = "", header = TRUE, out = "mydf")
}
\arguments{
  \item{sep}{Most of the time, the code shared is space
  separated (which is the default for this function). If
  the separator is any other character, it can be specified
  here.}

  \item{header}{Are headers included?}

  \item{out}{Desired output object name. Defaults to
  \code{mydf}.}
}
\description{
  For many questions at Stack Overflow, the question asker
  does not properly share their question (for example,
  using \code{\link{dput}} or by sharing some commands to
  make up the data). Most of the time, you can just copy
  and paste the text into R using \code{read.table(text =
  "clipboard", header = TRUE, stringsAsFactors = FALSE)}.
  This function is basically a convenience function for the
  above.
}
\details{
  The output of \code{\link{read.so}} is automatically
  assigned to an object in your workspace called
  "\code{mydf}" unless specified using the \code{out}
  argument.
}
\examples{
\dontrun{
## Copy the following text (select and ctrl-c)

# A B
# 1 2
# 3 4
# 5 6

## Now, just type:

read.so()
}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{dput}}, \code{\link{read.table}}
}

