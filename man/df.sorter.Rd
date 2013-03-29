\name{df.sorter}
\alias{df.sorter}
\title{Sort a \code{data.frame} by rows or columns}
\usage{
  df.sorter(data, var.order = names(data), col.sort = NULL,
    at.start = TRUE)
}
\arguments{
  \item{data}{The source \code{data.frame}.}

  \item{var.order}{The new order in which you want the
  variables to appear. See Details}

  \item{col.sort}{The columns \emph{within} which there is
  data that need to be sorted. See Details.}

  \item{at.start}{Should the pattern matching be from the
  start of the variable name? Defaults to \code{TRUE}.}
}
\description{
  The \code{\link{df.sorter}} function allows you to sort a
  \code{\link{data.frame}} by columns or rows or both. You
  can also quickly subset data columns by using the
  \code{var.order} argument.
}
\details{
  \emph{var.order} \itemize{ \item Defaults to
  \code{names(data)}, which keeps the variables in the
  original order. \item Variables can be referred to either
  by a vector of their index numbers or by a vector of the
  variable name; partial name matching also works, but
  requires that the partial match identifies similar
  columns uniquely (see Examples). Basic subsetting can
  also be done using \code{var.order} simply by omitting
  the variables you want to drop. } \emph{col.sort}
  \itemize{ \item Defaults to \code{NULL}, which means no
  sorting takes place. Variables can be referred to either
  by a vector of their index numbers or by a vector of the
  variable names; full names must be provided. }
}
\note{
  If you are sorting both by variables and within the
  columns and using numeric indexes as opposed to variable
  names, the \code{col.sort} order should be based on the
  location of the columns in the new \code{data.frame}, not
  the original \code{data.frame}.
}
\examples{
# Make up some data
set.seed(1)
dat = data.frame(id = rep(1:5, each=3), times = rep(1:3, 5),
              measure1 = rnorm(15), score1 = sample(300, 15),
              code1 = replicate(15, paste(sample(LETTERS[1:5], 3),
                                          sep="", collapse="")),
              measure2 = rnorm(15), score2 = sample(150:300, 15),
              code2 = replicate(15, paste(sample(LETTERS[1:5], 3),
                                          sep="", collapse="")))
# Preview your data
dat

# Change the variable order, grouping related columns
# Note that you do not need to specify full variable names,
#    just enough that the variables can be uniquely identified
head(df.sorter(dat, var.order = c("id", "ti", "cod", "mea", "sco")))

# As above, but sorted by 'times' and then 'id'
head(df.sorter(dat,
            var.order = c("id", "tim", "cod", "mea", "sco"),
            col.sort = c(2, 1)))

# Drop 'measure1' and 'measure2', sort by 'times', and 'score1'
head(df.sorter(dat,
            var.order = c("id", "tim", "sco", "cod"),
            col.sort = c(2, 3)))

# Just sort by columns, first by 'times' then by 'id'
head(df.sorter(dat, col.sort = c("times", "id")))

# Pattern matching anywhere in the variable name
head(df.sorter(dat, var.order= "co", at.start=FALSE))
\dontshow{rm(dat)}
}
\author{
  Ananda Mahto
}

