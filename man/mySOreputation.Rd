\name{mySOreputation}
\alias{mySOreputation}
\title{Parse your reputation page from any of the Stack Exchange sites}
\usage{
  mySOreputation(rep_file)
}
\arguments{
  \item{rep_file}{The path to a text version of your
  reputation page. Windows and Linux users can copy the
  text on the page with select all + copy, and simply use
  "clipboard" instead of saving the contents to a local
  file.}
}
\description{
  It is very easy to \emph{view} a detailed account of your
  reputation at any of the Stack Exchange sites by visiting
  \code{http://"sitename"/reputation} (obviously
  substituting "sitename" for the actual site of interest,
  for example, \url{http://stackoverflow.com/reputation}).
  However, that format is not very user-friendly if you
  want to do any analysis with it. This function parses
  that page into an R \code{data.frame}.
}
\examples{
## This is a real reputation file,
##    but the "question_id" variable is
##    made up.
rep_file <- system.file("soreputation.txt", package = "mrdwabmisc")
readLines(rep_file, 15)
mydf <- mySOreputation(rep_file = rep_file)
head(mydf, 15)
str(mydf)
plot(mydf$date, cumsum(mydf$rep_change))

\dontrun{
library(xts)
mydfx <- xts(mydf$rep_change, mydf$date)
apply.monthly(mydfx, sum)
plot(apply.monthly(mydfx, sum))
}

\dontshow{
rm(mydf, mydfx, rep_file)
}
}
\author{
  Paul Hiemstra provided the base parser. Built upon by
  Ananda Mahto.
}
\references{
  Values for the "actions" variable determined after
  visiting
  \url{http://meta.stackoverflow.com/questions/43004/how-do-i-audit-my-reputation/43005#43005}.
  There is one value not mentioned at that page, coded as
  \code{action_id == 99} and \code{action == Bonus} that
  corresponds to the bonus that a user gets when they have
  above a certain reputation and are active on multiple
  Stack Exchange sites.
}

