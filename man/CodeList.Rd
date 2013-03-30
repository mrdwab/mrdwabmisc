\name{CodeList}
\alias{CodeList}
\title{Create a list of code blocks from an input character vector}
\usage{
  CodeList(x)
}
\arguments{
  \item{x}{The input character vector}
}
\value{
  A character vector of just the code blocks from a Stack
  Overflow question.
}
\description{
  A helper function for \code{\link{SOCodeBlocks}} to
  extract the lines in the character vector corresponding
  to a code block.
}
\author{
  Ananda Mahto
}

