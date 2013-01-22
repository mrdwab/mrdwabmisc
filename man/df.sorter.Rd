\name{df.sorter}
\alias{df.sorter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
df.sorter(data, var.order = names(data), col.sort = NULL, at.start = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{var.order}{
%%     ~~Describe \code{var.order} here~~
}
  \item{col.sort}{
%%     ~~Describe \code{col.sort} here~~
}
  \item{at.start}{
%%     ~~Describe \code{at.start} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, var.order = names(data), col.sort = NULL, at.start = TRUE) 
{
    if (is.numeric(var.order)) 
        var.order = colnames(data)[var.order]
    else var.order = var.order
    if (isTRUE(at.start)) {
        x = unlist(lapply(var.order, function(x) sort(grep(paste("^", 
            x, sep = "", collapse = ""), names(data), value = TRUE))))
    }
    else if (!isTRUE(at.start)) {
        x = unlist(lapply(var.order, function(x) sort(grep(x, 
            names(data), value = TRUE))))
    }
    y = data[, x]
    if (is.null(col.sort)) {
        y
    }
    else if (is.numeric(col.sort)) {
        y[do.call(order, y[colnames(y)[col.sort]]), ]
    }
    else if (!is.numeric(col.sort)) {
        y[do.call(order, y[col.sort]), ]
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
