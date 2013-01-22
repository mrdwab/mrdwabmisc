\name{row.extractor}
\alias{row.extractor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
row.extractor(data, extract.by, what = "all")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{extract.by}{
%%     ~~Describe \code{extract.by} here~~
}
  \item{what}{
%%     ~~Describe \code{what} here~~
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
function (data, extract.by, what = "all") 
{
    if (is.numeric(extract.by)) {
        extract.by = extract.by
    }
    else if (is.numeric(extract.by) != 0) {
        extract.by = which(colnames(data) \%in\% "extract.by")
    }
    if (is.character(what)) {
        which.median = function(data, extract.by) {
            a = data[, extract.by]
            if (length(a)\%\%2 != 0) {
                which(a == median(a))
            }
            else if (length(a)\%\%2 == 0) {
                b = sort(a)[c(length(a)/2, length(a)/2 + 1)]
                c(max(which(a == b[1])), min(which(a == b[2])))
            }
        }
        X1 = data[which(data[extract.by] == min(data[extract.by])), 
            ]
        X2 = data[which(data[extract.by] == max(data[extract.by])), 
            ]
        X3 = data[which.median(data, extract.by), ]
        if (identical(what, "min")) {
            X1
        }
        else if (identical(what, "max")) {
            X2
        }
        else if (identical(what, "median")) {
            X3
        }
        else if (identical(what, "all")) {
            rbind(X1, X3, X2)
        }
    }
    else if (is.numeric(what)) {
        which.quantile <- function(data, extract.by, what, na.rm = FALSE) {
            x = data[, extract.by]
            if (!na.rm & any(is.na(x))) 
                return(rep(NA_integer_, length(what)))
            o <- order(x)
            n <- sum(!is.na(x))
            o <- o[seq_len(n)]
            nppm <- n * what - 0.5
            j <- floor(nppm)
            h <- ifelse((nppm == j) & ((j\%\%2L) == 0L), 0, 1)
            j <- j + h
            j[j == 0] <- 1
            o[j]
        }
        data[which.quantile(data, extract.by, what), ]
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
