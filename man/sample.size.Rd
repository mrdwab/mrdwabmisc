\name{sample.size}
\alias{sample.size}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sample.size(population, samp.size = NULL, c.lev = 95, c.int = NULL, what = "sample", distribution = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{population}{
%%     ~~Describe \code{population} here~~
}
  \item{samp.size}{
%%     ~~Describe \code{samp.size} here~~
}
  \item{c.lev}{
%%     ~~Describe \code{c.lev} here~~
}
  \item{c.int}{
%%     ~~Describe \code{c.int} here~~
}
  \item{what}{
%%     ~~Describe \code{what} here~~
}
  \item{distribution}{
%%     ~~Describe \code{distribution} here~~
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
function (population, samp.size = NULL, c.lev = 95, c.int = NULL, 
    what = "sample", distribution = 50) 
{
    z = qnorm(0.5 + c.lev/200)
    if (identical(what, "sample")) {
        if (is.null(c.int)) {
            c.int = 5
            message("NOTE! Confidence interval set to 5.\n      To override, set >> c.int << to desired value.\n")
        }
        else if (!is.null(c.int) == 1) {
            c.int = c.int
        }
        if (!is.null(samp.size)) {
            message("NOTE! >> samp.size << value provided but ignored.\n      See output for actual sample size(s).\n")
        }
        ss = (z^2 * (distribution/100) * (1 - (distribution/100)))/((c.int/100)^2)
        samp.size = ss/(1 + ((ss - 1)/population))
    }
    else if (identical(what, "confidence")) {
        if (is.null(samp.size)) {
            stop("Missing >> samp.size << with no default value.")
        }
        if (!is.null(c.int)) {
            message("NOTE! >> c.int << value provided but ignored.\n      See output for actual confidence interval value(s).\n")
        }
        ss = ((population * samp.size - samp.size)/(population - 
            samp.size))
        c.int = round(sqrt((z^2 * (distribution/100) * (1 - (distribution/100)))/ss) * 
            100, digits = 2)
    }
    else if (what \%in\% c("sample", "confidence") == 0) {
        stop(">> what << must be either -sample- or -confidence-")
    }
    RES = data.frame(population = population, conf.level = c.lev, 
        conf.int = c.int, distribution = distribution, sample.size = round(samp.size, 
            digits = 0))
    RES
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
