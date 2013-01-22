\name{stringseed.sampling}
\alias{stringseed.sampling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stringseed.sampling(seedbase, N, n, write.output = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seedbase}{
%%     ~~Describe \code{seedbase} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{write.output}{
%%     ~~Describe \code{write.output} here~~
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
function (seedbase, N, n, write.output = FALSE) 
{
    require(digest)
    hexval = paste0("0x", sapply(seedbase, digest, "crc32"))
    seeds = type.convert(hexval)\%\%.Machine$integer.max
    seedbase = as.character(seedbase)
    temp <- data.frame(seedbase, N, n, seeds)
    if (length(seedbase) == 1) {
        set.seed(temp$seeds)
        sample.list <- sample(temp$N, temp$n)
    }
    else {
        sample.list <- setNames(apply(temp[-1], 1, function(x) {
            set.seed(x[3])
            sample(x[1], x[2])
        }), temp[, 1])
    }
    temp <- list(input = data.frame(seedbase = seedbase, populations = N, 
        samplesizes = n, seeds = seeds), samples = sample.list)
    if (isTRUE(write.output)) {
        write.csv(temp[[1]], file = paste("Sample frame generated on", 
            Sys.Date(), ".csv", collapse = ""))
        capture.output(temp[[2]], file = paste("Samples generated on", 
            Sys.Date(), ".txt", collapse = ""))
    }
    rm(.Random.seed, envir = globalenv())
    temp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
