\name{stratified}
\alias{stratified}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stratified(df, group, size, seed = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{group}{
%%     ~~Describe \code{group} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (df, group, size, seed = NULL, ...) 
{
    df.interaction <- interaction(df[group])
    df.table <- table(df.interaction)
    df.split <- split(df, df.interaction)
    if (length(size) > 1) {
        if (length(size) != length(df.split)) 
            stop("Number of groups is ", length(df.split), " but number of sizes supplied is ", 
                length(size))
        if (is.null(names(size))) {
            n <- setNames(size, names(df.split))
            message(sQuote("size"), " vector entered as:\n\nsize = structure(c(", 
                paste(n, collapse = ", "), "),\n.Names = c(", 
                paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
        }
        else {
            ifelse(all(names(size) \%in\% names(df.split)), n <- size[names(df.split)], 
                stop("Named vector supplied with names ", paste(names(size), 
                  collapse = ", "), "\n but the names for the group levels are ", 
                  paste(names(df.split), collapse = ", ")))
        }
    }
    else if (size < 1) {
        n <- round(df.table * size, digits = 0)
    }
    else if (size >= 1) {
        if (all(df.table >= size)) {
            n <- setNames(rep(size, length.out = length(df.split)), 
                names(df.split))
        }
        else {
            message("Some groups\n---", paste(names(df.table[df.table < 
                size]), collapse = ", "), "---\ncontain fewer observations", 
                " than desired number of samples.\n", "All observations have been returned from those groups.")
            n <- c(sapply(df.table[df.table >= size], function(x) x = size), 
                df.table[df.table < size])
        }
    }
    seedme <- ifelse(is.null(seed), "No", "Yes")
    temp <- switch(seedme, No = {
        temp <- lapply(names(df.split), function(x) df.split[[x]][sample(df.table[x], 
            n[x], ...), ])
    }, Yes = {
        temp <- lapply(names(df.split), function(x) {
            set.seed(seed)
            df.split[[x]][sample(df.table[x], n[x], ...), ]
        })
    })
    rm(.Random.seed, envir = .GlobalEnv)
    do.call("rbind", temp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
