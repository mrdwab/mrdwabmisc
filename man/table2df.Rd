\name{table2df}
\alias{table2df}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
table2df(mytable, as.multitable = FALSE, direction = "wide")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mytable}{
%%     ~~Describe \code{mytable} here~~
}
  \item{as.multitable}{
%%     ~~Describe \code{as.multitable} here~~
}
  \item{direction}{
%%     ~~Describe \code{direction} here~~
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
function (mytable, as.multitable = FALSE, direction = "wide") 
{
    if (isTRUE(as.multitable)) {
        multitablecheck <- ifelse(length(dim(mytable)) == 2, 
            "NoMT", "MT")
        if (isTRUE(as.multitable) && multitablecheck == "NoMT") 
            warning(">>as.multitable<< set to TRUE, but evaluates to FALSE.\n              Defaulting to basic table to data.frame conversion methods.")
        switch(multitablecheck, NoMT = {
            as.multitable <- FALSE
        }, MT = {
            as.multitable <- TRUE
        })
    }
    tablearray2list <- function(dataset) {
        y <- ls()
        temp <- dim(dataset)[-c(1, 2)]
        temp1 <- capture.output(dataset)
        tempnames <- gsub(", , ", "", temp1[grep(", , ", temp1)])
        tempnames <- sapply(strsplit(tempnames, " = |, "), function(x) paste(x[1:length(x)\%\%2 == 
            0], collapse = "."))
        combinations <- expand.grid(lapply(temp, seq))
        tempsets <- apply(combinations, 1, function(x) paste(y, 
            "[ , , ", paste(x, collapse = ", "), "]"))
        mylist <- lapply(tempsets, function(x) eval(parse(text = x)))
        names(mylist) <- tempnames
        mylist
    }
    directionwide <- function(mydata) {
        ifelse(class(mydata) == "ftable", mydata <- mydata, mydata <- ftable(mydata))
        dfrows <- expand.grid(rev(attr(mydata, "row.vars")))
        dfcols <- as.data.frame.matrix(mydata)
        names(dfcols) <- interaction(expand.grid(attr(mydata, 
            "col.vars")))
        cbind(dfrows, dfcols)
    }
    directionlong <- function(mydata) {
        mydata <- as.table(mydata)
        as.data.frame(mydata)
    }
    if (isTRUE(as.multitable)) {
        temp <- tablearray2list(mytable)
        switch(direction, wide = {
            lapply(temp, directionwide)
        }, long = {
            lapply(temp, directionlong)
        }, stop(">>direction<< must be either wide or long"))
    }
    else {
        switch(direction, wide = {
            directionwide(mytable)
        }, long = {
            directionlong(mytable)
        }, stop(">>direction<< must be either wide or long"))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
