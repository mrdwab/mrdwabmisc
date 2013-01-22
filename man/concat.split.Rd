\name{concat.split}
\alias{concat.split}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
concat.split(data, split.col, sep = ",", structure = "compact", mode = NULL, drop.col = FALSE, fixed = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{split.col}{
%%     ~~Describe \code{split.col} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
}
  \item{structure}{
%%     ~~Describe \code{structure} here~~
}
  \item{mode}{
%%     ~~Describe \code{mode} here~~
}
  \item{drop.col}{
%%     ~~Describe \code{drop.col} here~~
}
  \item{fixed}{
%%     ~~Describe \code{fixed} here~~
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
function (data, split.col, sep = ",", structure = "compact", 
    mode = NULL, drop.col = FALSE, fixed = FALSE) 
{
    if (is.numeric(split.col)) 
        split.col = split.col
    else split.col = which(colnames(data) \%in\% split.col)
    a = as.character(data[, split.col])
    b = strsplit(a, sep, fixed = fixed)
    temp <- switch(structure, compact = {
        t1 <- read.table(text = a, sep = sep, fill = TRUE, row.names = NULL, 
            header = FALSE, blank.lines.skip = FALSE, strip.white = TRUE)
        names(t1) <- paste(names(data[split.col]), seq(ncol(t1)), 
            sep = "_")
        if (!is.null(mode)) warning("\n                        'mode' supplied but ignored. \n                        'mode' setting only applicable \n                        when structure='expanded'.")
        if (isTRUE(drop.col)) cbind(data[-split.col], t1) else cbind(data, 
            t1)
    }, list = {
        varname = paste(names(data[split.col]), "list", sep = "_")
        if (suppressWarnings(is.na(try(max(as.numeric(unlist(b))))))) {
            data[varname] = list(lapply(lapply(b, as.character), 
                function(x) gsub("^\\s+|\\s+$", "", x)))
        } else if (!is.na(try(max(as.numeric(unlist(b)))))) {
            data[varname] = list(lapply(b, as.numeric))
        }
        if (!is.null(mode)) warning("\n                        'mode' supplied but ignored. \n                        'mode' setting only applicable \n                        when structure='expanded'.")
        if (isTRUE(drop.col)) data[-split.col] else data
    }, expanded = {
        if (suppressWarnings(is.na(try(max(as.numeric(unlist(b))))))) {
            what = "string"
            ncol = max(unlist(lapply(b, function(i) length(i))))
        } else if (!is.na(try(max(as.numeric(unlist(b)))))) {
            what = "numeric"
            ncol = max(as.numeric(unlist(b)))
        }
        temp1 <- switch(what, string = {
            temp = as.data.frame(t(sapply(b, "[", 1:ncol)))
            names(temp) = paste(names(data[split.col]), 1:ncol, 
                sep = "_")
            temp = apply(temp, 2, function(x) gsub("^\\s+|\\s+$", 
                "", x))
            temp1 = cbind(data, temp)
        }, numeric = {
            temp = lapply(b, as.numeric)
            m = matrix(nrow = nrow(data), ncol = ncol)
            for (i in 1:nrow(data)) {
                m[i, temp[[i]]] = temp[[i]]
            }
            m = setNames(data.frame(m), paste(names(data[split.col]), 
                1:ncol, sep = "_"))
            if (is.null(mode)) mode = "binary"
            temp1 <- switch(mode, binary = {
                cbind(data, replace(m, m != "NA", 1))
            }, value = {
                cbind(data, m)
            }, stop("'mode' must be 'binary' or 'value'"))
        })
        if (isTRUE(drop.col)) temp1[-split.col] else temp1
    }, stop("'structure' must be either 'compact', 'expanded', or 'list'"))
    temp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
