\name{RandomNames}
\alias{RandomNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RandomNames(N = 100, cat = NULL, gender = NULL, MFprob = NULL, dataset = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{cat}{
%%     ~~Describe \code{cat} here~~
}
  \item{gender}{
%%     ~~Describe \code{gender} here~~
}
  \item{MFprob}{
%%     ~~Describe \code{MFprob} here~~
}
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
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
function (N = 100, cat = NULL, gender = NULL, MFprob = NULL, 
    dataset = NULL) 
{
    if (is.null(dataset)) {
        if (!exists("CensusNames1990", where = 1)) {
            if (isTRUE(list.files(pattern = "^CensusNames.RData$") == 
                "CensusNames.RData")) {
                load("CensusNames.RData")
            }
            else {
                ans = readline("\n    CensusNames.RData dataset not found in working directory.\n    CensusNames1990 object not found in workspace. \n\n    Download and load the dataset now? (y/n) ")
                if (ans != "y") 
                  return(invisible())
                require(RCurl)
                baseURL = c("https://raw.github.com/mrdwab/2657-R-Functions/master/")
                temp = getBinaryURL(paste0(baseURL, "data/CensusNames.RData"))
                load(rawConnection(temp), envir = .GlobalEnv)
                message("CensusNames1990 data downloaded from \n", 
                  paste0(baseURL, "data/CensusNames.RData \n"), 
                  "and added to your workspace\n\n")
                rm(temp, baseURL)
            }
        }
        dataset <- CensusNames1990
    }
    TEMP <- dataset
    possiblecats <- c("common", "rare", "average")
    if (all(cat \%in\% possiblecats) == FALSE) 
        stop("cat must be either \"all\", NULL,\n         or a combination of \"common\", \"average\", or \"rare\"")
    possiblegenders <- c("male", "female", "both")
    if (all(gender \%in\% possiblegenders) == FALSE) {
        stop("gender must be either \"both\", NULL, \"male\", or \"female\"")
    }
    if (isTRUE(identical(gender, c("male", "female"))) || isTRUE(identical(gender, 
        c("female", "male")))) {
        gender <- "both"
    }
    if (is.null(cat) || cat == "all") {
        surnames <- TEMP[["surnames"]][["Name"]]
        malenames <- paste("M-", TEMP[["malenames"]][["Name"]], 
            sep = "")
        femalenames <- paste("F-", TEMP[["femalenames"]][["Name"]], 
            sep = "")
    }
    else {
        surnames <- suppressWarnings(with(TEMP[["surnames"]], 
            TEMP[["surnames"]][Category == cat, "Name"]))
        malenames <- paste("M-", suppressWarnings(with(TEMP[["malenames"]], 
            TEMP[["malenames"]][Category == cat, "Name"])), sep = "")
        femalenames <- paste("F-", suppressWarnings(with(TEMP[["femalenames"]], 
            TEMP[["femalenames"]][Category == cat, "Name"])), 
            sep = "")
    }
    if (is.null(gender) || gender == "both") {
        if (is.null(MFprob)) 
            MFprob <- c(0.5, 0.5)
        firstnames <- sample(c(malenames, femalenames), N, replace = TRUE, 
            prob = c(rep(MFprob[1]/length(malenames), length(malenames)), 
                rep(MFprob[2]/length(femalenames), length(femalenames))))
    }
    else if (gender == "female") {
        firstnames <- sample(femalenames, N, replace = TRUE)
    }
    else if (gender == "male") {
        firstnames <- sample(malenames, N, replace = TRUE)
    }
    Surnames <- sample(surnames, N, replace = TRUE)
    temp <- setNames(data.frame(do.call(rbind, strsplit(firstnames, 
        "-"))), c("Gender", "FirstName"))
    cbind(temp, Surnames)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
