\name{stringseed.sampling}
\alias{stringseed.sampling}
\title{Use any alphanumeric input as a seed}
\usage{
  stringseed.sampling(seedbase, N, n, write.output = FALSE)
}
\arguments{
  \item{seedbase}{A vector of seeds to be used for
  sampling.}

  \item{N}{The "population" from which to draw the sample.}

  \item{n}{The desired number of samples.}

  \item{write.output}{Logical. Should the output be written
  to a file? Defaults to \code{FALSE}. If \code{TRUE}, a
  csv file is written with the sample "metadata", and a
  plain text file is written with the details of the
  resulting sample. The names of the files written are
  \code{"Sample frame generated on {date the script was
  run} .csv"} and \code{"Samples generated on {date the
  script was run} .txt"} and will be found in your current
  working directory.}
}
\value{
  This function returns a list with the \code{class}
  "stringSeedSampling" and uses a custom \code{print}
  method. The list items are: \itemize{ \item \code{input}:
  The "metadata" to remind you of your input parameters.
  \item \code{samples}: The samples resulting from the
  specified input parameters. In the case of batch sampling
  being used, \code{samples} will be a named list. } Use
  \code{print.default(your-object-here)} to view the
  underlying list.
}
\description{
  The \code{\link{stringseed.sampling}} function is
  designed as a batch sampling function that allows the
  user to specify any alphanumeric input as the seed
  \emph{per sample in the batch}.
}
\examples{
# We'll use a data.frame with a list of village names, the population,
#   and the desired samples as our columns. The function will use the
#   village names to generate a unique seed for each village before
#   drawing the sample.
myListOfPlaces <- data.frame(
villageName = c("Melakkal", "Sholavandan", "T. Malaipatti"),
population = c(120, 130, 140),
requiredSample = c(30, 25, 12))
myListOfPlaces

stringseed.sampling(seedbase = myListOfPlaces$villageName,
                 N = myListOfPlaces$population,
                 n = myListOfPlaces$requiredSample)

# Manual verification of the samples generated for Melakkal village
#   (for which the automatically generated seed was 1331891848)
set.seed(1331891848)
sample(120, 30)

# What about using the function on a single input?
stringseed.sampling("Santa Barbara", 1920, 100)
\dontshow{rm(myListOfPlaces)}
}
\author{
  Ananda Mahto
}
\references{
  See: \url{http://stackoverflow.com/q/10910698/1270695}
}
\seealso{
  \code{\link[digest:digest]{digest}}
}

