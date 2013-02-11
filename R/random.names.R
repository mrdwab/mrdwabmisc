#'Generate random names
#'
#'The \code{\link{RandomNames}} function uses data from the \emph{Genealogy Data: Frequently Occurring Surnames from Census 1990--Names Files} web page to generate a \code{\link{data.frame}} with random names.
#'
#'@param N The number of random names you want. Defaults to 100.
#'@param cat Do you want "common" names, "rare" names, names with an "average" frequency, or some combination of these? Should be specified as a character vector (for example, \code{c("rare", "common")}). Defaults to \code{NULL}, in which case all names are used as the sample frame.
#'@param gender Do you want first names from the "male" dataset, the "female" dataset, or from all available names? Should be specified as a quoted string (for example, \code{"male"}). Defaults to \code{NULL}, in which case all available first names are used as the sample frame.
#'@param MFprob What proportion of the sample should be male names and what proportion should be female? Specify as a numeric vector that sums to 1 (for example, \code{c(.6, .4)}). The first number represents the probability of sampling a "male" first name, and the second number represents the probability of sampling a "female" name. This argument is not used if only one gender has been specified in the previous argument. Defaults to \code{NULL}, in which case, the probability used is \code{c(.5, .5)}.
#'@param dataset What do you want to use as the dataset of names from which to sample? A default dataset is provided that can generate over 400 million unique names. See the "Dataset Details" note for more information.
#'
#'@note \emph{Dataset Details}
#'This function samples from a provided dataset of names. By default, it uses the data from the \emph{Genealogy Data: Frequently Occurring Surnames from Census 1990--Names Files} web page. Those data have been converted to \code{list} named \code{"CensusNames1990"} containing three \code{data.frame}s (named \code{"surnames"}, \code{"malenames"}, and \code{"femalenames"}). 
#'
#'Alternatively, you may provide your own data in a \code{list} formatted according to the following specifications (see the \code{"myCustomNames"} data in the "Examples*" section). \emph{Please remember that R is case sensitive!}
#'
#'\itemize{
#'\item This must be a named list with three items: \code{"surnames"}, \code{"malenames"}, and \code{"femalenames"}.
#'\item The contents of each list item is a \code{data.frame} with at least the following named columns: \code{"Name"} and \code{"Category"}.
#'\item Acceptable values for \code{"Category"} are \code{"common"}, \code{"rare"}, and \code{"average"}.
#'}
#'
#'@author Ananda Mahto
#'@references \itemize{
#'\item See \url{http://www.census.gov/genealogy/www/data/1990surnames/names_files.html} for source of data.
#'\item Inspired by the online Random Name Generator \url{http://random-name-generator.info/}
#'}
#'
#'@examples
#'
#'# Generate 20 random names
#'RandomNames(N = 20)
#'
#'# Generate a reproducible list of 100 random names with approximately 
#'#   80% of the names being female names, and 20% being male names.
#'set.seed(1)
#'temp <- RandomNames(cat = "common", MFprob = c(.2, .8))
#'list(head(temp), tail(temp))
#'table(temp$Gender)
#'
#'# Cleanup
#'rm(.Random.seed, envir=globalenv()) # Resets your seed
#'rm(temp)
#'
#'# Generate 10 names from the common and rare categories of names
#'RandomNames(N = 10, cat = c("common", "rare"))
#'
#'## ===================================== ##
#'## ======== USING YOUR OWN DATA ======== ##
#'
#'myCustomNames <- list(
#'  surnames = data.frame(
#'    Name = LETTERS[1:26], 
#'    Category = c(rep("rare", 10), rep("average", 10), rep("common", 6))),
#'  malenames = data.frame(
#'    Name = letters[1:10], 
#'    Category = c(rep("rare", 4), rep("average", 4), rep("common", 2))),
#'  femalenames = data.frame(
#'    Name = letters[11:26],
#'    Category = c(rep("rare", 8), rep("average", 4), rep("common", 4))))
#'str(myCustomNames)
#'
#'RandomNames(N = 15, dataset = myCustomNames)
#'\dontshow{rm(CensusNames1990, myCustomNames)}
#'
RandomNames <- function(N = 100, cat = NULL, gender = NULL, 
                        MFprob = NULL, dataset = NULL) {
  if (is.null(dataset)) {
    data(CensusNames)
    dataset <- CensusNames1990
  }
  TEMP <- dataset
  possiblecats <- c("common", "rare", "average")
  if(all(cat %in% possiblecats) == FALSE) 
    stop('cat must be either "all", NULL,
         or a combination of "common", "average", or "rare"')
  possiblegenders <- c("male", "female", "both")
  if (all(gender %in% possiblegenders) == FALSE) {
    stop('gender must be either "both", NULL, "male", or "female"')
  }
  if (isTRUE(identical(gender, c("male", "female"))) || 
        isTRUE(identical(gender, c("female", "male")))) {
    gender <- "both"
  }
  if (is.null(cat) || cat == "all") {
    surnames <- TEMP[["surnames"]][["Name"]]
    malenames <- paste("M-", TEMP[["malenames"]][["Name"]], sep="")
    femalenames <- paste("F-", TEMP[["femalenames"]][["Name"]], sep="")
  } else {
    surnames <- suppressWarnings(
      with(TEMP[["surnames"]], 
           TEMP[["surnames"]][Category == cat, "Name"]))
    malenames <- paste("M-", suppressWarnings(
      with(TEMP[["malenames"]], 
           TEMP[["malenames"]][Category == cat, "Name"])), sep="")
    femalenames <- paste("F-", suppressWarnings(
      with(TEMP[["femalenames"]], 
           TEMP[["femalenames"]][Category == cat, "Name"])), sep="")
  }
  
  if (is.null(gender) || gender == "both") {
    if (is.null(MFprob)) MFprob <- c(.5, .5)
    firstnames <- sample(c(malenames, femalenames), N, replace = TRUE,
                         prob = c(rep(MFprob[1]/length(malenames), 
                                      length(malenames)),
                                  rep(MFprob[2]/length(femalenames), 
                                      length(femalenames))))
  } else if (gender == "female") {
    firstnames <- sample(femalenames, N, replace = TRUE)
  } else if (gender == "male") {
    firstnames <- sample(malenames, N, replace = TRUE)
  }
  
  Surnames <- sample(surnames, N, replace = TRUE)
  temp <- setNames(data.frame(do.call(rbind, strsplit(firstnames, "-"))),
                   c("Gender", "FirstName"))
  cbind(temp, Surnames)
}
