## @knitr randomnames

RandomNames <- function(N = 100, cat = NULL, gender = NULL, 
                        MFprob = NULL, dataset = NULL) {
  # Generates a "data.frame" of random names with the following columns:
  #   "Gender", "FirstName", and "Surname". All arguments have preset
  #   defaults, so the function can be run simply by typing RandomNames(),
  #   which will generate 100 random male and female names.
  #
  # === EXAMPLES ===
  #
  #     RandomNames()
  #     RandomNames(N = 20)
  #     RandomNames(cat = "common", MFprob = c(.2, .8))
  #
  # See: 
  #   - http://www.census.gov/genealogy/www/data/1990surnames/names_files.html
  #   - http://random-name-generator.info/
  
  if (is.null(dataset)) {
    if (!exists("CensusNames1990", where = 1)) {
      if (isTRUE(list.files(
        pattern = "^CensusNames.RData$") == "CensusNames.RData")) {
        load("CensusNames.RData")
      } else {
        ans = readline("
    CensusNames.RData dataset not found in working directory.
    CensusNames1990 object not found in workspace. \n
    Download and load the dataset now? (y/n) ")
        if (ans != "y")
          return(invisible())
        require(RCurl)
        baseURL = c("https://raw.github.com/mrdwab/2657-R-Functions/master/")
        temp = getBinaryURL(paste0(baseURL, "data/CensusNames.RData"))
        load(rawConnection(temp), envir=.GlobalEnv)
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
