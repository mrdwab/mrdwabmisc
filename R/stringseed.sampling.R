## @knitr stringseedsampling



#'%% ~~function to do ... ~~
#'
#'%% ~~ A concise (1-5 lines) description of what the function does. ~~
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param seedbase %% ~~Describe \code{seedbase} here~~
#'@param N %% ~~Describe \code{N} here~~
#'@param n %% ~~Describe \code{n} here~~
#'@param write.output %% ~~Describe \code{write.output} here~~
#'@return %% ~Describe the value returned %% If it is a LIST, use %%
#'\item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#''comp2'} %% ...
#'@note %% ~~further notes~~
#'@author %% ~~who you are~~
#'@seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#'@references %% ~put references to the literature/web site here ~
#'@keywords ~kwd1 ~kwd2
#'@examples
#'
#'##---- Should be DIRECTLY executable !! ----
#'##-- ==>  Define data, use random,
#'##--	or do  help(data=index)  for the standard data sets.
#'
#'## The function is currently defined as
#'function (seedbase, N, n, write.output = FALSE) 
#'{
#'    require(digest)
#'    hexval = paste0("0x", sapply(seedbase, digest, "crc32"))
#'    seeds = type.convert(hexval)%%.Machine$integer.max
#'    seedbase = as.character(seedbase)
#'    temp <- data.frame(seedbase, N, n, seeds)
#'    if (length(seedbase) == 1) {
#'        set.seed(temp$seeds)
#'        sample.list <- sample(temp$N, temp$n)
#'    }
#'    else {
#'        sample.list <- setNames(apply(temp[-1], 1, function(x) {
#'            set.seed(x[3])
#'            sample(x[1], x[2])
#'        }), temp[, 1])
#'    }
#'    temp <- list(input = data.frame(seedbase = seedbase, populations = N, 
#'        samplesizes = n, seeds = seeds), samples = sample.list)
#'    if (isTRUE(write.output)) {
#'        write.csv(temp[[1]], file = paste("Sample frame generated on", 
#'            Sys.Date(), ".csv", collapse = ""))
#'        capture.output(temp[[2]], file = paste("Samples generated on", 
#'            Sys.Date(), ".txt", collapse = ""))
#'    }
#'    rm(.Random.seed, envir = globalenv())
#'    temp
#'  }
#'
stringseed.sampling <- function(seedbase, N, n, write.output = FALSE) {
  # Designed for batch sampling scenarios using alpha-numeric strings as a 
  #   --seedbase--. --N-- represents the "population", and --n--, the sample
  #   size needed. A vector is supplied for each argument (or, alternatively, 
  #   a data.frame with the required information). Optionally, the function 
  #   can write the output of the function to a file.
  #
  # === EXAMPLE ===
  #
  #   stringseed.sampling(seedbase = c("Village 1", "Village 2", "Village 3"),
  #                       N = c(150, 309, 297), n = c(15, 31, 30))
  #
  # See: http://stackoverflow.com/q/10910698/1270695
  
  require(digest)
  hexval = paste0("0x", sapply(seedbase, digest, "crc32"))
  seeds = type.convert(hexval) %% .Machine$integer.max
  seedbase = as.character(seedbase)
  
  temp <- data.frame(seedbase, N, n, seeds)
  if (length(seedbase) == 1) {
    set.seed(temp$seeds); sample.list <- sample(temp$N, temp$n)
  } else {
    sample.list <- setNames(
      apply(temp[-1], 1, function(x) 
        {set.seed(x[3]); sample(x[1], x[2])} ), temp[, 1])
  }
  
  temp <- list(
    input = data.frame(seedbase = seedbase, populations = N,
                       samplesizes = n, seeds = seeds),
    samples = sample.list)
  if(isTRUE(write.output)) {
    write.csv(temp[[1]], file=paste("Sample frame generated on",
                                    Sys.Date(), ".csv", collapse=""))
    capture.output(temp[[2]], file=paste("Samples generated on", 
                              Sys.Date(), ".txt", collapse=""))
  }
  rm(.Random.seed, envir=globalenv()) # "resets" the seed
  temp
}
