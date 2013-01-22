## @knitr stratified



#'%% ~~function to do ... ~~
#'
#'%% ~~ A concise (1-5 lines) description of what the function does. ~~
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param df %% ~~Describe \code{df} here~~
#'@param group %% ~~Describe \code{group} here~~
#'@param size %% ~~Describe \code{size} here~~
#'@param seed %% ~~Describe \code{seed} here~~
#'@param \dots %% ~~Describe \code{\dots} here~~
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
#'function (df, group, size, seed = NULL, ...) 
#'{
#'    df.interaction <- interaction(df[group])
#'    df.table <- table(df.interaction)
#'    df.split <- split(df, df.interaction)
#'    if (length(size) > 1) {
#'        if (length(size) != length(df.split)) 
#'            stop("Number of groups is ", length(df.split), " but number of sizes supplied is ", 
#'                length(size))
#'        if (is.null(names(size))) {
#'            n <- setNames(size, names(df.split))
#'            message(sQuote("size"), " vector entered as:\n\nsize = structure(c(", 
#'                paste(n, collapse = ", "), "),\n.Names = c(", 
#'                paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
#'        }
#'        else {
#'            ifelse(all(names(size) %in% names(df.split)), n <- size[names(df.split)], 
#'                stop("Named vector supplied with names ", paste(names(size), 
#'                  collapse = ", "), "\n but the names for the group levels are ", 
#'                  paste(names(df.split), collapse = ", ")))
#'        }
#'    }
#'    else if (size < 1) {
#'        n <- round(df.table * size, digits = 0)
#'    }
#'    else if (size >= 1) {
#'        if (all(df.table >= size)) {
#'            n <- setNames(rep(size, length.out = length(df.split)), 
#'                names(df.split))
#'        }
#'        else {
#'            message("Some groups\n---", paste(names(df.table[df.table < 
#'                size]), collapse = ", "), "---\ncontain fewer observations", 
#'                " than desired number of samples.\n", "All observations have been returned from those groups.")
#'            n <- c(sapply(df.table[df.table >= size], function(x) x = size), 
#'                df.table[df.table < size])
#'        }
#'    }
#'    seedme <- ifelse(is.null(seed), "No", "Yes")
#'    temp <- switch(seedme, No = {
#'        temp <- lapply(names(df.split), function(x) df.split[[x]][sample(df.table[x], 
#'            n[x], ...), ])
#'    }, Yes = {
#'        temp <- lapply(names(df.split), function(x) {
#'            set.seed(seed)
#'            df.split[[x]][sample(df.table[x], n[x], ...), ]
#'        })
#'    })
#'    rm(.Random.seed, envir = .GlobalEnv)
#'    do.call("rbind", temp)
#'  }
#'
stratified <- function(df, group, size, seed = NULL, ...) {
  # Returns a stratified random subset of a data.frame.
  #
  # --> df      The source data.frame
  # --> group   Your grouping variable
  # --> size    The desired sample size. If -size- is a decimal, 
  #             a proportionate sample is drawn. If it is >= 1, 
  #             a sample will be taken of that specified size
  # --> seed    The seed that you want to use, if any
  # --> ...     Further arguments to the sample function
  #
  # === EXAMPLES ===
  #
  #   set.seed(1)
  #   dat = data.frame(A = 1:100, 
  #                    B = sample(c("AA", "BB", "CC", "DD", "EE"), 
  #                               100, replace=T),
  #                    C = rnorm(100), D = abs(round(rnorm(100), digits=1)),
  #                    E = sample(c("CA", "NY", "TX"), 100, replace=T))
  #     
  #   stratified(dat, 5, .1, 1)
  #   stratified(dat, group = "E", size = .1, seed = 1)
  #   stratified(dat, "B", 5)
  
  df.interaction <- interaction(df[group])
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(", 
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)), 
             n <- size[names(df.split)], 
             stop("Named vector supplied with names ", 
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ", 
                  paste(names(df.split), collapse = ", ")))
    } 
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---", 
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  
  seedme <- ifelse(is.null(seed), "No", "Yes")
  
  temp <- switch(
    seedme,
    No = { temp <- lapply(
      names(df.split), 
      function(x) df.split[[x]][sample(df.table[x], 
                                       n[x], ...), ]) },
    Yes = { temp <- lapply(
      names(df.split),
      function(x) { set.seed(seed)
                    df.split[[x]][sample(df.table[x], 
                                         n[x], ...), ] }) })
  
  rm(.Random.seed, envir=.GlobalEnv) # "resets" the seed
  do.call("rbind", temp)
}
