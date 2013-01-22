## @knitr rowextractor


#'%% ~~function to do ... ~~
#'
#'%% ~~ A concise (1-5 lines) description of what the function does. ~~
#'
#'%% ~~ If necessary, more details than the description above ~~
#'
#'@param data %% ~~Describe \code{data} here~~
#'@param extract.by %% ~~Describe \code{extract.by} here~~
#'@param what %% ~~Describe \code{what} here~~
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
#'function (data, extract.by, what = "all") 
#'{
#'    if (is.numeric(extract.by)) {
#'        extract.by = extract.by
#'    }
#'    else if (is.numeric(extract.by) != 0) {
#'        extract.by = which(colnames(data) %in% "extract.by")
#'    }
#'    if (is.character(what)) {
#'        which.median = function(data, extract.by) {
#'            a = data[, extract.by]
#'            if (length(a)%%2 != 0) {
#'                which(a == median(a))
#'            }
#'            else if (length(a)%%2 == 0) {
#'                b = sort(a)[c(length(a)/2, length(a)/2 + 1)]
#'                c(max(which(a == b[1])), min(which(a == b[2])))
#'            }
#'        }
#'        X1 = data[which(data[extract.by] == min(data[extract.by])), 
#'            ]
#'        X2 = data[which(data[extract.by] == max(data[extract.by])), 
#'            ]
#'        X3 = data[which.median(data, extract.by), ]
#'        if (identical(what, "min")) {
#'            X1
#'        }
#'        else if (identical(what, "max")) {
#'            X2
#'        }
#'        else if (identical(what, "median")) {
#'            X3
#'        }
#'        else if (identical(what, "all")) {
#'            rbind(X1, X3, X2)
#'        }
#'    }
#'    else if (is.numeric(what)) {
#'        which.quantile <- function(data, extract.by, what, na.rm = FALSE) {
#'            x = data[, extract.by]
#'            if (!na.rm & any(is.na(x))) 
#'                return(rep(NA_integer_, length(what)))
#'            o <- order(x)
#'            n <- sum(!is.na(x))
#'            o <- o[seq_len(n)]
#'            nppm <- n * what - 0.5
#'            j <- floor(nppm)
#'            h <- ifelse((nppm == j) & ((j%%2L) == 0L), 0, 1)
#'            j <- j + h
#'            j[j == 0] <- 1
#'            o[j]
#'        }
#'        data[which.quantile(data, extract.by, what), ]
#'    }
#'  }
#'
row.extractor = function(data, extract.by, what="all") {
  # Extracts rows with min, median, and max values, or by quantiles. Values 
  #   for --what-- can be "min", "median", "max", "all", or a vector 
  #   specifying the desired quantiles. Values for --extract.by-- can be 
  #   the variable name or number.
  #
  # === EXAMPLES ===
  #
  #    set.seed(1)
  #    dat = data.frame(V1 = 1:10, V2 = rnorm(10), V3 = rnorm(10), 
  #                     V4 = sample(1:20, 10, replace=T))
  #    dat2 = dat[-10,]
  #    row.extractor(dat, 4, "all")
  #    row.extractor(dat1, 4, "min")
  #    row.extractor(dat, "V4", "median")
  #    row.extractor(dat, 4, c(0, .5, 1))
  #    row.extractor(dat, "V4", c(0, .25, .5, .75, 1))
  #
  # "which.quantile" function by cbeleites:
  # http://stackoverflow.com/users/755257/cbeleites
  # See: http://stackoverflow.com/q/10256503/1270695
  
  if (is.numeric(extract.by)) {
    extract.by = extract.by
  } else if (is.numeric(extract.by) != 0) {
    extract.by = which(colnames(data) %in% "extract.by")
  } 
  
  if (is.character(what)) {
    which.median = function(data, extract.by) {
      a = data[, extract.by]
      if (length(a) %% 2 != 0) {
        which(a == median(a))
      } else if (length(a) %% 2 == 0) {
        b = sort(a)[c(length(a)/2, length(a)/2+1)]
        c(max(which(a == b[1])), min(which(a == b[2])))
      }
    }
    
    X1 = data[which(data[extract.by] == min(data[extract.by])), ] # min
    X2 = data[which(data[extract.by] == max(data[extract.by])), ] # max
    X3 = data[which.median(data, extract.by), ]                # median
    
    if (identical(what, "min")) {
      X1
    } else if (identical(what, "max")) {
      X2
    } else if (identical(what, "median")) {
      X3
    } else if (identical(what, "all")) {
      rbind(X1, X3, X2)
    }
  } else if (is.numeric(what)) {
    which.quantile <- function (data, extract.by, what, na.rm = FALSE) {
      
      x = data[ , extract.by]
      
      if (! na.rm & any (is.na (x)))
        return (rep (NA_integer_, length (what)))
      
      o <- order (x)
      n <- sum (! is.na (x))
      o <- o [seq_len (n)]
      
      nppm <- n * what - 0.5
      j <- floor(nppm)
      h <- ifelse((nppm == j) & ((j%%2L) == 0L), 0, 1)
      j <- j + h
      
      j [j == 0] <- 1
      o[j]
    }
    data[which.quantile(data, extract.by, what), ]           # quantile
  }
}
