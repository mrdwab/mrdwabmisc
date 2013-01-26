#'Extract min/median/max/quantile rows from a \code{data.frame}
#'
#'The \code{\link{row.extractor}} function takes a \code{data.frame} and extracts rows with the min, median, or max values of a given variable, or extracts rows with specific quantiles of a given variable.
#'
#'@param data The source \code{data.frame}.
#'@param extract.by The column which will be used as the reference for extraction; can be specified either by the column number or the variable name.
#'@param what Options are \code{"min"} (for all rows matching the minimum value), \code{"median"} (for the median row or rows), \code{"max"} (for all rows matching the maximum value), or \code{"all"} (for min, median, and max); alternatively, a numeric vector can be specified with the desired quantiles, for instance \code{c(0, .25, .5, .75, 1)}.
#'@author Ananda Mahto
#'@seealso \code{\link{min}}, \code{\link{max}}, \code{\link{median}}, \code{\link{which.min}}, \code{\link{which.max}}, \code{\link{quantile}}
#'@references \itemize{
#'\item \code{which.quantile} function by cbeleites: \url{http://stackoverflow.com/users/755257/cbeleites}
#'\item See: \url{http://stackoverflow.com/q/10256503/1270695}
#'}
#'@examples
#'
#'# Make up some data
#'set.seed(1)
#'dat = data.frame(V1 = 1:50, V2 = rnorm(50), 
#'                 V3 = round(abs(rnorm(50)), digits=2), 
#'                 V4 = sample(1:30, 50, replace=TRUE))
#'# Get a sumary of the data
#'summary(dat)
#'
#'# Get the rows corresponding to the 'min', 'median', and 'max' of 'V4'
#'row.extractor(dat, 4) 
#'
#'# Get the 'min' rows only, referenced by the variable name
#'row.extractor(dat, "V4", "min") 
#'
#'# Get the 'median' rows only. Notice that there are two rows 
#'#    since we have an even number of cases and true median 
#'#    is the mean of the two central sorted values
#'row.extractor(dat, "V4", "median") 
#'
#'# Get the rows corresponding to the deciles of 'V3'
#'row.extractor(dat, "V3", seq(0.1, 1, 0.1)) 
#'
row.extractor = function(data, extract.by, what="all") {
  # 
  # 
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
