#' Convert \code{table} Objects to \code{data.frame}s
#' 
#' The \code{\link{table2df}} function takes an object of "class"
#' \code{\link{table}}, \code{\link{ftable}}, and \code{\link{xtabs}} and
#' converts them to \code{\link{data.frame}}s, while retaining as many of the
#' name details as possible.
#' 
#' 
#' @param mytable The \code{table} object you want to convert into a
#' \code{data.frame}. This can be an object in your workspace, or you can make
#' the call to \code{table}, \code{ftable}, or \code{xtabs} as the
#' \code{mytable} argument to this function.
#' @param as.multitable Logical; defaults to \code{FALSE}.  Some methods, for
#' instance \code{xtabs} and \code{table}, will create an array of tables as
#' the output when more than two variables are being tabulated. \itemize{ \item
#' If \code{as.multitable} is \code{TRUE}, the function will return a list of
#' \code{data.frame}s. \item If \code{as.multitable} is \code{FALSE}, the
#' function will convert the object to an \code{ftable} object before
#' performing the transformation. }
#' @param direction Can be either \code{"long"} or \code{"wide"}. \itemize{
#' \item If \code{"long"}, the frequencies will all be tabulated into a single
#' column.  This is the same behavior you will generally get if you used
#' \code{as.data.frame} on a \code{table} object. \item If "wide", the tabular
#' format is retained. }
#' @author Ananda Mahto
#' @seealso \code{\link{table}}, \code{\link{ftable}}, \code{\link{xtabs}}
#' @references The \code{expand.grid} method for remaking the columns from an
#' \code{ftable} was described by Kohske at
#' \url{http://stackoverflow.com/a/6463137/1270695}.
#' @examples
#' 
#' # Make up some data:
#' set.seed(1)
#' handedness <- data.frame(
#' gender = sample(c("Female", "Male", "Unknown"), 200, replace = TRUE),
#' handedness = sample(c("Right", "Left", "Ambidextrous"),
#'                  200, replace = TRUE, prob = c(.7, .2, .1)),
#' fav.col = sample(c("Red", "Orange", "Yellow", "Green", "Blue",
#'               "Indigo", "Violet", "Black", "White"),
#'               200, replace = TRUE),
#' fav.shape = sample(c("Triangle", "Circle", "Square", "Pentagon", "Hexagon",
#'                 "Oval", "Octagon", "Rhombus", "Trapezoid"),
#'                 200, replace = TRUE),
#' computer = sample(c("Win", "Mac", "Lin"), 200, replace = TRUE,
#'                prob = c(.5, .25, .25)))
#' # Preview the data
#' list(head(handedness), tail(handedness))
#' 
#' # A very basic table
#' HT1 <- with(handedness, table(gender, handedness))
#' HT1
#' table2df(HT1)
#' #'table2df(HT1, direction = "long")
#' 
#' # Another basic table
#' HT2 <- with(handedness, table(fav.col, computer))
#' HT2
#' table2df(HT2)
#' 
#' # This will create multiple tables, one for each possible computer value
#' HT3 <- with(handedness, table(gender, fav.col, computer))
#' HT3
#' 
#' # Default settings
#' table2df(HT3)
#' 
#' # As a list of data.frames
#' table2df(HT3, as.multitable = TRUE)
#' 
#' # As above, but with the output in long format
#' #   Only showing the first three lines of each data.frame
#' lapply(table2df(HT3, as.multitable = TRUE, direction = "long"), head, 3)
#' 
#' # Applied to an ftable
#' HT4 <- ftable(handedness,
#'          col.vars="fav.col",
#'          row.vars=c("gender", "computer"))
#' HT4
#' table2df(HT4)
#' 
#' # Applied to a single-row table
#' table2df(xtabs(breaks ~ wool, warpbreaks))
#' 
#' ## ======================================= ##
#' ## ========== OTHER EXAMPLES ============= ##
#' 
#' \dontrun{
#' table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph))
#' table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph),
#'     direction = "long")
#' table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph),
#'     as.multitable = TRUE, direction = "long")
#' table2df(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph),
#'     as.multitable = TRUE, direction = "wide")
#' }
#' 
#' \dontshow{rm(HT1, HT2, HT3, HT4, handedness)}
#' 
#' @export table2df
table2df <- function(mytable, as.multitable = FALSE, direction = "wide") {
  
  ## Check to see if as.multitable is being incorrectly used
  ##   but continue to process output with a warning
  if (isTRUE(as.multitable)) {
    multitablecheck <- ifelse(length(dim(mytable)) == 2, "NoMT", "MT")
    if (isTRUE(as.multitable) && multitablecheck == "NoMT")
      warning(">>as.multitable<< set to TRUE, but evaluates to FALSE.
              Defaulting to basic table to data.frame conversion methods.")
    switch(multitablecheck, NoMT = { as.multitable <- FALSE },
           MT = { as.multitable <- TRUE })
  }
  
  # Deal swiftly if it is a single row table
  if (length(dim(mytable)) == 1) {
    out <- setNames(as.data.frame(t(data.frame(mytable)[-1])), 
             data.frame(mytable)[, 1])
    out$Var1 <- paste(row.names(out), 
                      names(attr(mytable, "dimnames")), 
                      sep = ".")
    row.names(out) <- NULL
    out[, c("Var1", setdiff(names(out), "Var1"))]
  } else {
    tablearray2list <- function(dataset) {
      # Converts an array of tables to a list of tables
      y <- ls()
      temp <- dim(dataset)[-c(1, 2)]
      temp1 <- capture.output(dataset)
      tempnames <- gsub(", , ", "", temp1[grep(", , ", temp1)])
      tempnames <- sapply(
        strsplit(tempnames, " = |, "), 
        function(x) paste(x[1:length(x) %% 2 == 0], collapse = "."))
      combinations <- expand.grid(lapply(temp, seq))
      tempsets <- apply(combinations, 1, function(x)
        paste(y, "[ , , ", paste(x, collapse = ", "), "]"))
      mylist <- lapply(tempsets, function(x) eval(parse(text = x)))
      names(mylist) <- tempnames
      mylist
    }
    
    directionwide <- function(mydata) {
      # Function to assign proper names to resulting data.frames
      ifelse(class(mydata) == "ftable", 
             mydata <- mydata, mydata <- ftable(mydata))
      dfrows <- expand.grid(rev(attr(mydata, "row.vars")))
      dfcols <- as.data.frame.matrix(mydata)
      names(dfcols) <- interaction(expand.grid(attr(mydata, "col.vars")))
      cbind(dfrows, dfcols)
    }
    
    directionlong <- function(mydata) {
      # Basic tables are easy to convert to data.frames
      mydata <- as.table(mydata)
      as.data.frame(mydata)
    }
    
    if (isTRUE(as.multitable)) {
      temp <- tablearray2list(mytable)
      switch(direction,
             wide = { lapply(temp, directionwide) },
             long = { lapply(temp, directionlong) },
             stop(">>direction<< must be either wide or long"))
    } else {
      switch(direction,
             wide = { directionwide(mytable) },
             long = { directionlong(mytable) },
             stop(">>direction<< must be either wide or long"))
    }
  }
}
