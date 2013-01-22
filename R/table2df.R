## @knitr table2df

table2df <- function(mytable, as.multitable = FALSE, direction = "wide") {
  # Converts a table to a data.frame. If the table is an array of tables
  #   you can use >>as.multitable<< to create a list of data.frames.
  #   Direction can be "wide" or "long". If "long", result will be a 
  #   data.frame with the default column variables and a single column
  #   for the frequency.
  #
  # See: http://stackoverflow.com/a/6463137/1270695
  
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
