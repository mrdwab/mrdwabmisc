#'Sort a \code{data.frame} by rows or columns
#'
#'The \code{\link{df.sorter}} function allows you to sort a \code{\link{data.frame}} by columns or rows or both. You can also quickly subset data columns by using the \code{var.order} argument.
#'
#'@param data The source \code{data.frame}.
#'@param var.order The new order in which you want the variables to appear. See Details
#'@param col.sort The columns \emph{within} which there is data that need to be sorted. See Details.
#'@param at.start Should the pattern matching be from the start of the variable name? Defaults to \code{TRUE}.
#'
#'@details \emph{var.order}
#'\itemize{
#'\item Defaults to \code{names(data)}, which keeps the variables in the original order.
#'\item Variables can be referred to either by a vector of their index numbers or by a vector of the variable name; partial name matching also works, but requires that the partial match identifies similar columns uniquely (see Examples).
#'Basic subsetting can also be done using \code{var.order} simply by omitting the variables you want to drop.
#'}
#'\emph{col.sort}
#'\itemize{
#'\item Defaults to \code{NULL}, which means no sorting takes place.
#'Variables can be referred to either by a vector of their index numbers or by a vector of the variable names; full names must be provided.
#'}
#'
#'@note If you are sorting both by variables and within the columns and using numeric indexes as opposed to variable names, the \code{col.sort} order should be based on the location of the columns in the new \code{data.frame}, not the original \code{data.frame}.
#'@author Ananda Mahto
#'@references %% ~put references to the literature/web site here ~
#'@examples
#'
#'# Make up some data
#'set.seed(1)
#'dat = data.frame(id = rep(1:5, each=3), times = rep(1:3, 5),
#'                  measure1 = rnorm(15), score1 = sample(300, 15), 
#'                  code1 = replicate(15, paste(sample(LETTERS[1:5], 3), 
#'                                              sep="", collapse="")),
#'                  measure2 = rnorm(15), score2 = sample(150:300, 15),
#'                  code2 = replicate(15, paste(sample(LETTERS[1:5], 3), 
#'                                              sep="", collapse="")))
#'# Preview your data
#'dat
#'
#'# Change the variable order, grouping related columns
#'# Note that you do not need to specify full variable names,
#'#    just enough that the variables can be uniquely identified
#'head(df.sorter(dat, var.order = c("id", "ti", "cod", "mea", "sco")))
#'
#'# As above, but sorted by 'times' and then 'id'
#'head(df.sorter(dat, 
#'                var.order = c("id", "tim", "cod", "mea", "sco"), 
#'                col.sort = c(2, 1)))
#'                
#'# Drop 'measure1' and 'measure2', sort by 'times', and 'score1'
#'head(df.sorter(dat, 
#'                var.order = c("id", "tim", "sco", "cod"), 
#'                col.sort = c(2, 3)))
#'
#'# Just sort by columns, first by 'times' then by 'id'
#'head(df.sorter(dat, col.sort = c("times", "id")))
#'
#'# Pattern matching anywhere in the variable name
#'head(df.sorter(dat, var.order= "co", at.start=FALSE))
#'
df.sorter <- function(data, var.order=names(data), 
                      col.sort=NULL, at.start=TRUE ) {
  # Sorts a data.frame by columns or rows or both. Can also subset the 
  #   data columns by using --var.order--. Can refer to variables either 
  #   by names or number. If referring to variable by number, and sorting
  #   both the order of variables and the sorting within variables, 
  #   refer to the variable numbers of the final data.frame.
  #
  # === EXAMPLES ===
  #
  #    library(foreign)
  #    temp = "http://www.ats.ucla.edu/stat/stata/modules/kidshtwt.dta"
  #    kidshtwt = read.dta(temp); rm(temp)
  #    df.sorter(kidshtwt, var.order = c("fam", "bir", "wt", "ht"))
  #    df.sorter(kidshtwt, var.order = c("fam", "bir", "wt", "ht"),
  #              col.sort = c("birth", "famid")) # USE FULL NAMES HERE
  #    df.sorter(kidshtwt, var.order = c(1:4),   # DROP THE WT COLUMNS
  #              col.sort = 3)                   # SORT BY HT1  

  if (is.numeric(var.order)) 
    var.order = colnames(data)[var.order]
  else var.order = var.order
  
  if (isTRUE(at.start)) {
    x = unlist(lapply(var.order, function(x) 
      sort(grep(paste("^", x, sep="", collapse=""), 
                names(data), value = TRUE))))
  } else if (!isTRUE(at.start)) {
    x = unlist(lapply(var.order, function(x) 
      sort(grep(x, names(data), value = TRUE))))
  }
  
  y = data[, x]
  
  if (is.null(col.sort)) {
    y
  } else if (is.numeric(col.sort)) {
    y[do.call(order, y[colnames(y)[col.sort]]), ]
  } else if (!is.numeric(col.sort)) {
    y[do.call(order, y[col.sort]), ]
  }
}
