## @knitr dfsorter
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
