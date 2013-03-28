#'Split concatenated cells in a \code{data.frame}
#'
#'The concat.split function takes a column with multiple values, splits the
#'values into a list or into separate columns, and returns a new data.frame.
#'
#'\emph{structure} \itemize{ \item \code{"compact"} creates as many columns as
#'the maximum length of the resulting split. This is the most useful
#'general-case application of this function. \item When the input is numeric,
#'\code{"expanded"} creates as many columns as the maximum value of the input
#'data. This is most useful when converting to \code{mode = "binary"}. \item
#'\code{"list"} creates a single new column that is structurally a
#'\code{\link{list}} within a \code{\link{data.frame}}. } \emph{fixed} When
#'\code{structure = "expanded"} or \code{structure = "list"}, it is possible to
#'supply a a regular expression containing the characters to split on.  For
#'example, to split on \code{","}, \code{";"}, or \code{"|"}, you can set
#'\code{sep = ",|;|\|"} or \code{sep = "[,;|]"}, and \code{fixed = FALSE} to
#'split on any of those characters.
#'
#'@param data The source data.frame
#'@param split.col The variable that needs to be split; can be specified either
#'by the column number or the variable name.
#'@param sep The character separating each value (defaults to ",").
#'@param structure Can be either "compact", "expanded", or "list". Defaults to
#'"compact". See Details.
#'@param mode Can be either binary or value (where binary is default and it
#'recodes values to 1 or NA, like Boolean data, but without assuming 0 when
#'data is not available).  This setting only applies when structure =
#'"expanded"; an warning message will be issued if used with other structures.
#'@param drop.col Logical (whether to remove the original variable from the
#'output or not). Defaults to TRUE.
#'@param fixed Is the input for the \code{sep} value \emph{fixed}, or a
#'\emph{regular expression}? See Details.
#'@note If using \code{structure = "compact"}, the value for \code{sep} can
#'only be a single character. See the "Advanced Usage" example of how to
#'specify multiple characters for batch conversion of columns.
#'@author Ananda Mahto
#'@references \itemize{ \item See
#'\url{http://stackoverflow.com/q/10100887/1270695} \item The
#'\code{"condensed"} setting was inspired by an answer from David Winsemius to
#'a question at Stack Overflow.  See:
#'\url{http://stackoverflow.com/a/13924245/1270695} }
#'@examples
#'
#'## Load some data
#'data(concatenated)
#'head(concat.test)
#'
#'# Split up the second column, selecting by column number
#'head(concat.split(concat.test, 2))
#'
#'# ... or by name, and drop the offensive first column
#'head(concat.split(concat.test, "Likes", drop.col = TRUE))
#'
#'# The "Hates" column uses a different separator
#'head(concat.split(concat.test, "Hates", sep = ";", drop.col = TRUE))
#'
#'# You'll get a warning here, when trying to retain the original values
#'head(concat.split(concat.test, 2, mode = "value", drop.col = TRUE))
#'
#'# Try again. Notice the differing number of resulting columns
#'head(concat.split(concat.test, 2, structure = "expanded",
#'    mode = "value", drop.col = TRUE))
#'
#'# Let's try splitting some strings... Same syntax
#'head(concat.split(concat.test, 3, drop.col = TRUE))
#'
#'# Split up the "Likes column" into a list variable; retain original column
#'head(concat.split(concat.test, 2, structure = "list", drop.col=FALSE))
#'
#'# View the structure of the output for the first 10 rows to verify
#'# that the new column is a list; note the difference between "Likes"
#'# and "Likes_list".
#'str(concat.split(concat.test, 2, structure = "list",
#'  drop.col=FALSE)[1:10, c(2, 5)])
#'
#'# ADVANCED USAGE ###
#'
#'# Show just the first few lines, compact structure
#'# Note that the split characters must be specified
#'#   in the same order that lapply will encounter them
#'head(do.call(cbind,
#'            c(concat.test[1],
#'              lapply(1:(ncol(concat.test)-1),
#'                     function(x) {
#'                         splitchars = c(",", ",", ";")
#'                         concat.split(concat.test[-1][x], 1,
#'                                      splitchars[x],
#'                                      drop.col=TRUE)
#'                                      }))))
#'\dontshow{rm(concat.test)}
#'
concat.split <- function(data, split.col, sep = ",", structure = "compact",
                        mode = NULL, drop.col = FALSE, fixed = FALSE) {
    
    # Check to see if split.col is specified by name or position
    if (is.numeric(split.col)) split.col = split.col
    else split.col = which(colnames(data) %in% split.col)
    
    # Split the data
    a = as.character(data[ , split.col])
    b = strsplit(a, sep, fixed = fixed)
    
    temp <- switch(
        structure, 
        compact = {
            t1 <- read.table(text = a, sep = sep, fill = TRUE,
                             row.names = NULL, header = FALSE,
                             blank.lines.skip = FALSE, 
                             strip.white = TRUE)
            names(t1) <- paste(names(data[split.col]), 
                               seq(ncol(t1)), sep="_")
            if (!is.null(mode)) 
                warning("
                        'mode' supplied but ignored. 
                        'mode' setting only applicable 
                        when structure='expanded'.")
            if (isTRUE(drop.col)) cbind(data[-split.col], t1)
            else cbind(data, t1)
        },
        list = {
            varname = paste(names(data[split.col]), "list", sep="_")
            if (suppressWarnings(is.na(try(max(as.numeric(unlist(b))))))) {
                data[varname] = list(
                    lapply(lapply(b, as.character),
                           function(x) gsub("^\\s+|\\s+$", "", x)))
            } else if (!is.na(try(max(as.numeric(unlist(b)))))) {
                data[varname] = list(lapply(b, as.numeric))
            }
            if (!is.null(mode)) 
                warning("
                        'mode' supplied but ignored. 
                        'mode' setting only applicable 
                        when structure='expanded'.")
            if (isTRUE(drop.col)) data[-split.col]
            else data
        },
        expanded = {
            if (suppressWarnings(is.na(try(max(as.numeric(unlist(b))))))) {
                what = "string"
                ncol = max(unlist(lapply(b, function(i) length(i))))
            } else if (!is.na(try(max(as.numeric(unlist(b)))))) {
                what = "numeric"
                ncol = max(as.numeric(unlist(b)))
            }
            temp1 <- switch(
                what,
                string = {
                    temp = as.data.frame(t(sapply(b, '[', 1:ncol)))
                    names(temp) = paste(names(data[split.col]),
                                        1:ncol, sep="_")
                    temp = apply(
                        temp, 2, function(x) gsub("^\\s+|\\s+$", "", x))
                    temp1 = cbind(data, temp)
                },
                numeric = {
                    temp = lapply(b, as.numeric)
                    m = matrix(nrow = nrow(data), ncol = ncol)      
                    for (i in 1:nrow(data)) {
                        m[i, temp[[i]]] = temp[[i]]
                    }
                    
                    m = setNames(data.frame(m), 
                                 paste(names(data[split.col]), 1:ncol, sep="_"))
                    
                    if (is.null(mode)) mode = "binary"
                    temp1 <- switch(
                        mode,
                        binary = {cbind(data, replace(m, m != "NA", 1))},
                        value = {cbind(data, m)},
                        stop("'mode' must be 'binary' or 'value'"))
                })
            if (isTRUE(drop.col)) temp1[-split.col]
            else temp1
        },
        stop("'structure' must be either 'compact', 'expanded', or 'list'"))
    temp[temp == ""] <- NA
    temp
}
