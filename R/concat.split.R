## @knitr concatsplit
concat.split = function(data, split.col, sep = ",", structure = "compact",
                        mode = NULL, drop.col = FALSE, fixed = FALSE) {
    # Takes a column with multiple values, splits the values into 
    #   separate columns, and returns a new data.frame.
    # --data-- is the source data.frame; --split.col-- is the variable that 
    #   needs to be split; --structure-- the type of output that should be
    #   returned, either a -compact- or -expanded- form, or a -list-
    #   (defaults to -compact-).  --mode-- can be either -binary- or -value-
    #   (where -binary- is default and it recodes values to 1 or NA); --sep--
    #   is the character separating each value (defaults to -,-). --drop.col-- 
    #   is logical (whether to remove the original variable from the output).
    #
    # === EXAMPLES ===
    #
    #     dat = data.frame(
    #         V1 = c("1, 2, 4", "3, 4, 5",  "1, 2, 5", "4", "1, 2, 3, 5"),
    #         V2 = c("1;2;3;4", "1", "2;5", "3;2", "2;3;4"))
    #     dat2 = data.frame(
    #         V1 = c("Fred, John, Sue", "Jerry, Jill", 
    #                "Sally, Ryan", "Susan, Amos, Ben"))
    #     
    #     concat.split(dat, 1) 
    #     concat.split(dat, 1, structure="expanded")
    #     concat.split(dat, 1, structure="expanded", mode = "value")
    #     concat.split(dat, 2, sep=";")
    #     concat.split(dat, "V2", sep=";", mode="value")
    #     concat.split(dat2, 1)
    #     concat.split(dat2, "V1", drop.col=TRUE)
    #     concat.split(dat2, "V1", structure="expanded", drop.col=TRUE)
    #
    # See: http://stackoverflow.com/q/10100887/1270695
    # See also: http://stackoverflow.com/a/13912721/1270695
    
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
    temp
}
