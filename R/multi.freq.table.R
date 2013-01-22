## @knitr multifreqtable
multi.freq.table <- function(data, sep = "", boolean = TRUE, factors = NULL,
                             NAto0 = TRUE, basic = FALSE, dropzero=TRUE, 
                             clean=TRUE) {
  # Takes multiple-response data and tabulates it according
  #   to the possible combinations of each variable.
  #
  # === EXAMPLES ===
  #
  #     set.seed(1)
  #     dat = data.frame(A = sample(c(0, 1), 20, replace=TRUE), 
  #                      B = sample(c(0, 1), 20, replace=TRUE), 
  #                      C = sample(c(0, 1), 20, replace=TRUE),
  #                      D = sample(c(0, 1), 20, replace=TRUE),
  #                      E = sample(c(0, 1), 20, replace=TRUE))
  #   multi.freq.table(dat)
  #   multi.freq.table(dat[1:3], sep="-", dropzero=TRUE)
  #
  # See: http://stackoverflow.com/q/11348391/1270695
  #      http://stackoverflow.com/q/11622660/1270695
  
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  if (isTRUE(boolean)) {
    CASES = nrow(data)
    RESPS = sum(data, na.rm=TRUE)
    
    if(isTRUE(NAto0)) {
      data[is.na(data)] = 0
      VALID = CASES
      VRESP = RESPS
    } else if(!isTRUE(NAto0)) {
      data = data[complete.cases(data), ]
      VALID = CASES - (CASES - nrow(data))
      VRESP = sum(data)
    }
    
    if(isTRUE(basic)) {
      counts = data.frame(Freq = colSums(data),
                          Pct.of.Resp = (colSums(data)/sum(data))*100,
                          Pct.of.Cases = (colSums(data)/nrow(data))*100)
    } else if (!isTRUE(basic)) {
      counts = data.frame(table(data))
      Z = counts[, c(intersect(names(data), names(counts)))]
      Z = rowSums(sapply(Z, as.numeric)-1)
      if(Z[1] == 0) { Z[1] = 1 }
      N = ncol(counts)
      counts$Combn = apply(counts[-N] == 1, 1, 
                           function(x) paste(names(counts[-N])[x],
                                             collapse=sep))
      counts$Weighted.Freq = Z*counts$Freq
      counts$Pct.of.Resp = (counts$Weighted.Freq/sum(data))*100
      counts$Pct.of.Cases = (counts$Freq/nrow(data))*100
      if (isTRUE(dropzero)) {
        counts = counts[counts$Freq != 0, ]
      } else if (!isTRUE(dropzero)) {
        counts = counts
      }
      if (isTRUE(clean)) {
        counts = data.frame(Combn = counts$Combn, Freq = counts$Freq, 
                            Weighted.Freq = counts$Weighted.Freq,
                            Pct.of.Resp = counts$Pct.of.Resp, 
                            Pct.of.Cases = counts$Pct.of.Cases)
      }
    }
    message("Total cases:     ", CASES, "\n",
            "Valid cases:     ", VALID, "\n",
            "Total responses: ", RESPS, "\n",
            "Valid responses: ", VRESP, "\n")
    counts
  } else if (!isTRUE(boolean)) {
    CASES = nrow(data)
    RESPS = length(data[!is.na(data)])
    if (!isTRUE(any(sapply(data, is.factor)))) {
      if (is.null(factors)) {
        stop("Input variables must be factors.
        Please provide factors using the 'factors' argument or
             convert your data to factor before using function.")
      } else {
        data[sapply(data, is.character)] = 
          lapply(data[sapply(data, is.character)], 
                 function(x) factor(x, levels=factors))
      }      
    }
    if (isTRUE(basic)) {
      ROWS = levels(unlist(data))
      OUT = table(unlist(data))
      PCT = (OUT/sum(OUT)) * 100
      OUT = data.frame(ROWS, OUT, PCT, row.names=NULL)
      OUT = data.frame(Item = OUT[, 1], Freq = OUT[, 3], 
                       Pct.of.Resp = OUT[, 5],
                       Pct.of.Cases = (OUT[, 3]/CASES)*100)
      message("Total cases:     ", CASES, "\n",
              "Total responses: ", RESPS, "\n")
      OUT
    } else if (!isTRUE(basic)) {
      Combos = apply(data, 1, function(x) paste0(sort(x), collapse = sep))
      Weight = as.numeric(rowSums(!is.na(data)))
      OUT = data.frame(table(Combos, Weight))
      OUT = OUT[OUT$Freq > 0, ]
      OUT$Weight = as.numeric(as.character(OUT$Weight))
      if(OUT$Weight[1] == 0) { OUT$Weight[1] = 1 }
      OUT$Weighted.Freq = OUT$Weight*OUT$Freq
      OUT$Pct.of.Resp = (OUT$Weighted.Freq/RESPS)*100
      OUT$Pct.of.Cases = (OUT[, 3]/CASES)*100
      message("Total cases:     ", CASES, "\n",
              "Total responses: ", RESPS, "\n")
      OUT[-2]
    } 
  }
}
