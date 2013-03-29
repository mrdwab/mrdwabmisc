#'Tabulates columns from a \code{data.frame} containing multiple-response data
#'
#'The \code{\link{multi.freq.table}} function takes a \code{\link{data.frame}}
#'containing Boolean responses to multiple response questions and tabulates the
#'number of responses by the possible combinations of answers.
#'
#'In addition to tabulating the \emph{frequency} (\code{Freq}), there are two
#'other columns in the output: \emph{Percent of Responses} (\code{Pct.of.Resp})
#'and \emph{Percent of Cases} (\code{Pct.of.Cases}).
#'
#'Percent of Responses is the frequency divided by the total number of answers
#'provided; this column should sum to 100%. In some cases, for instance when a
#'combination table is generated and there are cases where a respondent did not
#'select any option, the Percent of Responses value would be more than 100%.
#'Percent of Cases is the frequency divided by the total number of valid cases;
#'this column would most likely sum to more than 100% when a basic table is
#'produced since each respondent (case) can select multiple answers, but should
#'sum to 100% with other tables.
#'
#'@param data The multiple responses that need to be tabulated.
#'@param sep The desired separator for collapsing the combinations of options;
#'defaults to \code{""} (collapsing with no space between each option name).
#'@param boolean Are you tabulating boolean data (see \code{dat} Examples)?
#'Defaults to \code{TRUE}.
#'@param factors If you are trying to tabulate non-boolean data, and the data
#'are not factors, you can specify the factors here (see \code{dat2} Examples).
#'Defaults to \code{NULL} and is not used when \code{boolean = TRUE}.
#'@param NAto0 Should \code{NA} values be converted to \code{0}? Defaults to
#'\code{TRUE}, in which case, the number of valid cases should be the same as
#'the number of cases overall. If set to \code{FALSE}, any rows with \code{NA}
#'values will be dropped as invalid cases. Only applies when \code{boolean =
#'TRUE}.
#'@param basic Should a basic table of each item, rather than combinations of
#'items, be created? Defaults to \code{FALSE}.
#'@param dropzero Should combinations with a frequency of zero be dropped from
#'the final table? Defaults to \code{TRUE}. Does not apply when \code{boolean =
#'TRUE}.
#'@param clean Should the original tabulated data be retained or dropped from
#'the final table? Defaults to \code{TRUE} (drop). Does not apply when
#'\code{boolean = TRUE}.
#'@author Ananda Mahto
#'@references \code{apply} shortcut for creating the \code{Combn} column in the
#'output by Justin. See: \url{http://stackoverflow.com/q/11348391/1270695} and
#'\url{http://stackoverflow.com/q/11622660/1270695}
#'@examples
#'
#'## ========================================= ##
#'## ============= BOOLEAN DATA ============== ##
#'
#'# Make up some data
#'set.seed(1)
#'dat <- data.frame(A = sample(c(0, 1), 20, replace=TRUE),
#'               B = sample(c(0, 1, NA), 20,
#'                          prob=c(.3, .6, .1), replace=TRUE),
#'               C = sample(c(0, 1, NA), 20,
#'                          prob=c(.7, .2, .1), replace=TRUE),
#'               D = sample(c(0, 1, NA), 20,
#'                          prob=c(.3, .6, .1), replace=TRUE),
#'               E = sample(c(0, 1, NA), 20,
#'                          prob=c(.4, .4, .2), replace=TRUE))
#'
#'# View your data
#'dat
#'
#'# How many cases have "NA" values?
#'table(is.na(rowSums(dat)))
#'
#'# Apply the function with all defaults accepted
#'multi.freq.table(dat)
#'
#'# Tabulate only on variables "A", "B", and "D", with a different
#'# separator, keep any zero frequency values, and keeping the
#'# original tabulations. There are no solitary "D" responses.
#'multi.freq.table(dat[c(1, 2, 4)], sep="-", dropzero=FALSE, clean=FALSE)
#'
#'# As above, but without converting "NA" to "0".
#'# Note the difference in the number of valid cases.
#'multi.freq.table(dat[c(1, 2, 4)], NAto0=FALSE,
#'               sep="-", dropzero=FALSE, clean=FALSE)
#'
#'# View a basic table.
#'multi.freq.table(dat, basic=TRUE)
#'
#'## ========================================= ##
#'## =========== NON-BOOLEAN DATA ============ ##
#'
#'# Make up some data
#'dat2 <- structure(list(Reason.1 = c("one", "one", "two", "one", "two",
#'                                 "three", "one", "one", NA, "two"),
#'                    Reason.2 = c("two", "three", "three", NA, NA,
#'                                 "two", "three", "two", NA, NA),
#'                    Reason.3 = c("three", NA, NA, NA, NA,
#'                                 NA, NA, "three", NA, NA)),
#'                    .Names = c("Reason.1", "Reason.2", "Reason.3"),
#'                    class = "data.frame",
#'                    row.names = c(NA, -10L))
#'
#'# View your data
#'dat2
#'
#'\dontrun{# The following will not work.
#'# The data are not factored.
#'multi.freq.table(dat2, boolean=FALSE)}
#'
#'# Factor create the factors.
#'multi.freq.table(dat2, boolean=FALSE,
#'               factors = c("one", "two", "three"))
#'
#'# And, a basic table.
#'multi.freq.table(dat2, boolean=FALSE,
#'               factors = c("one", "two", "three"),
#'               basic=TRUE)
#'\dontshow{rm(dat, dat2)}
#'
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
