#' @export
#' @aliases print.stringSeedSampling
print.stringSeedSampling <- function(x, digits = 15, prefix = "", ...) {
  if (nrow(x$input) == 1) {
    cat("", "Input Parameters\n", "  Seedbase        : ", sep = prefix)
    utils::str(x$input$seedbase, give.head = FALSE)
    cat("", "  Population      : ", sep = prefix)
    utils::str(x$input$populations, give.head = FALSE)
    cat("", "  Desired Sample  : ", sep = prefix)
    utils::str(x$input$samplesizes, give.head = FALSE)
    cat("", "  Seed Used       : ", sep = prefix)
    utils::str(x$input$seeds, digits = 15, give.head = FALSE)
    cat("\n", "Samples\n\n", sep = prefix)
    print(x$samples, ncol = 10, byrow = TRUE)
  } else {
    cat("", "Input parameters\n\n", sep = prefix)
    print.data.frame(x$input, digits = 15, row.names = FALSE)
    cat("\n", "Samples\n\n", sep = prefix)
    for (i in seq_along(x$samples)) {
      cat("", utils::str(names(x$samples[i]), give.head = FALSE))
      print(x$samples[[i]])
    }
  }
  invisible(x)
}
NULL

#' @export
#' @aliases print.subSequence
print.subSequence <- function (x, digits = getOption("digits"), prefix = "", ...) 
{
  if (is.null(digits)) 
    digits <- getOption("digits")
  cat("", "Subsequences\n", "  Period      : ", sep = prefix)
  utils::str(x$Period, give.head = FALSE)
  cat("", "  Sequence    : ", sep = prefix)
  utils::str(x$Sequence, digits.d = digits,
             vec.len = length(x$Sequence), 
             width = 60, strict.width = "wrap",
             give.head = FALSE)
  cat("", "  Repetitions : ", sep = prefix)
  utils::str(x$Repetitions, digits.d = digits, give.head = FALSE)
  if (!is.null(x$Groups)) {
    cat("", "  Groups      : ", sep = prefix)
    utils::str(x$Groups, digits.d = digits,
               vec.len = length(x$Groups), 
               width = 60, strict.width = "wrap", 
               give.head = FALSE)
  }
  invisible(x)
}
NULL

#' @export
#' @aliases print.sampletosum
print.sampletosum <- function(x, prefix = "", ...) {
  cat("", "SampleToSum\n", "  Target     : ", sep = prefix)
  utils::str(x$Target, give.head = FALSE)
  cat("", "  Total      : ", sep = prefix)
  utils::str(x$Total, give.head = FALSE)
  cat("", "  Iterations : ", sep = prefix)
  utils::str(x$Iterations, give.head = FALSE)
  cat("\n", "  Result     : ", sep = prefix)
  utils::str(x$Result, vec.len = length(x$Result), 
             width = 60, strict.width = "wrap", 
             give.head = FALSE)
  invisible(x)
}
NULL