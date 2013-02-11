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