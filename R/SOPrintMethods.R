
print.SOCodeBlocks <- function(x, ...) {
  lapply(seq_along(x), 
         function(y) {
           cat("\n>>> Block", y, "\n\n")
           cat("", paste(x[[y]], "\n"))
           cat("", paste(rep("-", 60), sep = "", collapse = ""), "\n")
         })
  invisible(x)
}

print.QAList <- function(x, ...) {
  y <- lapply(x, PreTagClean)
  z <- c(paste("Question      : ", attr(x, "QTitle"), 
               "\n>>>  Question ID   : ", attr(x, "QID"), 
               "\n>>>  Question Tags : ", attr(x, "QTags"), sep =""),
         paste("Answer ID:", attr(x, "AnswerIDs")))
  lapply(seq_along(y), 
         function(Y) {
           cat("\n>>> ", z[Y], "\n\n")
           cat("", paste(y[[Y]], "\n"))
           cat("", paste(rep("-", 60), sep = "", collapse = ""), "\n")
         })
  invisible(x)
} 