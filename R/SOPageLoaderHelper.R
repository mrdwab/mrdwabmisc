#' Download a Stack Overflow question page as a character vector
#' 
#' Helper function for other Stack Overflow related functions. Downloads the
#' page as a character vector and extracts the portion that is needed for other
#' functions.
#' 
#' 
#' @param qid The numeric question ID.
#' @return A character vector
#' @author Ananda Mahto
SOQuestionPage <- function(qid) {
  T1 <- suppressWarnings(
    readLines(paste("http://stackoverflow.com/q", 
                    as.character(qid), sep = "/")))
  cStart <- which(
    grepl(
      '<div itemscope itemtype="http://schema.org/Article">', T1, 
      fixed = TRUE)) + 3
  cEnd <- which(grepl("<a name='new-answer'></a>", T1, fixed = TRUE)) - 1
  T1[cStart:cEnd]
}
NULL










#' Clean text within \code{<pre><code>} tag blocks
#' 
#' Text within \code{<pre><code>} tag blocks need to be "cleaned". For R code
#' that usually means changing \code{&lt;} and \code{&gt;} to \code{<} and
#' \code{>}.
#' 
#' 
#' @param x The input character vector
#' @return A character vector
#' @author Ananda Mahto
PreTagClean <- function(x) {
  x <- gsub(".*<pre><code>", "", x)
  x <- gsub("</code></pre>.*", "", x)
  x <- gsub("&lt;", "<", x)
  x <- gsub("&gt;", ">", x)
  pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"
  x <- gsub(pattern, "\\1", x)
  x
}
NULL










#' Create a list of code blocks from an input character vector
#' 
#' A helper function for \code{\link{SOCodeBlocks}} to extract the lines in the
#' character vector corresponding to a code block.
#' 
#' 
#' @param x The input character vector
#' @return A character vector of just the code blocks from a Stack Overflow
#' question.
#' @author Ananda Mahto
CodeList <- function(x) {
  T1 <- data.frame(start = which(grepl("<pre><code>", x)),
                   end = which(grepl("</code></pre>", x)))
  T1 <- data.frame(t(T1))
  codeList <- lapply(T1, function(z) x[z[1]:z[2]])
  codeList <- lapply(codeList, PreTagClean)
  codeList
}
