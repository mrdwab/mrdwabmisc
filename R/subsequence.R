#' Identify Sequences in a Vector
#' 
#' The \code{\link{subsequence}} function is like the inverse of
#' \code{\link{rep}}, and is somewhat related to \code{\link{rle}}. It detects
#' the sequence in a vector and returns the period of the sequence, the actual
#' sequence, the number of times the sequence is repeated, and optionally, a
#' "Groups" vector the same length as the input vector that can be used as a
#' grouping variable.
#' 
#' 
#' @param data The input vector
#' @param groups Logical. Should the grouping vector be returned?
#' @author Ananda Mahto
#' @seealso \code{\link{rep}}, \code{\link{rle}},
#' @references This function was written as an answer to the following Stack
#' Overflow question: \url{http://stackoverflow.com/q/12824931/1270695}
#' @examples
#' 
#' ## Some sample data
#' s1a <- rep(c(1, 2, 3), 3)
#' s1b <- c(s1a, 1)
#' s2 <- rep(c(1, 2, 3), 50)
#' s3 <- c(1, 2, 3, 4, 2, 3, 4, 1, 2, 3, 4, 2, 3, 4)
#' set.seed(1)
#' s4 <- rep(sample(300, 15), 5)
#' 
#' subsequence(s1a)
#' ## Note the creation of a grouping variable
#' subsequence(s1b, groups = TRUE)
#' subsequence(s2)
#' subsequence(s3)
#' subsequence(s4)
#' 
#' @export subsequence
subsequence <- function(data, groups = FALSE) {
    ii <- 0
    while (TRUE) {
        ii <- ii + 1
        LAG <- all((diff(data, lag = ii) == 0))
        if (isTRUE(LAG)) {
            break
        }
    }
    out = list(Period = ii,
               Sequence = data[1:ii],
               Repetitions = length(data)/ii)
    if (isTRUE(groups)) {
        Groups <- rep(1:out$Repetitions, each = ii)
        if (length(Groups) < length(data)) {
          Fill <- length(data) - length(Groups)
          out$Groups <- c(Groups, rep(max(Groups)+1, Fill))
        }
    }
    class(out) <- c("subSequence", class(out))
    out
}
