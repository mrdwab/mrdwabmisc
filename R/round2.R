#'Round numbers the way you learned in school
#'
#'The \code{\link{round2}} function rounds numbers in the way you probably
#'learned in school, that is, round up to the next number for values of 5 and
#'above.
#'
#'To reduce bias in rounding, R's \code{\link{round}} function uses a
#'"round-to-even" approach. Still, many people are surprised when they find
#'that R's \code{round} function will return the same value for
#'\code{round(1.5)} and \code{round(2.5)}. This function uses the rounding
#'approach found in most school lessons and in software like Excel to make the
#'results comparable.
#'
#'@param x The number (or vector of numbers) that needs rounding.
#'@param digits The number of decimal places in the output.
#'@author Unknown (see "References")
#'@seealso \code{\link{round}}
#'@references Function originally found in an anonymous comment at the
#'Statistically Significant blog. See
#'\url{http://www.webcitation.org/68djeLBtJ}
#'@examples
#'
#'input <- seq(from = 0.5, by = 1, length.out = 10)
#'round(input)
#'round2(input)
#'round(input/10, digits = 1)
#'round2(input/10, digits = 1)
#'\dontshow{rm(input)}
#'
round2 <- function(x, digits = 0) {
  posneg = sign(x)
  z = abs(x) * 10 ^ digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg
}
