#' @title Euclidean Function
#'
#' @description Provides the Greatest Common Divisor for 2 integer values
#'
#' @param x integer value
#' @param y integer value
#'
#' @return A resulting number that is the greatest common divisor of the 2 input values
#'
#' @examples
#' euclidean(10, 100)
#' euclidean(x=10, y=100)
#' result_var <- euclidean(10,100)
#'
#' @export euclidean
#'
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
euclidean <- function (x,y) {
  x <- abs(x)
  y <- abs(y)
  gcd <- 1
  if (is.integer(x) == TRUE || is.integer(y) == TRUE) stop ("invalid input")
  for (i in 1:min(x,y)) {
    if (x %% i == 0 && y %% i == 0) gcd <- i
  }
  gcd
}
