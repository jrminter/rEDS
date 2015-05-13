#' Flip matrix (upside-down)
#'
#' From http://www1.maths.lth.se/help/R/image/
#'
#' @param x Input matrix.
#'
#' @return The rotated matrix
#'
#' @keywords keywords
#'
#' @include rotateMatrix180.R
#'
#' @export
flipMatrix <- function(x) {
  xx <- mirrorMatrix(rotateMatrix180(x))
  return(xx)
}
