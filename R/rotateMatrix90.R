#' rotateMatrix90
#'
#' Rotate a matrix 90 degrees clockwise.
#'
#' From http://www1.maths.lth.se/help/R/image/
#'
#' @param x input matrix.
#'
#' @return The rotated matrix
#'
#' @keywords keywords
#'
#' @include mirrorMatrix.R
#'
#' @export
#'
rotateMatrix90 <- function(x) {
  a <- t(mirrorMatrix(x))
  return(a)
}
