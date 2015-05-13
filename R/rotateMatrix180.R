#' rotateMatrix180
#' 
#' Rotate a matrix 180 degrees clockwise.
#' 
#' From http://www1.maths.lth.se/help/R/image/
#'
#' @param x The input matrix.
#'
#' @return The rotated matrix
#'
#' @keywords keywords
#'
#' @export
rotateMatrix180 <- function(x) { 
  xx <- rev(x)
  dim(xx) <- dim(x)
  return(xx)
}
