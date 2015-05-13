#' rotate180.matrix
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
rotate180.matrix <- function(x) { 
  xx <- rev(x)
  dim(xx) <- dim(x)
  return(xx)
}
