#' cropSlice
#'
#' Crop a slide (matrix) to remove undesired regions. helpful to
#' process partially empty data cubes from Oxford
#'
#' @param slice Input matrix (slice)
#'
#' @param cutB Number of pixels to cut from the bottom.
#'
#' @param cutL Number of pixels to cut from the left.
#'
#' @param cutT Number of pixels to cut from the top.
#'
#' @param cutR Number of pixels to cut from the right.
#'
#' @return The cropped matrix (slice)
#'
#' @keywords keywords
#'
#' @export
cropSlice <- function(slice, cutB, cutL, cutT, cutR){
  nCol <- ncol(slice)
  nRow <- nrow(slice)
  ret <- slice[cutL:nRow, ]
  # image(ret)
  ret <- ret[,cutB:nCol]
  # image(ret)
  nCol <- ncol(ret)
  nRow <- nrow(ret)
  end <- nRow-cutR
  ret <- ret[1:end, ]
  # image(ret)
  end <- nCol-cutT
  ret <- ret[, 1:end]
  # image(ret)

  return(ret)
}