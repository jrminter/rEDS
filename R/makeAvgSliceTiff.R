#' Make a TIF image from a matrix
#'
#' Useful for analysis in NIST Lispix
#'
#' @param mat The matrix with the average slice to convert
#'
#' @param name The base name for the TIF file
#'
#' @param tifDir A directory for the pdf histogram. Defaults to ../tif.
#'
#' @return none
#'
#' @keywords keywords
#'
#' @import rtiff
#'
#' @include flipMatrix.R
#' @include rotateMatrix90.R
#'
#' @export
#'
makeAverageSliceTif <- function(mat, name, tifDir='../tif'){
  # create the directory if it does not exist
  dir.create(tifDir, showWarnings = FALSE, recursive = TRUE)
  tifFile <- paste0(tifDir, '/', name, '.tif' )
  if(file.exists(tifFile)) file.remove(tifFile)
  writeTiff(flipMatrix(rotateMatrix90(mat)), tifFile)
}

