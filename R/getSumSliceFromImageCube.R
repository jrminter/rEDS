#' getSumSliceFromImageCube
#'
#' Get a sum slice from a vector of channels in a DataCube object
#'
#' @param x The datacube object.
#'
#' @param ch A vector of channels to average.
#'
#' @param debug A logical to print info. Default FALSE.
#'
#' @return An integer matrix (total cts/ch).
#'
#' @keywords keywords
#'
#' @include DataCube.R
#'
#' @export
getSumSliceFromImageCube <- function(x, ch, debug=FALSE){
  if(debug){
    print(dim(x@data))
    print(ch)
    print(ch[1])
  }
  mat <- as.matrix(x@data[,,ch[1]])
  for(i in 2:length(ch)){
    theCh <- ch[i]
    tmp <- as.matrix(x@data[,, theCh])
    mat <- mat + tmp
  }
  return(mat)
}
