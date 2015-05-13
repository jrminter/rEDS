#' GetAvgSliceFromImageCube
#'
#' Get an average slice from a vector of channels in  a  \code{DataCube} object
#'
#' @param x The datacube object.
#'
#' @param ch A vector of channels to average.
#'
#' @param debug A logical to print info. Default \code{FALSE}.
#'
#' @return msa A numeric matrix (ct/ch).
#'
#' @keywords keywords
#'
#' @include DataCube.R
#'
#' @export
GetAvgSliceFromImageCube <- function(x, ch, debug=FALSE){
  if(debug){
    print(dim(x@data))
    print(ch)
    print(ch[1])
  }
  mat <- as.matrix(x@data[,,ch[1]])
  storage.mode(mat) <- "numeric"
  for(i in 2:length(ch)){
    theCh <- ch[i]
    tmp <- as.matrix(x@data[,, theCh])
    storage.mode(tmp) <- "numeric"
    mat <- mat + tmp
  }
  # compute the average average
  mat <- mat/as.numeric(length(ch))
  return(mat)
}
