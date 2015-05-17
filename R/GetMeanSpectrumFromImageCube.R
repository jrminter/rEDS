#' getMeanSpectrumFromImageCube
#'
#' Get an mean spectrum from a vector of channels in  a  \code{DataCube} object
#'
#' @param x The datacube object.
#'
#' @param ch A vector of channels to average.
#'
#' @param debug A logical to print info. Default \code{FALSE}.
#'
#' @return df data frame (ch, ct).
#'
#' @keywords keywords
#'
#' @include DataCube.R
#'
#' @export
getMeanSpectrumFromImageCube <- function(x, ch, debug=FALSE){
  if(debug){
    print(dim(x@data))
    print(ch)
    print(ch[1])
  }
  cts <- 1:length(ch)
  storage.mode(cts) <- "numeric"
  for(i in 1:length(ch)){
    theCh <- ch[i]
    tmp <- as.matrix(x@data[,, theCh])
    storage.mode(tmp) <- "numeric"
    avg <- mean(tmp)
    cts[i] <- avg
  }
  # compute a data frame average average

  df <- data.frame(ch=ch,cts=cts)
  return(df)
}
