#' Plot a data cube image
#'
#' Generic function to create an image from a data cube channel
#'
#' @param x The Spectrum object x.
#'
#' @param ... Additional graphical argument list.
#'
#' @param ch An integer. The channel to plot.
#'
#' @return A plot.
#'
#' @seealso \code{\link{plot}}
#'
#' @include DataCube.R
#'
#' @export
#' @docType methods
#' @rdname image-methods
#'
setMethod("image", "DataCube", function(x,...,ch){
  if (x@record.by.image==FALSE){
    image(as.matrix(x@data[ch,,]),col=rainbow(16),useRaster=TRUE,axes=FALSE,main=paste("Channel",ch),sub=x@rawfile)
  }
  if (x@record.by.image==TRUE){
    image(as.matrix(x@data[,,ch]),col=rainbow(16),useRaster=TRUE,axes=FALSE,main=paste("Channel",ch),sub=x@rawfile)
  }
}
)
