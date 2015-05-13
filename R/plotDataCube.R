#' Plot a data cube
#'
#' Generic function to plot a data cube. From Jeff Davis at NIST.
#'
#' @param x The Spectrum object \code{x}.
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
#'
#' @docType methods
#' @rdname datacube-plot-methods
setMethod("plot","DataCube", function(x,...,ch){
  if (x@record.by.image==FALSE){
    plot(t(x@data[ch,,]))
  }
  if (x@record.by.image==TRUE){
    plot(t(x@data[,,ch]))
  }
}
)
