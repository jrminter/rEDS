#' Plot an MSA spectrum
#'
#' From Jeff Davis at NIST
#'
#' TO DO: see why plot of multi column MSA fails...
#'
#' @param x The Spectrum object \code{x}.
#'
#' @param ... Additional graphical argument list.
#'
#' @param yzoom A factor \code{yzoom}. To zoom y axis, default 1
#'
#' @param minEnergy The maximum energy to display, default 100 ev. Useful to cut off noise peak.
#'
#' @param maxEnergy The maximum energy to display, default 10000 ev.
#'
#' @param doLog A boolean flag for log intensity, default = FALSE
#'
#' @return A plot.
#'
#' @seealso \code{\link{plot}}
#'
#' @import methods
#'
#' @export
#' @docType methods
#' @rdname spectrum-plot-methods
#'
setMethod("plot", "Spectrum",
          function(x,..., yzoom=1, minEnergy=100, maxEnergy=10000, doLog=FALSE){
  par(mfrow=c(1,1))
  xaxis=seq(from=-abs(x@offset),
            to=(x@npoints*x@xperchan-abs(x@offset)-x@xperchan),
            by=x@xperchan)
  if (x@xunits=="keV") xaxis<-xaxis*1000
  counts=x@spectrum[1:length(xaxis)]
  # make a data frame
  dat  <- data.frame(ev=xaxis, intensity=counts)
  dat  <- dat[dat$ev > minEnergy, ]
  dat  <- dat[dat$ev < maxEnergy, ]
  if(doLog==TRUE){
    dat <- dat[dat$intensity > 0, ]
    X <- dat$ev
    Y <- dat$intensity
    plot(X, Y, xlab="Energy [eV]", ylab="log Counts", main=x@title,
         type="l", col="red", lwd=2,
         # ylim=c(0, maxY),
         xlim=c(0, max(X)), log = "y")
  } else {
    X <- dat$ev
    Y <- dat$intensity
    plot(X, Y, xlab="Energy [eV]", ylab="Counts", main=x@title,
         type="l", col="red", lwd=2,
         ylim=c(0,(max(Y, na.rm=TRUE)/ yzoom)),
         xlim=c(0, max(X)))
  }
}
)
