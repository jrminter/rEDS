#' getChanInt
#'
#' Get a channel interval to integrate for a given midpoint
#' and half-width given key detector parameters.
#'
#' @param keV The mid-point for an X-ray line or background
#' region in keV.
#'
#' @param eVhW The half width (in eV) to integrate. Default
#' value is 64 eV.
#'
#' @param evPerCh The channel width for the detector. The
#' default value is 4.9943 eV.
#'
#' @param offset The zero offset (eV). The default value is
#' -94.4842 eV.
#'
#' @return A concatenated list of (lo, hi) channwl.
#'
#' @keywords keywords
#'
#' @export
getChanInt <- function(keV, eVhW=64.0, evPerCh=4.9943, offset=-94.4842){
  computeChannel <- function(keV, evPerCh, offset){
    eV <- 1000.0 * keV
    eV <- eV + offset
    ch <- eV/evPerCh
    return (ch)
  }
  mid <- computeChannel(keV, evPerCh, offset)
  chHW <- eVhW / evPerCh
  lo <- round(mid-chHW, 0)
  hi <- round(mid+chHW, 0)
  
  ret <- c(lo, hi)
  names(ret) <- c("lo", "hi")
  return(ret)
}
