#' Compute the C thickness on Si at 5kV
#'
#' Use the results from a LOESS fit to results from DTSA-II Monte Carlo
#' simulations of C thicknesses between 0 and 100 nm on Si at 5 kV
#'
#' @param c.to.si Ratio of C to Si peak intensities at 5 kV.
#'
#' @param verbose Boolean flag. Default FALSE. Print info if TRUE.
#'
#' @return The C thickness in nm
#'
#' @keywords keywords
#'
#' @examples
#'
#' library(rEDS)
#' a <- computeCarbonThicknessOnSi5kV(0.5)
#' print(a)
#'
#' @export
computeCarbonThicknessOnSi5kV <- function(c.to.si, verbose=FALSE){
  t.C.nm <- seq(from=0.5, to=100, by=0.5)
  df2 <- data.frame(t.C.nm=t.C.nm, c.to.si.mu=t.C.nm)
  fi <- system.file("extdata", "c-to-si-loess-5kV.rda", package = "rEDS")
  load(fi)
  if(verbose) print(ls())
  df2$c.to.si.mu <- predict(c.to.si.loess.5kV, newdat=df2)
  df2$dif <- sqrt((df2$c.to.si.mu-c.to.si)^2)
  mv = min(df2$dif)
  t <- df2$t.C.nm[which(df2$dif==mv)]
  return(t)
}
