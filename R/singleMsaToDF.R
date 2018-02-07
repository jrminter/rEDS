#' Convert a SingleMSA spectrum to a data frame
#'
#' @param fi string containing the file path to the spectrum
#'
#' @param pc float containing the probe current
#'
#' @return a dataframe with keV and counts
#'
#' @export
#'
singleMsaToDF <- function(fi, pc){
  spc <- singleMSA(spec=fi, pc)
  cts <- spc@spectrum
  xaxis=seq(from=-abs(spc@offset),
            to=(spc@npoints*spc@xperchan-abs(spc@offset)-spc@xperchan),
            by=spc@xperchan)
  if (spc@xunits != "keV") xaxis <- xaxis/1000.
  df <- data.frame(keV=xaxis, cts=cts)
  return(df)
}
