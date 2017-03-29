#' Prepare a DTSA-II PhiRhoZ.csv file for plotting
#'
#' Use the results from DTSA-II Monte Carlo simulations that
#' produce a PhiRhoZ.csv file to create an R dataframe for
#' plotting. Note: this file is TAB delimited and has two
#' header lines and needs some cleanup...
#'
#' @param phiRhoZFile The path to the PhiRhoZ.csv file.
#'
#' @param verbose Boolean flag. Default FALSE. Print info if TRUE.
#'
#' @return a dataframe
#'
#' @keywords keywords
#'
#' @examples
#'
#' # not run...
#' # library(rEDS)
#' # fi <- '/path/to/PhiRhoX.csv'
#' # df <- prepDataframeDtsaPhiRhoZ(fi)
#' # print(head(df))
#'
#' @export
prepDataframeDtsaPhiRhoZ <- function(phiRhoZFile, nSkip=11, verbose=FALSE){
  res <- readLines(phiRhoZFile)
  rawNames <-res[1]
  colN <- unlist(strsplit(rawNames, '\t'))
  if(verbose) print(head(colN))
  # DTSA PhiRhoZ.csv files are really tab delimnited and have two header lines
  df <- read.csv(phiRhoZFile, header = FALSE, skip=nSkip, sep = '\t')
  names(df) <- colN
  if(verbose) print(head(df))
  # average the min and max ramge to get the midpoint and subtract 5000 (a working distance?)
  z <- 0.5*(df[,1] + df[,2]) - 5000.0
  df[,1] <- z
  colnames(df)[1] <- "Z [\U00B5m]"
  df <- df[,-2]
  return(df)
}
