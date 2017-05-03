#' Prepare a DTSA-II PhiRhoZ.csv file for plotting
#'
#' Use the results from DTSA-II Monte Carlo simulations that
#' produce a PhiRhoZ.csv file to create an R dataframe for
#' plotting. Note: this file is TAB delimited and has two
#' header lines and so needs some cleanup...
#'
#' @param inDir A string containing the directory with the
#' PhiRhoZ.csv file. Terminate with a '/' !!!
#'
#' @param fName A string. Default is 'PhiRhoZ.csv'
#'
#' @return a dataframe
#'
#' @keywords keywords
#'
#' @examples
#'
#' # not run...
#' # library(rEDS)
#' # in <- '/path/to/'
#' # fi <- 'myPhiRhoZ.csv'
#' # df <- prepDataframeDtsaPhiRhoZ(in, fi)
#' # print(head(df))
#'
#' @export
prepDataframeDtsaPhiRhoZ <- function(inDir, fName='PhiRhoZ.csv'){
  # Karl Broman says "your closest collaborator is you, three years from now,
  # and you don't respond to email." These comments are for future me and
  # anyone else...
  #
  # Sometimes I rename the default, so make it flexible...
  fi <- sprintf('%s%s', inDir, fName)
  li <- readLines(fi, n=1)
  # generate the column names from the file
  nam <- unlist(strsplit(li,'\t'))
  # read the raw data, skipping the names and the units. It is tab delimited...
  df <-read.csv(fi, header=FALSE, sep='\t', skip=2, col.names=nam)
  # the values are ALWAYS 0 before df$Max = 5000
  df <- df[df$Max > 5000.,]
  # the last half of the columns are in terms of intensity.
  # The next three lines delete these.
  lo <- 0.5*ncol(df)
  hi <- ncol(df)
  s <- seq(lo,hi)
  df <- df[, -s]
  # we really want the midpoint of the bins, so we generate it and replace the
  # first two colums with the midpoint
  x <- 0.5*(df$Min + df$Max)-5000.0
  # print(head(x))
  df <- df[, -1]
  df$Max <- x
  names(df)[1] <- "Z.\U00B5m"
  # reset the rownames and then return the dataframe.
  rownames(df) <- NULL
  return(df)
}
