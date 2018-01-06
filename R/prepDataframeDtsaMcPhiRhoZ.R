#' Prepare a DTSA-II Monte Carlo PhiRhoZ.csv file for plotting
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
#' # inDir <- '/path/to/'
#' # fi <- 'myPhiRhoZ.csv'
#' # df <- prepDataframeDtsaMcPhiRhoZ(inDir, fi)
#' # print(head(df))
#'
#' @export
prepDataframeDtsaMcPhiRhoZ <- function(inDir, fName='PhiRhoZ.csv'){
  # Karl Broman says "your closest collaborator is you, three years from now,
  # and you don't respond to email." These comments are for future me and
  # anyone else...
  #
  # Sometimes I rename the default, so make it flexible...
  fi <- sprintf("%s%s", inDir, fName)
  dataLines <- readLines(fi)
  dataLines <- gsub( "\t", ",", dataLines )
  dataLines <- dataLines[-c(2:11)]
  fBase <- strsplit(fName, ".", TRUE)[[1]][1]
  fOut <- sprintf("%s%s-fixed.csv", inDir, fBase)
  sink(fOut)
  for (l in dataLines){
    cat(l)
    cat('\n')
  }
  sink()
  df <-read.csv(file=fOut,header = TRUE, as.is=TRUE)
  if (file.exists(fOut)) file.remove(fOut)
  z <- 0.5*(df$Min+df$Max)-5000.
  df$Max <- z
  df <-df[, -1]
  nam <- names(df)
  nam[1] <- "Z.um"
  names(df) <- nam
  # n <- ncol(df)
  # n <- round(n/2,0)+1
  # df <-df[,1:n]
  fOut <- sprintf("%s%s-final.csv", inDir, fBase)
  write.csv(df, file=fOut, row.names = FALSE)
  rownames(df) <- NULL
  return(df)
}
