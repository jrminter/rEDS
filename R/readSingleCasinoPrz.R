#' Read a single Casino PhiRhoZ file saved from File > Copy Data
#'
#' Use the the .txt file that can be saved from Casino 2 with
#' CopyData from the Phi-RhoZ view.
#'
#' @param dirName A string containing the directory with the
#' PhiRhoZ.txt file. Do NOT terminate with a '/' !!!
#'
#' @param fName A string. The base file name. See the call
#' example below.
#'
#' @param bTrunc A boolean. Default TRUE. Truncate zero values for Gen.
#'
#' @return a dataframe
#'
#' @keywords keywords
#'
#' @examples
#'
#' # not run...
#' # library(rEDS)
#' # dn <- "C:/Users/l837410/Documents/src/casino/CD1452A"
#' # fn <- "prz-Zn-03kV"
#' # df <- readSingleCasinoPrz(dn, sprintf("%s.txt", fn))
#' # print(head(df))
#'
#' @export

readSingleCasinoPrz <- function(dirName, filName, bTrunc=TRUE){
  fi <- sprintf("%s/%s", dirName, filName)
  df <- read.table(fi,header = FALSE, sep = "", skip=2)
  names(df) <- c("nm", "generated", "emitted")
  if(bTrunc){
    df <- df[df$gen>0, ]
    row.names(df) <- NULL
  }
  return(df)
}
