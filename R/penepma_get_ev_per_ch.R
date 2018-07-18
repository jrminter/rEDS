#' Retrieve the ev/ch from a PENEPMA spectrum
#'
#' @param fi String. The path to the file. ex: "./pe-spect-01.dat"
#'
#' @return the ev/channel (numeric)
#'
#' @export
penepma_get_ev_per_ch <- function(fi){
  x <- readLines(fi, 6)[6]
  y <- as.numeric(strsplit(x, " ")[[1]][7])
  return(y)
}
