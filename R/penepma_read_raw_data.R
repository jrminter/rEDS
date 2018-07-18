#' Retrieve the raw channel data from a PENEPMA spectrum
#'
#' @param fi String. The path to the file. ex: "./pe-spect-01.dat"
#'
#' @return a data frame
#'
#' @import ramify
#'
#' @export
#'
penepma_read_raw_data <- function(fi){
  library(ramify)

  df <- read.table(fi, sep=" ", skip=12)
  df <- df[, -c(1,2,3) ]
  df <- df[, -c(2, 4)]
  names(df) <- c('ev', 'mu', 'se')
  df$ev <- df$ev/1000.
  names(df) <- c('keV', 'mu', 'se') # make keV

  mv <- max(df$mu)
  df$mu <- clip(df$mu, .min = 1.0e-10, .max = mv)
  df$mu <- 1e9*df$mu
  mv <- max(df$se)
  df$se <- clip(df$se, .min = 1.0e-10, .max = mv)
  df$se <- 1e9*df$se
  return(df)
}
