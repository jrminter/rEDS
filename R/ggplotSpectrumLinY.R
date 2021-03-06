#' Make a ggplot from an MSA spectrum with linear count axis
#'
#' @param spcFile string containing the file path to the spectrum
#'
#' @param spc_title string with the title for the plot. kV will be appended.
#'
#' @param min_keV_limit The minimum keV value to plot. Useful to remove zero noise peak.
#'
#' @param max_keV_limit The maximum limit to plot. If not set, it uses the maximum beam energy in the spectrum.
#'
#' @param del_keV The spacing for energy
#'
#' @param probecur Float value for probe current.
#'
#' @return None. Prints a ggplot
#'
#' @import ggplot2
#'
#' @export
#'
#' @example
#' library(rEDS)
#' fi <- system.file("extdata", "Benitoite.msa", package = "rEDS")
#' ggplotSpectrumLinY(fi, "Benitoite", 0.2, 7.0, 1.0, 2.5)

ggplotSpectrumLinY <- function(spcFile, spc_title,
                               min_keV_limit, max_keV_limit,
                               del_keV, probecur){
  df <- singleMsaToDF(spcFile, probecur)
  max_cts <- max(df$cts)
  df <- df[df$keV < max_keV_limit, ]
  df <- df[df$keV > min_keV_limit, ]
  plt <- ggplot() +
         geom_line(data=df, aes(x=keV, y=cts),
                   colour="darkblue") +
         scale_x_continuous(breaks = seq(from=0, to=max_keV_limit,
                                         by=del_keV),
                                         limits = c(0,max_keV_limit)) +
         xlab("keV") +
         ylab("counts") +
         ggtitle(spc_title) +
         theme(axis.text=element_text(size=12),
               axis.title=element_text(size=12),
               plot.title=element_text(hjust = 0.5)) # center the title

  return(plt)
}
