#' Plot a PENEPMA spectrum file
#'
#' @param fi String. The path to the file. ex: "./pe-spect-01.dat"
#'
#' @param title String. The title for the plot.
#'
#' @return a ggplot
#'
#' @import ggplot2
#'
#' @export
#'
#'
penepma_plot_spectrum <- function(fi, title){
  ev_per_ch <- penepma_get_ev_per_ch(fi)
  df <- penepma_read_raw_data(fi)
  plt <- ggplot() +
         geom_line(data=df, aes(x=keV, y=mu),
                   colour="darkblue") +
         xlab("keV") +
         ylab("counts") +
         ggtitle(title) +
         theme(axis.text=element_text(size=12),
               axis.title=element_text(size=14),
               plot.title = element_text(hjust = 0.5))
         # coord_trans(y = "sqrt") +
         NULL

  return(plt)

}
