#' Plot a CASINO PhiRhoZ dataframe
#'
#' Plot a dataframe from a Casino PhiRhoZ file processed
#' with readSingleCasinoPrz
#'
#' @param df A dataframe processed with reaSingleCasinoPrz
#'
#' @param name string. The name of the element and transition.
#'
#' @param e0 number. The accelerating voltage.
#'
#' @param z.min number. The minimum z in microns. default = 0.0
#'
#' @param z.max number. The maximum z in microns. default = 2.0
#'
#' @param prz.max number. The maximum phi rho-z. default 2.75
#'
#' @param prz.step number. The step size in prz space. default 0.1
#'
#' @return A ggplot2 plot
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
#' # plt <- plotCasinoPrz(df, "Zn L3-M5", 3.0, z.min=0, z.max=.75, prz.step=0.05)
#' # print(plt)
#'
#' @export

plotCasinoPrz <- function(df, name, e0,
                          z.min=0, z.max=2.0,
                          prz.max=2.75, prz.step=0.1){
  library(ggplot2)

  lab1 <- "Gen"
  lab2 <- "Emi"
  lab3 <- sprintf("%s \u03D5(\u03C1z)", name)
  lab4 <- sprintf("\u03D5(\u03C1z) of %s X-rays at %g kV", name, e0)
  plt <- ggplot(df, aes(x = nm/1000.)) +
    geom_point(aes(y = generated), colour="blue") +
    annotate("text", label = lab1,
             x = .95*z.max, y = (0.97*prz.max),
             size = 5, colour = "blue") +
    geom_point(aes(y = emitted), colour="red") +
    # geom_smooth(aes(x = z.um, y = eO),
    #            span = loessSpan, color='red') +
    annotate("text", label = lab2,
             x = .95*z.max, y = (0.90*prz.max),
             size = 5, colour = "red") +
    ylab(label=lab3) +
    xlab("depth [\U00B5m]") +
    scale_x_continuous(breaks = seq(from = z.min,
                                    to = z.max,
                                    by = prz.step),
                       limits = c(z.min, z.max)) +
    scale_y_continuous(breaks = seq(from = 0, to = prz.max, by = .25),
                       limits = c(0, prz.max)) +
    ggtitle(lab4) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          plot.title = element_text(hjust = 0.5)) # center the title
  return(plt)
}
