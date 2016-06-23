#' edsDetectorMultiGgplot
#'
#' Analyze a file with measurements of detector parameters generated
#' by a custom script wrapping DTSA-II's detector calibration alien's
#' functions and plot the response using ggplot2 to prepare a panel plot.
#'
#' @param csvPath The path to a csv file written by the script.
#'
#' @param detName The name (string) for the detector. Used as a
#' title for the plots.
#'
#' @param dcw.lim (default c(5.0030, 5.0045)) a two element list with
#' the expected y-limits for the detector channel width plot.
#'
#' @param dcw.lab (default 5.0044) the expected Y-axis location to write
#' the annotation with the mean channel width.
#'
#' @param dzo.lim (default c(-102.25, -103.75)) a two element list with
#' the expected y-limits for the detector zero offset plot.
#'
#' @param dzo.lab (default -102.) the expected Y-axis location to write
#' the annotation with the mean detector zero axis.
#'
#' @param dmn.lim (default c(129.5, 130.1)) a two element list with the
#' expected y-limits for the detector resolution - FWHM at MnKa plot.
#'
#' @param dmn.lab (default 130.1) the expected Y-axis location to write
#' the annotation with the mean detector resolution.
#'
#' @param dcul.lim (default c(6400, 7400)) a two element list with the
#' expected y-limits for the plot of the intensity detector CuLa peak
#' in cts/sec-nA
#'
#' @param dcul.lab (default 7400.) the expected Y-axis location to write
#' the annotation with the intensity detector CuLa peak in cts/sec-nA.
#'
#' @param bVerbose A flag (default FALSE) to print intermediate
#' diagnosticresults.
#'
#'
#' @return None
#'
#' @keywords keywords
#'
#' @export
#'
edsDetectorMultiGgplot <- function(csvPath, detName,
         dcw.lim=c(5.0030, 5.0045),
         dcw.lab=5.0044,
         dzo.lim=c(-102.25, -103.75),
         dzo.lab=-102.,
         dmn.lim=c(129.5, 130.1),
         dmn.lab=130.1,
         dcul.lim=c(6400, 7400),
         dcul.lab=7400.,
         bVerbose=FALSE){

  library(ggplot2)
  library(grid)
  pd <- position_dodge(0.)
  pushViewport(viewport(layout=grid.layout(3, 2,
                                           heights=unit(c(0.5, 5, 5),
                                                        "null"))))

  df <- read.csv(csvPath,header=TRUE, as.is=TRUE)

  # start channel width plot
  dcw <- ggplot( data = df, aes( date, cw.ev.mu ))
  dcw <- dcw + expand_limits(y=dcw.lim)
  dcw <- dcw + geom_errorbar(aes(ymin=cw.ev.mu-cw.ev.unc,
                                 ymax=cw.ev.mu+cw.ev.unc),
                             width=.5, size=1, position=pd)
  dcw <- dcw + geom_line(size=1)
  dcw <- dcw + geom_point(size=2)
  dcw <- dcw + xlab("Date")
  dcw <- dcw + ylab("Channel Width [eV]")
  muCW <- mean(df$cw.ev.mu)
  sMuCW <- sprintf("mean: %.5f", muCW)
  dcw <- dcw + geom_hline(aes(yintercept=muCW), colour='blue', size=1)
  dcw <- dcw + annotate("text", x=2, y=dcw.lab,
                        label=sMuCW, colour='blue')
  dcw <- dcw + theme(axis.text.x = element_text(angle = 30, hjust = 1))

  # start zero offset
  dzo <- ggplot(data = df, aes(date, zo.ev.mu))
  dzo <- dzo + expand_limits(y=dzo.lim)
  dzo <- dzo + geom_errorbar(aes(ymin=zo.ev.mu-zo.ev.unc,
                                 ymax=zo.ev.mu+zo.ev.unc),
                             width=.5, size=1, position=pd)
  dzo <- dzo + geom_line(size=1)
  dzo <- dzo + geom_point(size=2)
  dzo <- dzo + xlab("Date")
  dzo <- dzo + ylab("Zero Offset [eV]")
  # dzo <- dzo + ggtitle(detName)
  muZO <- mean(df$zo.ev.mu)
  sMuZO <- sprintf("mean: %.3f", muZO)
  dzo <- dzo + geom_hline(aes(yintercept=muZO),
                          colour='blue', size=1)
  dzo <- dzo + annotate("text", x=2, y=dzo.lab,
                        label=sMuZO, colour='blue')
  dzo <- dzo + theme(axis.text.x = element_text(angle = 30, hjust = 1))

  dmn <- ggplot(data = df, aes(date, mn.res.mu))
  dmn <- dmn + expand_limits(y=dmn.lim)
  dmn <- dmn + geom_errorbar(aes(ymin=mn.res.mu-mn.res.unc,
                                 ymax=mn.res.mu+mn.res.unc),
                             width=.5, size=1, position=pd)
  dmn <- dmn + geom_line(size=1)
  dmn <- dmn + geom_point(size=2)
  dmn <- dmn + xlab("Date")
  dmn <- dmn + ylab("Resolution at MnKa [eV]")
  # dmn <- dmn + ggtitle(detName)
  muMn <- mean(df$mn.res.mu)
  sMuMn <- sprintf("mean: %.2f", muMn)
  dmn <- dmn + geom_hline(aes(yintercept=muMn),
                          colour='blue', size=1)
  dmn <- dmn + annotate("text", x=2, y=dmn.lab,
                        label=sMuMn, colour='blue')
  dmn <- dmn + theme(axis.text.x = element_text(angle = 30, hjust = 1))

  cul <- ggplot(data = df, aes(date, cu.la.cts.per.na.sec.mu))
  cul <- cul + expand_limits(y=dcul.lim)
  cul <- cul + geom_errorbar(aes(ymin=cu.la.cts.per.na.sec.mu
                                 - cu.la.cts.per.na.sec.unc,
                                 ymax=cu.la.cts.per.na.sec.mu
                                 + cu.la.cts.per.na.sec.unc),
                             width=.5, size=1, position=pd)
  cul <- cul + geom_line(size=1)
  cul <- cul + geom_point(size=2)
  cul <- cul + xlab("Date")
  cul <- cul + ylab("Cu-La cps/nA/s")
  # culacpsna <- culacpsna + ggtitle(detName)
  muCuLa <- mean(df$cu.la.cts.per.na.sec.mu)
  sMuCuLa <- sprintf("mean: %.2f", muCuLa)
  cul <- cul + geom_hline(aes(yintercept=muCuLa),
                          colour='blue', size=1)
  cul <- cul + annotate("text", x=2, y=dcul.lab,
                        label=sMuCuLa, colour='blue')
  cul <- cul + theme(axis.text.x = element_text(angle = 30, hjust = 1))


  print(dcw, vp = viewport(layout.pos.row = 2,
                           layout.pos.col = 1))
  print(dzo, vp = viewport(layout.pos.row = 2,
                           layout.pos.col = 2))
  print(dmn, vp = viewport(layout.pos.row = 3,
                           layout.pos.col = 1))
  print(cul, vp = viewport(layout.pos.row = 3,
                           layout.pos.col = 2))
  grid.text(detName, vp = viewport(layout.pos.row = 1,
                                   layout.pos.col = 1:2))

}
