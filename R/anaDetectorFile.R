#' anaDetectorFile
#'
#' Analyze a file with measurements of detector parameters generated
#' by a custom script wrapping DTSA-II's detector calibration alien's
#' functions.
#'
#' @param csvPath The path to a csv file written by the script.
#'
#' @param detName The name (string) for the detector. Used as a
#' tilte for the plots.
#'
#' @param bVerbose A flag (default FALSE) to print intermediate
#' results.
#'
#' @param labOffCW An offset for the Date label for the channel width
#' plot. Defaults to 1. This is measured from the end.
#'
#' @param labOffZO An offset for the Date label for the zero offset
#' plot. Defaults to 1. This is measured from the end.
#'
#' @param labOffMn An offset for the Date label for the Mn FwHm at MnKa
#' plot. Defaults to 1. This is measured from the end.
#'
#' @param labOffCu An offset for the Date label for the Cu-La cps/nA-sec
#' plot. Defaults to 1. This is measured from the end.
#'
#'
#' @return A list of ggplot2 plots with keys 'detCW',
#' 'detZO', 'detMn', and 'cuLaPk.
#'
#' @keywords keywords
#'
#' @export
anaDetectorFile <- function(csvPath, detName, bVerbose=FALSE,
                            labOffCW=1, labOffZO=1, labOffMn=1,
                            labOffCu=1){
  library(ggplot2)
  df <- read.csv(csvPath,header=TRUE, as.is=TRUE)
  nr = nrow(df)
  date <- df$date
  df$date <- as.Date(date, "%Y-%m-%d")
  if(bVerbose){
    print(head(df))
  }
  pd <- position_dodge(0.1)
  dcw <- ggplot( data = df, aes( date, cw.ev.mu ))
  dcw <- dcw + geom_errorbar(aes(ymin=cw.ev.mu-cw.ev.unc,
                                 ymax=cw.ev.mu+cw.ev.unc),
                             width=.5, size=1, position=pd)
  dcw <- dcw + geom_line(size=1)
  dcw <- dcw + geom_point(size=2)
  dcw <- dcw + xlab("Date")
  dcw <- dcw + ylab("Channel Width [eV]")
  dcw <- dcw + ggtitle(detName)
  muCW <- mean(df$cw.ev.mu)
  sMuCW <- sprintf("mean: %.5f", muCW)
  dcw <- dcw + geom_hline(aes(yintercept=muCW),
                          colour='blue', size=1)
  dcw <- dcw + annotate("text", x=df$date[nr-labOffCW], y=muCW+0.0005,
                        label=sMuCW, colour='blue')
  # print(dcw)

  dzo <- ggplot(data = df, aes(date, zo.ev.mu))
  dzo <- dzo + geom_errorbar(aes(ymin=zo.ev.mu-zo.ev.unc,
                                 ymax=zo.ev.mu+zo.ev.unc),
                             width=.5, size=1, position=pd)
  dzo <- dzo + geom_line(size=1)
  dzo <- dzo + geom_point(size=2)
  dzo <- dzo + xlab("Date")
  dzo <- dzo + ylab("Zero Offset [eV]")
  dzo <- dzo + ggtitle(detName)
  muZO <- mean(df$zo.ev.mu)
  sMuZO <- sprintf("mean: %.3f", muZO)
  dzo <- dzo + geom_hline(aes(yintercept=muZO),
                          colour='blue', size=1)
  dzo <- dzo + annotate("text", x=df$date[nr-labOffZO], y=muZO+0.3,
                        label=sMuZO, colour='blue')
  # print(dzo)


  dmn <- ggplot(data = df, aes(date, mn.res.mu))
  dmn <- dmn + geom_errorbar(aes(ymin=mn.res.mu-mn.res.unc,
                                 ymax=mn.res.mu+mn.res.unc),
                             width=.5, size=1, position=pd)
  dmn <- dmn + geom_line(size=1)
  dmn <- dmn + geom_point(size=2)
  dmn <- dmn + xlab("Date")
  dmn <- dmn + ylab("Resolution at MnKa [eV]")
  dmn <- dmn + ggtitle(detName)
  muMn <- mean(df$mn.res.mu)
  sMuMn <- sprintf("mean: %.2f", muMn)
  dmn <- dmn + geom_hline(aes(yintercept=muMn),
                                      colour='blue', size=1)
  dmn <- dmn + annotate("text", x=df$date[nr-labOffMn], y=muMn+0.01,
                        label =sMuMn, colour='blue')
  # print(dmn)


  culacpsna <- ggplot(data = df, aes(date, cu.la.cts.per.na.sec.mu))
  culacpsna <- culacpsna + geom_errorbar(aes(ymin=cu.la.cts.per.na.sec.mu
                                             - cu.la.cts.per.na.sec.unc,
                                             ymax=cu.la.cts.per.na.sec.mu
                                             +cu.la.cts.per.na.sec.unc),
                                         width=.5, size=1, position=pd)
  culacpsna <- culacpsna + geom_line(size=1)
  culacpsna <- culacpsna + geom_point(size=2)
  culacpsna <- culacpsna + xlab("Date")
  culacpsna <- culacpsna + ylab("Cu-La cps/nA/s")
  culacpsna <- culacpsna + ggtitle(detName)
  muCuLa <- mean(df$cu.la.cts.per.na.sec.mu)
  sMuCuLa <- sprintf("mean: %.2f", muCuLa)
  culacpsna <- culacpsna + geom_hline(aes(yintercept=muCuLa),
                                          colour='blue', size=1)
  culacpsna <- culacpsna + annotate("text", x=df$date[nr-labOffCu],
                                    y=muCuLa+25, label=sMuCuLa,
                                    colour='blue')
  # print(culacpsna)

  ret <- list(detCW=dcw,
              detZO=dzo,
              detMn=dmn,
              cuLaPk=culacpsna)

  return (ret)
}
