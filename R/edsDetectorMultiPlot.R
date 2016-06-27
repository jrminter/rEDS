#' edsDetectorMultiPlot
#'
#' Analyze a file with measurements of detector parameters generated
#' by a custom script wrapping DTSA-II's detector calibration alien's
#' functions. Generate a 4-up plot using base graphics.
#'
#' @param csvPath The path to a csv file written by the script.
#'
#' @param detTitle The name (string) for the detector. Used as a
#' title for the plot.
#'
#' @param off.cw (0.00015) offset to extend channel width plot Y axis
#' to accomodate label.
#'
#' @param off.zo (0.20) offset to extend zero offset plot Y axis
#' to accomodate label.
#'
#' @param off.mn (0.15) offset to extend Mn-Ka halfwidth plot Y axis
#' to accomodate label.
#'
#' @param off.cula (200.) offset to extend Cu-La cts/sec/nA-secplot Y
#' axis to accomodate label.
#'
#' @param lw.bars (2) line width of the error bars for the plots. In
#' each case these bars are the uncertainty reported by the DTSA-II
#' detector calibration.
#'
#' @param len.bars (2) length of the cap on the error bars for the
#' plots.
#'
#' @return None Function only writes a plot to the active graphic
#' device.
#'
#' @keywords keywords
#'
#' @export
edsDetectorMultiPlot <- function(csvPath,
                               detTitle,
                               off.cw=0.00015,
                               off.zo=0.20,
                               off.mn=0.15,
                               off.cula=200.,
                               lw.bars=2,
                               len.bars=0.075){
  o.mfrow <- par("mfrow")
  o.mar <- par("mar")
  o.ma <- par("oma")
  par(mfrow=c(2, 2))
  par(mar=c(3.1,4.1,1.0,0.5))
  par(oma=c(0,0,2,0))
  df <- read.csv(csvPath,header=TRUE, as.is=TRUE)
  date <- df$date
  d.min <- min(date)
  d.max <- max(date)
  y.l <- df$cw.ev.mu - df$cw.ev.unc
  y.u <- df$cw.ev.mu + df$cw.ev.unc
  df$date <- as.Date(date, "%Y-%m-%d")
  lims <- data.frame(date=c(min(df$date), max(df$date)),
                     y=c(min(y.l)-off.cw, max(y.u)+off.cw))
  plot(y~date, data=lims, type='n',
       xlab='date', ylab='channel width [ev]')
  points(cw.ev.mu~date, data=df, pch=19)
  arrows(df$date, df$cw.ev.mu,
         df$date, y.l, angle=90, code=2,
         length = len.bars, lwd = lw.bars)
  arrows(df$date, df$cw.ev.mu,
         df$date, y.u, angle=90, code=2,
         length = len.bars, lwd = lw.bars)
  abline(h=mean(df$cw.ev.mu), lw=2, col='blue')
  muCW <- mean(df$cw.ev.mu)
  sMuCW <- sprintf("mean: %.5f", muCW)
  legend("topleft", sMuCW, lw=2, col='blue')

  y.l <- df$zo.ev.mu - df$zo.ev.unc
  y.u <- df$zo.ev.mu + df$zo.ev.unc
  lims <- data.frame(date=c(min(df$date), max(df$date)),
                     y=c(min(y.l)-off.zo, max(y.u)+off.zo))
  plot(y~date, data=lims, type='n',
       xlab='date', ylab='zero offset [ev]')
  points(zo.ev.mu~date, data=df, pch=19)
  arrows(df$date, df$zo.ev.mu,
         df$date, y.l, angle=90, code=2,
         length = len.bars, lwd = lw.bars)
  arrows(df$date, df$zo.ev.mu,
         df$date, y.u, angle=90, code=2,
         length = len.bars, lwd = lw.bars)
  abline(h=mean(df$zo.ev.mu), lw=2, col='blue')
  muZO <- mean(df$zo.ev.mu)
  sMuZO <- sprintf("mean: %.3f", muZO)
  legend("topleft", sMuZO, lw=2, col='blue')

  y.l <- df$mn.res.mu - df$mn.res.unc
  y.u <- df$mn.res.mu + df$mn.res.unc
  lims <- data.frame(date=c(min(df$date), max(df$date)),
                     y=c(min(y.l)-off.mn, max(y.u)+off.mn))
  plot(y~date, data=lims, type='n',
       xlab='date', ylab='FWHM at MnKa [ev]')
  points(mn.res.mu~date, data=df, pch=19)
  arrows(df$date, df$mn.res.mu,
         df$date, y.l, angle=90, code=2,
         length = len.bars, lwd = lw.bars)
  arrows(df$date, df$mn.res.mu,
         df$date, y.u, angle=90, code=2,
         length = len.bars, lwd = lw.bars)
  abline(h=mean(df$mn.res.mu), lw=2, col='blue')
  muMn <- mean(df$mn.res.mu)
  sMuMn <- sprintf("mean: %.3f", muMn)
  legend("topleft", sMuMn, lw=2, col='blue')

  y.l <- df$cu.la.cts.per.na.sec.mu - df$cu.la.cts.per.na.sec.unc
  y.u <- df$cu.la.cts.per.na.sec.mu + df$cu.la.cts.per.na.sec.unc

  lims <- data.frame(date=c(min(df$date), max(df$date)),
                     y=c(min(y.l)-off.cula, max(y.u)+off.cula))
  plot(y~date, data=lims, type='n',
       xlab='date', ylab='CuLa [cts/nA-sec]')
  points(cu.la.cts.per.na.sec.mu~date, data=df, pch=19)
  arrows(df$date, df$cu.la.cts.per.na.sec.mu,
         df$date, y.l, angle=90, code=2,
         length = len.bars, lwd = lw.bars)
  arrows(df$date, df$cu.la.cts.per.na.sec.mu,
         df$date, y.u, angle=90, code=2,
         length = len.bars, lwd = lw.bars)
  abline(h=mean(df$cu.la.cts.per.na.sec.mu), lw=2, col='blue')
  muCuLa <- mean(df$cu.la.cts.per.na.sec.mu)
  sMuCuLa <- sprintf("mean: %d.", round(muCuLa,0))
  legend("topleft", sMuCuLa, lw=2, col='blue')
  mtext(detTitle, side=3, outer = TRUE, cex = 1.25)

  par(mfrow=o.mfrow)
  par(mar=o.mar)
  par(oma=o.ma)
}
