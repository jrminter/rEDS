#' rsPlotPdf
#'
#' A convenience function to plot a function to a pdf file from
#' R studio after plotting to the screen
#' The function crops the margins with a cal to pdfcrop.
#' The function creates a new device, plots, and then restores the
#' old device.
#'
#' @param pltFn A function to generate the plot.
#'
#' @param strFile The path for the file
#'
#' @param width The width in inches, defaults to 9
#'
#' @param height The height in inches, defaults to 6
#'
#' @param pts The point size to use. Defaults to 12
#'
#'
#' @keywords keywords
#'
#' @export
rsPlotPdf <- function(pltFn, strFile, width=9, height=6, pts=12){
  Sys.sleep(0.1)
  old <- dev.cur()
  dev.next()
  pdf(file="temp.pdf", width=width, height=height, pointsize=pts,
      useDingbats=TRUE)
  pltFn
  dev.off()
  if(file.exists(strFile)){
    file.remove(strFile)
    Sys.sleep(2.0)
  }
  str.cmd <- sprintf("pdfcrop --margins 10 temp.pdf %s",
                     strFile)
  system(str.cmd)
  unlink("temp.pdf")
  dev.set(old)
}
