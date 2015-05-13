#' Make AvgSlice and histogram plots from a matrix
#'
#' @param mat The matrix with the average slice to plot
#'
#' @param name The base name for the plot
#'
#' @param gamma The gamma level for the grayscale. Defaults to 1.25
#'
#' @param save.plots A logical to save the plots. Defaults to FALSE.
#'
#' @param pdfDir A directory for the pdf histogram. Defaults to ../pdf.
#'
#' @param pdf.pts The size for the pdf font. Defaults to 12.
#'
#' @param pngDir  A directory for the png image plot. Defaults to ../png.
#'
#' @param png.wid The width of the png. Defaults to 640.
#'
#' @param png.ht The Height of the png. Defaults to 480.
#'
#' @param png.pts Point size for the png. Defaults to 18.
#'
#' @return two plots
#'
#' @keywords keywords
#'
#' @include makeImageFromAvgSlice.R
#' @include flipMatrix.R
#' @include rotateMatrix90.R
#'
#' @export
#'
makeAverageSlicePlots <- function(mat, name, gamma=1.25,
                                save.plots=FALSE, pdfDir='../pdf', pdf.pts=12,
                                pngDir='../png', png.wid=640, png.ht=400, png.pts=18){
  makeImageFromAvgSlice(flipMatrix(rotateMatrix90(mat)),
                        gamma=gamma, title=name)
  if(save.plots){
    Sys.sleep(0.1)
    d.cur <- dev.cur()
    dev.next()
    # create the directory if it does not exist
    dir.create(pngDir, showWarnings = FALSE, recursive = TRUE)
    pngFile <- paste0(pngDir, '/', name, '.png' )
    if(file.exists(pngFile)) file.remove(pngFile)
    png(filename=pngFile, width=png.wid, height=png.ht, pointsize = png.pts)
    makeImageFromAvgSlice(flipMatrix(rotateMatrix90(mat)),
                          gamma=gamma, title=name)
    dev.off()
    dev.set(which = d.cur)
  }

  hist(mat, main=name)
  if(save.plots){
    # create the directory if it does not exist
    dir.create(pdfDir, showWarnings = FALSE, recursive = TRUE)
    pdfFile <- paste0(pdfDir, '/', name, '-hist.pdf' )
    if(file.exists(pdfFile)) file.remove(pdfFile)
    Sys.sleep(0.1)
    d.cur <- dev.cur()
    dev.copy2pdf(device=d.cur, file="temp.pdf", useDingbats=TRUE,
                 pointsize=pdf.pts)
    str.cmd <- sprintf("pdfcrop --margins 10 temp.pdf %s", pdfFile)
    system(str.cmd)
    unlink("temp.pdf")
  }
}

