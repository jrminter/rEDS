#' Convert a PENEPMA spectrum file to MSA format
#'
#' Convert a spectrum simulated by PENEPMA into MSA format to
#' be read into DTSA-II (where we have markers...)
#'
#'
#' @param datFile string - the path to the 'pe-spect-##.dat'
#' file
#'
#' @param msaFile string - path fpr the output 'my-spec.msa' file.
#'
#' @param eo number - the voltage in kV for the simulation
#'
#' @param title string - label for the spectrum
#'
#' @param owner string - label for the scientist
#'
#' @param bDebug boolean - default FALSE. If TRUE, it prints headers
#' for diagnostics. Otherwise the function is silent.
#'
#' @return None
#'
#' It silently writes the .msa file
#'
#' @keywords keywords
#'
#' @examples
#' # Not run
#' # inF <- '../penepma/pe-spect-01.dat'
#' # ouF <- '../penepma/k411-15kV.msa'
#' # penepmaToMsa(inF, ouF, 15, 'simulation', 'jrminter')
#'
#' @export
penepmaToMsa <- function(datFile, msaFile, e0, title, owner, bDebug=FALSE){
  df <- read.table(datFile, header=FALSE, skip=12, sep=" ")
  df <- df[, -c(1,2,3,5,7)]
  names(df) <- c('eV', 'intens', 'unc')
  # the zero-offset is the first eV value
  df$eV <-round(df$eV, 0)
  zo <- df$eV[1]
  df$intens <- round(1e12*df$intens, 1)
  df$unc <- round(1e12*df$unc, 1)

  df$intens <- 0.001*df$intens
  df$unc <- 0.001*df$unc


  if(bDebug == TRUE){
    print(head(df))
    print(tail(df))
  }



  sink(msaFile)
  cat('#FORMAT      : EMSA/MAS Spectral Data File\n')
  cat('#VERSION     : 1.0\n')
  li <- sprintf('#TITLE       : %s\n', title)
  cat(li)
  today <- Sys.Date()
  today <- format(today, format="%d-%b-%Y")
  li <- sprintf('#DATE        : %s\n', today)
  cat(li)
  ti <- Sys.time()
  ti <- format(ti, "%H:%M:%S")
  li <- sprintf('#TIME        : %s\n', ti)
  cat(li)
  li <- sprintf('#OWNER       : %s\n', owner)
  cat(li)
  npts <- length(df$eV)
  li <- sprintf('#NPOINTS     : %s\n', npts)
  cat(li)
  cat('#NCOLUMNS    : 1\n')
  cat('#XUNITS      : eV\n')
  cat('#YUNITS      : counts\n')
  cat('#DATATYPE    : Y\n')

  li <- sprintf('#XPERCHAN    : %.2f\n', max(df$eV)/as.numeric(npts))
  cat(li)

  # xo <- sprintf("#OFFSET      : %.2f\n", 0.0)
  # cat(xo)
  cat("#OFFSET      : 0.0\n")

  ev <- sprintf('#BEAMKV      : %g\n', e0)
  cat(ev)

  cat('#XLABEL      : Energy [eV]\n')
  cat('#YLABEL      : Counts\n')
  cat('#SPECTRUM    : \n')

  lData <- nrow(df)

  i <- 1
  while(i < (lData-1)){
    cts <- round(df$intens[i], 4)
    li <- sprintf('%.4f, \n', cts)
    cat(li)
    i = i + 1
  }
  cat('#ENDOFDATA   : \n')
  sink()
}
