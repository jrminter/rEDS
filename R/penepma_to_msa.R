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
#' @param e0 number - the voltage in kV for the simulation
#'
#' @param title string - label for the spectrum (default "Penepma")
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
#' # inF <- system.file("extdata", "pe-spect-01.dat", package = "rEDS")
#' # ouF <- './penepma/k411-15kV.msa'
#' # penepma_to_msa(inF, ouF, 15, 'simulation')
#'
#' @export
penepma_to_msa <- function(datFile,
                           msaFile,
                           e0,
                           title,
                           owner="Penepma",
                           bDebug=FALSE){

  ev_per_ch <- penepma_get_ev_per_ch(datFile)
  df <- penepma_read_raw_data(datFile)

  if(bDebug == TRUE){
    print(head(df))
    print(tail(df))
    print("ev/ch")
    print(ev_per_ch)
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
  npts <- length(df$keV)
  li <- sprintf('#NPOINTS     : %s\n', npts)
  cat(li)
  cat('#NCOLUMNS    : 1\n')
  cat('#XUNITS      : keV\n')
  cat('#YUNITS      : counts\n')
  cat('#DATATYPE    : Y\n')

  li <- sprintf('#XPERCHAN    : %.7f\n', ev_per_ch/1000.)
  cat(li)

  cat("#OFFSET      : 0.0\n")
  ev <- sprintf('#BEAMKV      : %g\n', e0)
  cat(ev)
  cat('#XLABEL      : Energy [keV]\n')
  cat('#YLABEL      : Counts\n')
  cat('#SPECTRUM    : \n')

  lData <- nrow(df)
  i <- 1
  while(i < lData){
    keV <- round(df$keV[i], 5)
    cts <- round(100.0*df$mu[i], 2)
    # li <- sprintf('%.1f, %.1f \n', eV, cts)
    li <- sprintf('%.2f \n', cts)
    cat(li)
    i = i + 1
  }
  cat('#ENDOFDATA   : \n')
  sink()
}
