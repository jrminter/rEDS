#' Convert an MCXRAY simulated spectrum file to MSA format
#'
#' Convert a spectrum simulated by MCXRAY into MSA format to
#' be read into DTSA-II (where we have markers...) and can process.
#'
#'
#' @param datFile string - the path to the 'pe-spect-##.dat'
#' file
#'
#' @param msaFile string - path fpr the output 'my-spec.msa' file.
#'
#' @param eo number - the voltage in kV for the simulation
#'
#' @param pc number - the probe current in nanoamps
#'
#' @param lt number - the live time in sec
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
#' # inF <- '../dat/20nm-C-50nm-SiO2-Si-07kV.txt'
#' # ouF <- '../msa/20nm-C-50nm-SiO2-Si-07kV.msa'
#' # mcxrayToMsa(inF, ouF, 15,  7, 1.0, 100.0,
#' #             '20nm-C-50nm-SiO2-Si-07kV', 'jrminter', TRUE)
#'
#' @export
mcxrayToMsa <- function(datFile, msaFile, e0, pc, lt,
                        title, owner, bDebug=FALSE){
  int.dig <- 4
  datFile <- gsub("\\\\", "/", datFile)
  msaFile <- gsub("\\\\", "/", msaFile)
  df <- read.table(datFile, header=FALSE, skip=2, sep="\t")
  names(df) <- c('eV', 'int')
  df$eV <- 1000.*df$eV # convert to eV
  df$eV < round(df$eV, 5)
  df$int <- round(df$int, int.dig)
  # print(head(df))
  # print(tail(df))
  # print(max(df$int))

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
  cat('#NCOLUMNS    : 2\n')
  cat('#XUNITS      : eV\n')
  cat('#YUNITS      : counts\n')
  cat('#DATATYPE    : XY\n')

  li <- sprintf('#XPERCHAN    : %.5f\n', max(df$eV)/as.numeric(npts))
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
    cts <- round(df$int[i], int.dig)
    en  <- round(df$eV[i], 5)
    li  <- sprintf('%.5f, %.4f, \n', en, cts)
    cat(li)
    i = i + 1
  }
  cat('#ENDOFDATA   : \n')
  sink()
}
