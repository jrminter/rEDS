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
penepmaToMsa <- function(datFile,
                           msaFile,
                           e0,
                           title,
                           owner,
                           bDebug=FALSE){
    df <- read.table(datFile, header = FALSE, sep=" ", skip=12)
    df <- df[, c(4,6,8)]
    keV <- df[,1]/1000.
    df[,1] <- keV
    names(df) <- c('keV', 'pd', 'unc')
    df <- df[df$pd > 1.0e-35, ]
    mv <- min(df$pd)
    df$pd <- df$pd/mv
    df$unc <- df$unc/mv
    rownames(df) <- c()


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
    npts <- length(df$keV)
    li <- sprintf('#NPOINTS     : %s\n', npts-2)
    cat(li)
    cat('#NCOLUMNS    : 2\n')
    cat('#XUNITS      : eV\n')
    cat('#YUNITS      : counts\n')
    cat('#DATATYPE    : XY\n')

    eStart <- 1000.0*df$keV[1]
    eEnd <- 1000.0*df$keV[nrow(df)]
    deltaE <- (eEnd-eStart)/(as.numeric(nrow(df)))

    li <- sprintf('#XPERCHAN    : %.2f\n', deltaE)
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
    while(i < lData){
      eV <- round(1000.0*df$keV[i])
      cts <- round(df$pd[i], 1)
      li <- sprintf('%.1f, %.1f \n', eV, cts)
      cat(li)
      i = i + 1
    }
    cat('#ENDOFDATA   : \n')
    sink()
}
