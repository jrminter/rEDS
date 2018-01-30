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
#' # inF <- '../penepma/pe-spect-01.dat'
#' # ouF <- '../penepma/k411-15kV.msa'
#' # penepmaToMsa(inF, ouF, 15, 'simulation')
#'
#' @export
penepmaToMsa <- function(datFile,
                           msaFile,
                           e0,
                           title,
                           owner="Penepma",
                           bDebug=FALSE){
    df <- read.table(inF, header = FALSE, sep=" ", skip=12)
    df <- df[, c(4,6,8)]

    names(df) <- c('eV', 'pd', 'unc')

    setLoToNA <- function(x){
        if (x == 1.0e-35){
            x <- NA
        }
        x
    }

    df$pd  <- as.numeric(lapply(df$pd, setLoToNA))
    df$unc <- as.numeric(lapply(df$unc, setLoToNA))

    mv <- min(df$pd, na.rm = TRUE)
    df$pd <- df$pd/mv
    df$unc <- df$unc/mv

    mv <- min(df$pd, na.rm = TRUE)
    df$pd[is.na(df$pd)]  <- mv  # <- 1.0

    mv <- min(df$unc, na.rm = TRUE)
    df$unc[is.na(df$unc)] <- mv # 5.0

    rownames(df) <- c()
    d <- diff(df$eV)
    deltaE <- median(d)


    if(bDebug == TRUE){
        print(head(df))
        print(tail(df))
        print("ev/ch")
        print(deltaE)
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

    li <- sprintf('#XPERCHAN    : %.3f\n', deltaE)
    cat(li)

    # xo <- sprintf("#OFFSET      : %.2f\n", eStart)
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
      # li <- sprintf('%.1f, %.1f \n', eV, cts)
      li <- sprintf('%.1f \n', cts)
      cat(li)
      i = i + 1
    }
    cat('#ENDOFDATA   : \n')
    sink()
}
