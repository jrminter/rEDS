#' readDataCube
#'
#' Read in a hyperspectral data cube in the Lispix .raw/.rpl file format.
#' From Jeff Davis at NIST
#'
#' @param rplFile A file path to a Lispix .rpl file. "" defaults
#' to a file chooser.
#'
#' @param bOpenExampleSpc Whether to open an examples MSA spc. Default = FALSE
#'
#' @param exSpecFile A file path to an example DTSA-II .msa file. ""
#' to a file chooser
#'
#' @param data.what The data type. Defaults to integer.
#'
#' @param record.by.image A logical. Defaults to TRUE.
#'
#' @return A datacube object
#'
#' @import tcltk
#'
#' @keywords keywords
#'
#' @include DataCube.R
#'
#' @export
readDataCube <- function(rplFile="",
                         bOpenExampleSpc = FALSE,
                         exSpecFile="",
                         data.what="integer",
                         record.by.image=TRUE){
  if(nchar(rplFile) < 1){
    rplFile = tk_choose.files(caption="Choose RPL File")
  }

  to.read <- file(rplFile,"rb")
  rpl <- read.table(rplFile,header=TRUE)
  pixelsX <- as.integer(as.character((rpl$value[which(rpl$key=="width")])))
  pixelsY <- as.integer(as.character((rpl$value[which(rpl$key=="height")])))
  NumImages <- as.integer(as.character((rpl$value[which(rpl$key=="depth")])))
  data.length <- as.integer(as.character((rpl$value[which(rpl$key=="data-length")])))
  data.type<-as.character((rpl$value[which(rpl$key=="data-type")]))
  if (data.type=="unsigned") data.type=FALSE else data.type=TRUE
  byte.order <- as.character((rpl$value[which(rpl$key=="byte-order")]))
  if (byte.order=="big-endian") byte.order="big" else byte.order="little"
  PixelVectors <- pixelsX*pixelsY	#Number of pixels in images
  TotalValues = (pixelsX*pixelsY*NumImages)
  to.read = file(sub(".rpl",".raw",rplFile),"rb")
  Data <- readBin(to.read, what=data.what, n=TotalValues ,size=data.length,
                  endian=byte.order, signed=data.type)
  if (record.by.image==FALSE){
    mydim <- c(NumImages, pixelsX, pixelsY)
    dim(Data) <- mydim
  }
  if (record.by.image==TRUE){
    mydim <- c(pixelsX, pixelsY, NumImages)
    dim(Data) <- mydim
  }
  myexample <- new("Spectrum")
  if (bOpenExampleSpc == TRUE){
    if(nchar(exSpecFile) < 1){
      exSpecFile = tk_choose.files(caption="Choose MSA File")
    }
    myexample <- singleMSA(exSpecFile)
  }
  final <- new("DataCube",
               data=Data,
               example.spec=myexample,
               dimensions=mydim,
               rawfile=as.character(to.read),
               record.by.image=record.by.image)
  close(to.read)
  return(final)
}
