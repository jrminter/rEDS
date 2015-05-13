#' SingleMSA
#'
#' Read a single spectrum in the MSA file format from a file and
#' return a Spectrum object.
#' From Jeff Davis at NIST
#'
#' @param spec A file path. Default is an empty string and opens a TK file chooser.
#'
#' @param probecur The probe current in nA. Default is 1.0
#'
#' @return A spectrum object
#'
#' @keywords keywords
#'
#' @import tcltk
#'
#' @export
#' @include Spectrum.R
SingleMSA <- function(spec="", probecur=1.0){
  if(nchar(spec) < 1){
    spec = tk_choose.files()
  }
  msaHead <- read.table(spec,comment.char="&", fill=TRUE,
                        strip.white=FALSE, sep="", stringsAsFactors=FALSE,
                        row.names=NULL)
  msaHead <- msaHead[1:which(msaHead[,1]=="#SPECTRUM"),]
  msaSpec <- read.table(spec,sep=",",comment.char="#")
  msaSpec <- if(msaHead[which(msaHead[,1]=="#NCOLUMNS"),3] == "2"){
               msaSpec[,2] } else {
               msaSpec[,1]
             }
  if (msaHead[which(msaHead[,1]=="#NCOLUMNS"),3] == "4.0"){
    .tmp<-scan(spec,comment.char="#",sep=",")
    msaSpec<-.tmp[!is.na(.tmp)]
  }
  probeCurTest<-ifelse(msaHead[,1]=="#PROBECUR",TRUE,FALSE)
  if (TRUE %in% probeCurTest) probecur=as.numeric(msaHead[which(msaHead[,1]=="#PROBECUR"),3])
  comp<-0
  elements<-"None"
  #These lines check for a DTSA II added tag identifying standard
  # compositions or quantified compositions.  The Function parses
  # the DTSA II reporting format into a data frame for later use.
  if ("##D2STDCMP" %in% msaHead[,1]){
    .tmp<-which(msaHead=="##D2STDCMP")
    trixy<-scan(spec,what="character",skip=(.tmp-1),sep="\n")[1]
    trixy<-strsplit(trixy,",")[[1]][-1]
    trixy<-sub(")","",trixy)
    trixy<-sub("\\(","",trixy)
    trixy<-strsplit(trixy,":")
    elements<-sapply(trixy,function(x){x[1]})
    comp<-sapply(trixy,function(x){x[2]})
  }
  if ("##D2QUANT" %in% msaHead[,1]){
    .tmp<-which(msaHead=="##D2QUANT")
    trixy<-scan(spec,what="character",skip=(.tmp),sep="\n")[1]
    trixy<-strsplit(trixy,",")[[1]][-1]
    trixy<-sub(")","",trixy)
    trixy<-sub("\\(","",trixy)
    trixy<-strsplit(trixy,":")
    elements<-sapply(trixy,function(x){x[1]})
    comp<-sapply(trixy,function(x){x[2]})
  }
  if ("#TITLE" %in% msaHead[,1]) {identity<-msaHead[which(msaHead[,1]=="#TITLE"),
                                                    3:length(msaHead[which(msaHead[,1]=="#TITLE"),])]}
  if ("##SPECIMEN" %in% msaHead[,1]){
    .tmp<-which(msaHead=="##SPECIMEN")
    id<-scan(spec,what="character",skip=(.tmp-1),sep="\n")[1]
    id<-strsplit(id,":")
    identity<-id[[1]][2]
  }
  myposition<-c(0,0,0)
  if ("#XPOSITION" %in% msaHead[,1]){
    .xtmp<-msaHead[which(msaHead[,1]=="#XPOSITION"),3]
    .ytmp<-msaHead[which(msaHead[,1]=="#YPOSITION"),3]
    .ztmp<-msaHead[which(msaHead[,1]=="#ZPOSITION"),3]
    myposition<-as.numeric(c(.xtmp,.ytmp,.ztmp))
  }
  msa<-new("Spectrum",
           title=paste(as.character(msaHead[which(msaHead[,1]=="#TITLE"),c(-1,-2)]),sep=".",collapse="*"),
           npoints=as.numeric(msaHead[which(msaHead[,1]=="#NPOINTS"),3]),
           ncolumns=as.numeric(msaHead[which(msaHead[,1]=="#NCOLUMNS"),3]),
           xunits=as.character(msaHead[which(msaHead[,1]=="#XUNITS"),3]),
           xperchan=as.numeric(msaHead[which(msaHead[,1]=="#XPERCHAN"),3]),
           offset=as.numeric(msaHead[which(msaHead[,1]=="#OFFSET"),3]),
           beamkv=as.numeric(msaHead[which(msaHead[,1]=="#BEAMKV"),3]),
           probecur=probecur,
           livetime=as.numeric(msaHead[which(msaHead[,1]=="#LIVETIME"),3]),
           realtime=as.numeric(msaHead[which(msaHead[,1]=="#REALTIME"),3]),
           spectrum=as.vector(msaSpec),identity=as.character(identity),
           composition=as.numeric(comp),
           elements=as.character(elements),
           position=myposition)
  return(msa)

}
