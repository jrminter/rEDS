#' Class "DataCube"
#' This class is used for hyperspectral x-ray data cubes.
#'
#' It contains a slot for an "example" spectrum, which can be used to
#' set the calibration and meta-data for the data cube.  Values of probe
#' current are in  nanoamps (nA), values of pixel spacing are micrometers
#' per pixel (um/pix) and dwell time is in seconds (s). From Jeff Davis at NIST.
#'
#' @aliases DataCube-class
#' @docType class
#' @name DataCube-class
#' @rdname DataCube-class
#'
#' @slot data An array of data points.
#' @slot example.spec An example spectrum (class Spectrum).
#' @slot dimensions A numeric.
#' @slot rawfile A string (path to .raw file).
#' @slot record.by.image A logical.
#' @slot manufacturer A string.
#' @slot classification A matrix.
#' @slot dwell.time A numeric, time per px in sec.
#' @slot pixel.spacing A numeric, (um/px).
#' @slot probecur A numeric (nA).
#' @slot roi An object of class ROI.
#' @slot sum.images An array.
#' @slot temp.frame A data frame.
#'
#' @import methods
#'
#' @exportClass DataCube
#' @include Roi.R
#' @include Spectrum.R
setClass("DataCube",
         representation(data="array",
                        example.spec="Spectrum",
                        dimensions="numeric",
                        rawfile="character",
                        record.by.image="logical",
                        manufacturer="character",
                        classification="matrix",
                        dwell.time="numeric",
                        pixel.spacing="numeric",
                        probecur="numeric",
                        roi="ROI",
                        sum.images="array",
                        temp.frame="data.frame"),
         prototype=prototype(probecur=1.0,
                             pixel.spacing=100,
                             dwell.time=1.0))
