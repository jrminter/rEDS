#' Class "Spectrum"
#'
#' The \code{"Spectrum"} class creates the primary object for EDS data.
#'
#' It is created by the SingleMSA function.  It assumes 20 keV
#' accelerating voltage and 1.0 nA of probe current unless
#' otherwise specified. From Jeff Davis at NIST
#'
#' @aliases Spectrum-class
#' @docType class
#' @name Spectrum-class
#' @rdname Spectrum-class
#'
#' @slot title A character string title.
#' @slot npoints An integer specifying the number of points.
#' @slot ncolumns Number of columns.
#' @slot xunits A string, typically 'eV'.
#' @slot xperchan A numeric, typically '1.0'.
#' @slot offset A numeric, zero offset.
#' @slot beamkv A numeric, beam kv.
#' @slot probecur A numeric, probe current in nA.
#' @slot livetime A numeric, live time in sec.
#' @slot realtime A numeric, real time in sec.
#' @slot spectrum A vector of data values.
#' @slot composition A numeric, composition?.
#' @slot elements An array.
#' @slot net.counts A numeric, net counts.
#' @slot identity A character, whatever.
#' @slot position A numeric, specimen position.
#'
#' @import methods
#'
#' @exportClass Spectrum
#'
#' @include Roi.R
setClass("Spectrum",
         representation(title="character", npoints="numeric",
                        ncolumns="numeric", xunits="character",
                        xperchan="numeric", offset="numeric",
                        beamkv="numeric", probecur="numeric",
                        livetime="numeric", realtime="numeric",
                        spectrum="vector", composition="numeric",
                        elements="character", net.counts="numeric",
                        identity="character", position="numeric"),
         prototype = prototype(probecur=1.0, beamkv=20.0))

