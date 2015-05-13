#' Class "ROI"
#'
#' The "ROI" class is used to set up a region of interest for
#' simple integration and peak summing.
#'
#' It can be created by the ROI function or directly.
#'
#' @aliases ROI-class
#' @docType class
#' @name ROI-class
#' @rdname ROI-class
#'
#' @slot elements A character string of elements.
#' @slot start A vector of starting.
#' @slot end A vector of ending.
#'
#' @import methods
#'
#' @exportClass ROI
setClass("ROI",
         representation(elements="character",
                        start="vector",
                        end="vector"),
         prototype(elements=NA_character_,
                   start=NA_real_,
                   end=NA_real_))
