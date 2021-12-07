


#' @title Valid color inputs
#'
#' @description Functions that return the valid input options
#' for arguments that take predefined color palettes or color spectra.
#'
#' @return Character vector or list of character vectors.
#' @export

validColorPalettes <- function(){

  return(colorpalettes)

}

#' @rdname validColorPalettes
validColorSpectra <- function(){

  all_color_spectra()

}

#' @export
validScaleTransformations <- function(){

  c("asn", "atanh", "boxcox", "date", "exp",
    "hms", "identity", "log", "log10", "log1p",
    "log2", "logit", "modulus", "probability",
    "probit", "pseudo_log", "reciprocal", "reverse",
    "sqrt", "time")

}

#' @export
validVarTransformations <- function(){

  base::names(transformation_fns)

}


