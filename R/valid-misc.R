


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

  base::names(transformation_fns)

}


