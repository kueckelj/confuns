
#' Deprecated
#'
#' @description \code{all_colorpanels()} is deprecated due to naming issues. Use
#' \code{all_color_palettes()} instead.
#'
#' @return
#' @export
#'
all_colorpanels <- function(){ # deprecated due to naming issues -> use 'all_color_palettes()'

  list("science" = colorpanels)

}

#' Deprecated
#'
#' \code{all_colorspectra()} is deprecated due to naming issues. Use
#' \code{all_color_spectra()} instead.
#'
#' @return
#' @export
#'
all_colorspectra <- function(){

  list(
    "Diverging" = diverging,
    "Sequential single hue" = sequential_single_hue,
    "Sequential multi hue" = sequential_multi_hue
  ) %>% purrr::map(.f = function(i){base::sort(i)})

}


#' @rdname colorpalettes
#' @export
colorpanels <- c("milo", "jco", "npg", "aaas", "nejm", "lo", "jama", "uc") # deprecated due to naming issues -> use 'colorpalettes'

