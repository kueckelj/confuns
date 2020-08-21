#' @title ggplot2 - color add on
#'
#' @description A wrapper around a variety of \code{scale_color_-() / scale_fill_-()}
#' functions.
#'
#' @param aes Character value. Either \emph{'color'} or \emph{'fill'}. Denotes the
#' used aesthetic.
#' @param variable Vector. The variable that is mapped onto the denoted aesthetic.
#' @param clrsp Character value. The color spectrum of choice if \code{variable} is
#' numeric. Run \code{confuns::all_colorspectra()}
#' to see all valid input choices.
#'
#' (Ignored if \code{variable} is categorical.)
#'
#' @param clrp Character value. The color panel of choice if \code{variable} is categorical.
#' Run \code{confuns::all_colorpanels()} to see all valid input choices.
#'
#' (Ignored if \code{variable} is numeric)
#'
#' @param ... Additional arguments given to the respective function.
#'
#'  \itemize{
#'   \item{\code{ggplot2::scale_color_viridis_c(...)}: If \code{variable} is numeric and
#'   \code{clrsp} is one of \emph{'cividis', 'viridis', 'inferno', 'magma', 'plasma'}}
#'   \item{\code{colorspace::scale_<aes>_continuous_sequential(...)}: If \code{variable} is numeric
#'   and \code{clrsp} is sequential}
#'   \item{\code{colorspace::scale_<aes>_continuous_diverging(...)}: If \code{variable} is numeric
#'   and \code{clrsp} is diverging}
#'   \item{\code{ggplot2::scale_<aes>_manual(...)}: If \code{variable} is categorical.}
#'   }
#'
#' @return An unnamed list containing the ggproto object.
#'
#' @details If the specified \code{clrp} does not contain enough colors to cover the
#' specified variable NULL is returned - which makes the call to \code{ggplot2::ggplot()}
#' use the default ggplot2 color panel.
#'
#' Make sure to use correct capitalization and white spaces denoting the color panel or -spectrum
#' of choice.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = qsec, y = wt)) +
#' geom_point(aes(color = mpg)) +
#' scale_color_add_on(aes = "color", variable = mtcars$mpg, clrsp = "Reds 3")
#'

scale_color_add_on <- function(aes = c("color", "fill"),
                               variable,
                               clrsp = NULL,
                               clrp = NULL,
                               ...){

  confuns::is_value(aes, "character", "aes")
  base::stopifnot(aes %in% c("fill", "color"))

  if(!base::is.null(clrp)){confuns::is_value(clrp, "character", "clrp")}
  if(!base::is.null(clrp)){confuns::is_value(clrsp, "character", "clrsp")}

  # ----- numeric variable
  if(base::is.numeric(variable)){

    if(clrsp %in% confuns::sequential_multi_hue |
       clrsp %in% confuns::sequential_single_hue){

      if(aes == "fill"){

        if(clrsp %in% c("inferno", "cividis", "viridis", "magma", "plasma")){

          add_on <- ggplot2::scale_fill_viridis_c(option = clrsp, ...)

        } else {

          add_on <- colorspace::scale_fill_continuous_sequential(clrsp, ...)

        }

      } else {

        if(clrsp %in% c("inferno", "cividis", "viridis", "magma", "plasma")){

          add_on <- ggplot2::scale_color_viridis_c(option = clrsp, ...)

        } else {

          add_on <- colorspace::scale_color_continuous_sequential(clrsp, ...)

        }

      }


    } else if(clrsp %in% confuns::diverging){

      if(aes == "fill"){

        add_on <- colorspace::scale_fill_continuous_diverging(clrsp, ...)

      } else {

        add_on <- colorspace::scale_color_continuous_diverging(clrsp, ...)

      }

    } else {

      base::stop("Invalid input for argument 'clrsp'.")

    }

  # ----- categorical variable
  } else {

    n <- base::unique(variable) %>% base::length()

    # 1. determine colors
    if(clrp %in% colorpanels){

      clrp_name <- clrp

      clrp <-
        stringr::str_c("clrp_", clrp, sep = "") %>%
        base::parse(text = .) %>%
        base::eval()

      l <- base::length(clrp)

    } else {

      base::stop("Invalid input for argument 'clrp'.")

    }


    # 2. check whether fill or color as aesthetic
    if(aes == "fill"){

      if(l > n){

        add_on <- ggplot2::scale_fill_manual(values = clrp, ...)

      } else {

        base::message(glue::glue("Colorclrp '{clrp_name}' contains only {l} values. Need {n}. Using default color clrp."))
        add_on <- NULL

      }

    } else {

      if(l > n){

        add_on <- ggplot2::scale_color_manual(values = clrp, ...)

      } else {

        base::message(glue::glue("Colorclrp '{clrp_name}' contains only {l} values. Need {n}. Using default color panel."))
        add_on <- NULL

      }

    }

  }

  base::return(list(add_on))

}
