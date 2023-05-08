#' @title ggplot2 - color add on
#'
#' @description A wrapper around a variety of \code{scale_color_-() / scale_fill_-()}
#' functions.
#'
#' @param aes Character value. Either \emph{'color'} or \emph{'fill'}. Denotes the
#' used aesthetic.
#' @param variable The variable that is mapped onto the denoted aesthetic or a character
#' value - one of \emph{'numeric', 'discrete'}.
#' @param clrsp Character value. The color spectrum of choice. Run \code{confuns::all_color_spectra()}
#' to see all valid input choices.
#'
#' (Ignored if \code{variable} is discrete)
#'
#' @param clrp Character value. The color palette of choice.
#' Run \code{confuns::all_color_palettes()} to see all valid input choices.
#'
#' (Ignored if \code{variable} is numeric)
#'
#' @inherit color_vector params
#' @param color.trans Character value. If the variable displayed by color is continuous (numeric)
#' \code{color.trans} is given to argument \code{trans} of the respective \code{scale_<aes>_*()}
#' function.
#'
#' @param ... Additional arguments given to the respective function.
#'
#'  \itemize{
#'   \item{\code{ggplot2::scale_<aes>_viridis_c(...)}: If \code{variable} is numeric and
#'   \code{clrsp} is one of \emph{'cividis', 'viridis', 'inferno', 'magma', 'plasma'}.}
#'   \item{\code{colorspace::scale_<aes>_continuous_sequential(...)}: If \code{variable} is numeric
#'   and \code{clrsp} is sequential.}
#'   \item{\code{colorspace::scale_<aes>_continuous_diverging(...)}: If \code{variable} is numeric
#'   and \code{clrsp} is diverging.}
#'   \item{\code{ggplot2::scale_<aes>_viridis_d(...)}: If \code{variable} is numeric
#'   and \code{clrp} is one of \emph{'cividis', 'viridis', 'inferno', 'magma', 'plasma'}.}
#'   \item{\code{ggplot2::scale_<aes>_greyscale(...)}: If \code{variable} is discrete
#'   and \code{clrp} is set to \emph{'greyscale'}.}
#'   \item{\code{ggplot2:.scale_<aes>_discrete()}: If \code{variable} is discrete
#'   and \code{clrp} is set to \emph{'default'}.}
#'   \item{\code{ggplot2::scale_<aes>_manual(...)}: If \code{variable} is discrete.}
#'   }
#'
#' @return An unnamed list containing the ggproto object.
#'
#' @details If the specified \code{clrp} does not contain enough colors to cover the
#' specified variable NULL is returned - which makes the call to \code{ggplot2::ggplot()}
#' use the default ggplot2 color panel.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = qsec, y = wt)) +
#' geom_point(aes(color = mpg)) +
#' scale_color_add_on(aes = "color", clrsp = "Reds 3")
#'

scale_color_add_on <- function(aes = "color",
                               variable = NULL,
                               clrsp = NULL,
                               clrp = NULL,
                               clrp.adjust = NULL,
                               color.trans = "identity",
                               ...){

  is_value(aes, "character", "aes")
  base::stopifnot(aes %in% c("fill", "color", "colour"))

  if(base::is.null(variable) & base::is.null(clrp) & !base::is.null(clrsp)){

    variable <- "numeric"

  } else if(base::is.null(variable) & !base::is.null(clrp) & base::is.null(clrsp)){

    variable <- "grouping"

  }

  if(!base::is.null(clrp)){confuns::is_value(clrp, "character", "clrp")}
  if(!base::is.null(clrsp)){confuns::is_value(clrsp, "character", "clrsp")}

  if(base::all(base::is.na(variable))){

    var_class <- base::class(variable)

    if(!var_class %in% c("character", "numeric")){

      return(NULL)

    } else {

      variable <- var_class

    }

  }


  # ----- numeric variable
  if(base::is.numeric(variable) | base::all(variable == "numeric")){

    if(clrsp %in% confuns::sequential_multi_hue |
       clrsp %in% confuns::sequential_single_hue){

      if(aes == "fill"){

        if(clrsp %in% viridis_options){

          add_on <- ggplot2::scale_fill_viridis_c(option = clrsp, trans = color.trans, ...)

        } else {

          add_on <- colorspace::scale_fill_continuous_sequential(clrsp, trans = color.trans, ...)

        }

      } else { # aes == color|colour

        if(clrsp %in% viridis_options){

          add_on <- ggplot2::scale_color_viridis_c(option = clrsp, trans = color.trans, ...)

        } else {

          add_on <- colorspace::scale_color_continuous_sequential(clrsp, trans = color.trans, ...)

        }

      }

    } else if(clrsp %in% confuns::diverging){

      if(aes == "fill"){

        add_on <- colorspace::scale_fill_continuous_diverging(clrsp, trans = color.trans, ...)

      } else {

        add_on <- colorspace::scale_color_continuous_diverging(clrsp, trans = color.trans, ...)

      }

    } else {

      stop("Invalid input for argument 'clrsp'.")

    }

  # ----- discrete variable
  } else if(!base::is.numeric(variable) | base::all(variable == "grouping")){

    n_groups <- dplyr::n_distinct(variable)
    n_colors <- -Inf

    # if argument 'variable' is a factor or the variable itself make sure to name the colors
    if(base::is.factor(variable)){

      names <- base::levels(variable)

    } else if(base::length(variable) > 1){

      names <- base::unique(variable)

    } else {

      names <- NULL

    }

    check_one_of(
      input = clrp,
      against = all_color_palettes_vec(),
      suggest = TRUE
    )

    color_vec <- color_vector(clrp = clrp, names = names, clrp.adjust = clrp.adjust)

    # 2. check whether fill or color as aesthetic
    if(aes == "fill"){

      add_on <- ggplot2::scale_fill_manual(values = color_vec, ...)

    } else {

      add_on <- ggplot2::scale_color_manual(values = color_vec, ...)

    }

  } else {

    base::message("Invalid input for argument 'variable' returning NULL.")
    add_on <- NULL

  }

  return(list(add_on))

}
