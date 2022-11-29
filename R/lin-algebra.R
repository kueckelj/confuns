
#' @title Lin alg dummy
#' @param d Number of digits to which the output values are rounded.
#' @export
lin_alg_dummy <- function(){}


#' @title Compute circle circumference
#'
#' @description Computes the circumference of a circle based on it' radius.
#'
#' @param radius Numeric vector. The radius/radii.
#' @inherit lin_alg_dummy params
#'
#' @return Numeric vector of the same length.
#' @export

circumference <- function(radius, d = Inf){

  base::round(x = (2*pi*radius), digits = d)

}

#' @title Compute circle radius
#'
#' @description Computes the radius of a circle based on the circumference
#'
#' @param circ Numeric vector. The circumference/circumferences.
#' @inherit lin_alg_dummy params
#'
#' @return Numeric vector of the same length.
#' @export
radius <- function(circ, digits = Inf){

  base::round(x = (circ / (2*pi)), digits = d)

}


#' @title Convert radian to degree
#'
#' @description Converts radians to degrees.
#'
#' @param rad Numeric vector of values in radian. Values must frange from 0 to `2*pi`.
#' @inherit lin_alg_dummy params
#'
#' @return Numeric vector of the same length.
#' @export
rad2degr <- function(rad, d = Inf){

  is_vec(x = rad, mode = "numeric")

  rad <- base::abs(rad)

  base::stopifnot(base::all(rad >= 0 & rad <= (2*pi)))

  base::round(x = (360/(pi*2)) * rad, digits = d)

}


#' @title Convert radians to degrees
#'
#' @description Converts radian
#'
#' @param degr Numeric vector in degrees. Values must range from 0 to 360.
#' @inherit lin_alg_dummy params
#'
#' @return Numeric vector of the same length.
#' @export
degr2rad <- function(degr, d = Inf){

  is_vec(x = degr, mode = "numeric")

  base::stopifnot(base::all(degr >= 0 & degr <= 360))

  base::round(x = (degr/360) * (pi*2), d = d)

}



#' @title Create trigonometric vectors
#'
#' @description Projects point within a plane to a points based on angle and distance
#' starting from one starting point.
#'
#' @param start Numeric vector of length two. First value sets x- and
#' second value sets y-coordinate of the start.
#' @param angle Angles of the trigonometric vectors.
#' @param dist Distances from center to final points.
#' @param prolong Numeric vector or `NULL`. If numeric, sets values with which
#' the projected vector is prolonged. For that, an additional set
#' of *xend* and *yend* variables is added to the output data.frame for
#' each prolonging.
#'
#' @inherit lin_alg_dummy params
#'
#' @return Data.frame with four variables *x*, *y*, *xend* and *yend*. Each
#' row corresponds to a trigonometric vector. Values of *x* and *y* are equal
#' to input for `center`. Variables *xend* and *yend* correspond to the endpoints of each
#' projection.
#'
#' @details Length of `angle` and `dist` should be equal or at least recyclable. This
#' means one of the two argument lengths should be a multiple of the other one.
#'
#' `d` only affects the variables `angle` and `dist`.
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#'
#' # argument input options -----
#'
#' # not recyclable - fails
#'
#' angle <- c(0, 90, 180)
#'
#' dist <- c(10, 20)
#'
#' trig_vecs <- make_trig_vec(start = c(0, 0), angle = angle, dist = dist)
#'
#'
#' # equal length - works
#'
#' angle <- c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300)
#'
#' dist <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)
#'
#' trig_vecs <- make_trig_vec(start = c(0, 0), angle = angle, dist = dist)
#'
#' trig_vecs
#'
#'
#' # recyclable - works
#'
#' angle <- c(0, 55, 75, 200)
#'
#' dist <- c(10)
#'
#' trig_vecs <- make_trig_vec(start = c(0, 0), angle = angle, dist = dist)
#'
#' trig_vecs
#'
#'
#' # plot results -----
#'
#' ggplot(mapping = aes(x = x, y = y)) +
#'   geom_segment(
#'     data = trig_vecs,
#'     mapping = aes(xend = xend, yend = yend),
#'     arrow = arrow()
#'   )
#'
#'
#' # store prolonged projections -----
#'
#' angle <- c(25, 135, 245, 315)
#' dist <- c(20, 22.5 , 25, 27.5)
#'
#' trig_vecs_prol <-
#'   make_trig_vec(
#'     start = c(0, 0),
#'     angle = angle,
#'     dist = dist,
#'     prolong = c(1.1, 0.5), # stores additional projections with 1.1 and 0.5 times the length
#'     d = 2 # round info vars to two digits
#'     )
#'
#' trig_vecs_prol
#'
#' ggplot(mapping = aes(x = x, y = y)) +
#'   geom_segment(
#'     data = trig_vecs_prol,
#'     mapping = aes(xend = xend, yend = yend), # segments with original projection
#'     arrow = arrow()
#'   ) +
#'    geom_text(
#'     data = trig_vecs_prol,
#'     mapping = aes(
#'       x = xend_p1,
#'       y = yend_p1,
#'       label = str_c(angle, "°; ", dist)
#'      )
#'    )
#'

make_trig_vec <- function(start, angle, dist, prolong = NULL, d = Inf){

  # check input
  is_vec(x = start, mode = "numeric", of.length = 2)

  x <- start[1]
  y <- start[2]

  is_vec(x = prolong, mode = "numeric", skip.allow = TRUE, skip.val = NULL)

  are_vectors(c("angle", "dist"), mode = "numeric")

  la <- base::length(angle)
  ld <- base::length(dist)

  if(la != ld){

    if(ld %% la == 0){

      n <- ld/la

      angle <- base::rep(angle, n)

    } else if(la %% ld == 0){

      n <- la/ld

      dist <- base::rep(dist, n)

    } else {

      stop("Input for argument `angle` and for argument `dist` must be of the same length or recyclable.")

    }

  }

  # convert to radians
  angle_rad <- degr2rad(degr = angle)

  trig_vectors_out <-
    purrr::map2_dfr(
      .x = angle_rad,
      .y = dist,
      .f = function(ar, hyp){ # hyp = hypothenuse

        #ankathete (engl. = adjacent side)
        ak <- tibble::tibble(x = x, y = y, xend = x, yend = NA)

        # length of adjacent side
        ak[["yend"]] <- base::cos(ar) * hyp

        # gegenkathete (engl. = opposite side)
        gk <- tibble::tibble(x = x, y = y, xend = NA, yend = y)

        # length of opposite side
        gk[["xend"]] <- base::sin(ar) * hyp

        out <-
          tibble::tibble(
            x = x,
            y = y,
            angle = rad2degr(ar, d = d),
            dist = hyp,
            xend = gk[["xend"]],
            yend = ak[["yend"]]

          )

        if(base::is.numeric(prolong)){

          for(i in base::seq_along(prolong)){

            p <- prolong[i]

            # length of adjacent side
            ak[["yend"]] <- base::cos(ar) * (hyp*p)

            # length of opposite side
            gk[["xend"]] <- base::sin(ar) * (hyp*p)

            out[[stringr::str_c("yend_p",i)]] <- ak[["yend"]]
            out[[stringr::str_c("xend_p",i)]] <- gk[["xend"]]

          }

        }

        return(out)

      }
    )

  return(trig_vectors_out)

}






#' @title Pythagoras theorem
#'
#' @description Computes the missing part of the variables of the theorem
#' of pythagoras *a^2 x b^2 = c^2*.
#'
#' @param a,b,c Numeric values. The one that is `NA` is computed.
#'
#' @return Numeric value.
#' @export
pythagoras_theorem <- function(a = NA, b = NA, c = NA){

  if(is.na(c)){

    c <- base::sqrt(x = (a^2 + b^2))

    return(c)

  } else if(base::is.na(b)){

    b <- base::sqrt((c^2 - a^2))

    return(b)

  } else if(base::is.na(a)){

    a <- base::sqrt((c^2 - b^2))

    return(a)

  }

}

