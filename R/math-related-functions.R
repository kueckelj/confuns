#' @title Normalize numeric values
#'
#' @description Scales the values of the provided
#' vector to values between 0 and 1.
#'
#' @param x A numeric vector.
#'
#' @return A numeric with the same length as \code{x}.
#' @export
#'
#' @examples
#' purrr::map_df(.x = mtcars[,sapply(mtcars, is.numeric)],
#'               .f = normalize)

normalize <- function(x){

  base::stopifnot(base::is.numeric(x))

  (x-min(x))/(max(x)-min(x))

}

#' @title Zscore computation
#'
#' @description Computes zscores of the provided variable.
#'
#' @param x A numeric vector.
#'
#' @return A numeric with the same length as \code{x}.
#' @export
#'
normalize_zscore <- function(x, na.rm = TRUE){

  vec_mean <- base::mean(x, na.rm = na.rm)

  vec_sd <- stats::sd(x = x, na.rm = na.rm)

  vec_zscore <- (x - vec_mean) / vec_sd

  return(vec_zscore)

}



#' @title Curve fitting
#'
#' @description Fits a curve to a numeric vector.
#'
#' @param input Numeric vector.
#' @param fn Character value. The function to call specified as a string.
#' Currently one of: \emph{'one_peak', 'two_peaks', 'gradient', 'sinus', 'log', 'early_peak', 'late_peak'}.
#' @param rev Logical. If set to TRUE the fitted curve is returned upside-down.
#' @inherit normalize_dummy params
#'
#' @return Numeric vector.
#'
#' @details This function takes a numeric vector of length x and returns
#' a numeric vector of the same length. The values of the returned vector correspond to
#' the values needed to display or work with the pattern specified in \code{fn}.
#'
#' @examples
#'library(tidyverse)
#'library(confuns)
#'library(magrittr)
#'
#'
#'data.frame(variable = normalize(base::log(1:100))) %>%
#'  dplyr::mutate(
#'    curve = fit_curve(input = variable, fn = "sinus", normalize = TRUE),
#'    residuals = (variable - curve) %>% normalize() ,
#'    seq = row_number()) %>%
#'  tidyr::pivot_longer(
#'    cols = dplyr::all_of(x = c("curve", "residuals", "variable")),
#'    names_to = "pattern",
#'    values_to = "values") %>%
#'  ggplot(mapping = aes(x = seq, y = values)) +
#'  geom_line(mapping = aes(color = pattern))
#'
#' @export

fit_curve <- function(input, fn, rev = FALSE, normalize = TRUE){

  is_value(x = fn, mode = "character", ref = "fn")
  base::stopifnot(fn %in% c("one_peak", "two_peaks", "gradient", "log", "sinus", "linear", "early_peak", "late_peak"))

  out <-
    base::call(name = fn, input = input) %>%
    base::eval()

  if(base::isTRUE(rev)){

    out <- (out * -1)

  }

  if(base::isTRUE(normalize)){

    out <- confuns::normalize(x = out)

  }

  base::return(out)

}



#' @title Fit a curve
#'
#' @description Functions that return the numeric values of the respective
#' mathematic function they are constructed of. Length and range of the
#' return vector are equal to those of the input vector.
#'
#' @param input Numeric vector.
#'
#' @export

one_peak <- function(input){

  base::stopifnot(base::is.numeric(input))

  base::seq(1.5 * pi , 3.5 * pi, length.out = base::length(input)) %>%
    base::sin() %>% scales::rescale(to = c(min(input), max(input)))

}

#' @rdname one_peak
#' @export
two_peaks <- function(input){

  base::stopifnot(base::is.numeric(input))

  base::seq(1.5 * pi, 5.5 * pi, length.out = base::length(input)) %>%
    base::sin() %>% scales::rescale(to = c(min(input), max(input)))

}

#' @rdname one_peak
#' @export
linear <- function(input){

  base::stopifnot(base::is.numeric(input))

  base::seq(0, 1, length.out = base::length(input)) %>%
    scales::rescale(to = c(min(input), max(input)))

}

#' @rdname one_peak
#' @export
sinus <- function(input){

  base::stopifnot(base::is.numeric(input))

  base::seq(0, 2 * pi, length.out = base::length(input)) %>%
  base::sin()  %>% scales::rescale(to = c(min(input), max(input)))

}

#' @rdname one_peak
#' @export
gradient <- function(input){

  base::stopifnot(base::is.numeric(input))

  base::seq(0, 1 * pi, length.out = base::length(input)) %>%
    base::cos()  %>% scales::rescale(to = c(min(input), max(input)))
}

#' @rdname one_peak
#' @export
log <- function(input){

  base::stopifnot(base::is.numeric(input))

  base::log(x = seq_along(input)) %>%
    scales::rescale(to = c(min(input), max(input)))

}

#' @rdname one_peak
#' @export
early_peak <- function(input){

  half_length <- (base::length(input)/2)

  prel_res <-
    base::seq(1.5 * pi , 3.5 * pi, length.out = half_length) %>%
    base::sin() %>% scales::rescale(to = c(min(input), max(input)))

  res <- c(prel_res, base::rep(base::min(prel_res), half_length))

  base::return(res)

}

#' @rdname one_peak
#' @export
late_peak <- function(input){

  half_length <- (base::length(input)/2)

  prel_res <-
    base::seq(1.5 * pi , 3.5 * pi, length.out = half_length) %>%
    base::sin() %>% scales::rescale(to = c(min(input), max(input)))

  res <- c(base::rep(base::min(prel_res), half_length), prel_res)

  base::return(res)

}


