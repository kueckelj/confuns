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



#' @title Trigonometric & mathematic curve
#'
#' @description Fits a curve to the input vector.
#'
#' @param input Numeric vector.
#' @param fn Character value. The \code{confuns::function()} to call.
#' Currently one of: \emph{'one_peak', 'two_peaks', 'gradient', 'log'.}
#' @param rev Logical. If set to TRUE the fitted curve is returned upside-down.
#' @inherit normalize_dummy params
#'
#' @return A numeric vector of equal length to the input containing the
#' numeric values of the 'fitted model'.
#'
#' @export

fit_model <- function(input, fn, rev = FALSE, normalize = TRUE){

  base::stopifnot(base::length(f) == 1 ||
                    f %in% c("one_peak", "two_peaks", "gradient", "log"))

  fn <- stringr::str_c("confuns::", fn, sep = "")

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
#' mathematic function they are constructed of.
#'
#' @param input Numeric vector
#'

one_peak <- function(input){

  base::stopifnot(base::is.numeric(input))

  base::seq(1.5 * pi , 3.5 * pi, length.out = base::length(input)) %>%
    base::sin()

}

#' @rdname one_peak
two_peaks <- function(input){

  base::stopifnot(base::is.numeric(input))

  base::seq(1.5 * pi, 5.5 * pi, length.out = base::length(input)) %>%
    base::sin()

}

#' @rdname one_peak
gradient <- function(input){

  base::stopifnot(base::is.numeric(input))

  base::seq(0, 1 * pi, length.out = base::length(input)) %>%
    base::cos()
}

#' @rdname one_peak
log <- function(input){

  base::stopifnot(base::is.numeric(input))

  base::log(x = seq_along(input))

}


