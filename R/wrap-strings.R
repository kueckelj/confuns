

#' @title Regex for standalone detection of words
#' @export
wrap_standalone <- function(string, do = TRUE){

  if(base::isTRUE(do)){

    x <- "[^A-Za-z0-9]" # letters or numbers

    string <- stringr::str_c(x, string, x)

  }

  return(string)

}

#' @title Wrap all elements of a character vector
#'
#' @param input An object that contains character strings.
#' @param wrap.in A character vector of length one or two.
#'
#' If of length one: Pre- and Suffix of the results will be the same.
#'
#' If of length two: \code{wrap.in[1]} will be the prefix. and \code{wrap.in[1]}
#' the suffix.
#'
#' @return The provided R-object. All elements of the provided character vector
#' are wrapped with a pre- and suffix. If \code{input} is not of type vector the
#' return value from \code{base::names(\code{input})} will be wrapped accordingly.
#'
#' @export
#'


wrap_strings <- function(input, wrap.in = character()){

  if(!base::is.vector(wrap.in)){

    stop("Argument 'wrap.in' needs to be of class 'vector'.")

  }

  if(!base::length(wrap.in) %in% c(1, 2)){

    base::stop("Argument 'wrap.in' must be of lengtht 1 or 2.")

  } else if(base::length(wrap.in) == 1){

    wrap.in <- base::rep(x = wrap.in, 2)

  }

  if(base::is.vector(input) && !base::is.list(input)){

    stringr::str_c(wrap.in[1], input, wrap.in[2], sep = "")

  } else {

    if(base::is.null(base::names(input))){

      base::message("Input needs to have names if it is not of class vector. Returning unprocessed input.")

    } else {

      base::names(input) <-
        stringr::str_c(wrap.in[1], base::names(input), wrap.in[2], sep = "")

    }

    base::return(input)

  }

}



