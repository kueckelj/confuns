#' @export
rgx_lookahead <- function(pattern, negate = FALSE, match = ".*"){

  if(base::isFALSE(negate)){

    out <- stringr::str_c(match, "(?=", pattern, ")", sep = "")

  } else if(base::isTRUE(negate)){

    out <- stringr::str_c(match, "(?!=", pattern = ")", sep = "")

  }

  return(out)

}

#' @export
rgx_lookbehind <- function(pattern, negate = FALSE, match = ".*"){

  if(base::isFALSE(negate)){

    out <- stringr::str_c("(?<=", pattern, ")", match, sep = "")

  } else if(base::isTRUE(negate)){

    out <- stringr::str_c("(?!<=", pattern = ")", match, sep = "")

  }

  return(out)

}


#' @title Extract string content with lookarounds
#'
#' @description Extracts content of strings that appear before or after a pattern.
#'
#' @param string Character value.
#'
#' @param pattern The regular expression that matches the part of the string
#' before/after which is looked. (Not included in the output.)
#' @param match The regular expression that is matched to the part of the string
#' before/after \code{pattern}.
#' @param negate
#' @param cut.left,cut.right Logical value. If TRUE, empty space of the remaining string
#' is removed on the left/right side. Defaults to TRUE.
#'
#' @return Character value.
#'
#' @export
str_extract_before <- function(string,
                               pattern,
                               match = "^.*",
                               negate = FALSE,
                               cut.right = TRUE,
                               cut.left = TRUE){

  out <-
    stringr::str_extract(
      string = string,
      pattern = rgx_lookahead(pattern = pattern, negate = negate, match = match)
    )

  if(base::isTRUE(cut.right)){

    out <- stringr::str_remove(out, pattern = " *$")

  }

  if(base::isTRUE(cut.left)){

    out <- stringr::str_remove(out, pattern = "^ *")

  }

  return(out)

}

#' @rdname str_extract_before
#' @export
str_extract_after <- function(string,
                              pattern,
                              match = ".*$",
                              negate = FALSE,
                              cut.right = TRUE,
                              cut.left = TRUE){

  out <-
    stringr::str_extract(
      string = string,
      pattern = rgx_lookbehind(pattern = pattern, negate = negate, match = match)
    )

  if(base::isTRUE(cut.right)){

    out <- stringr::str_remove(out, pattern = " *$")

  }

  if(base::isTRUE(cut.left)){

    out <- stringr::str_remove(out, pattern = "^ *")

  }

  return(out)

}




#' @title Obtain string suggestions
#'
#' @description Compares input string against a pool of options and returns
#' the values of the pool that are similar to the input string.
#'
#' @param string Input string. \code{str_suggest_vec()} takes a single
#' character value of length 1. \code{str_suggest_list()} takes a character
#' vector of arbitrary length.
#' @param pool Character vector that contains all valid options.
#' @param max.dist Numeric value. The maximum distance a string of the
#' pool may have to be included in the suggestions.
#' @param n.try Numeric value. The number of times the search for suggestions
#' is repeated with \code{max.dist+1}. Defaults to 0.
#' @param n.top Numeric value. The n best suggestions in terms of string
#' distance to input string that are returned.
#' @param out.fail The output value if no suggestions can be made.
#'
#' @return \code{str_suggest_vec()} returns a character vector of suggestions. Is
#' of length 0 if no suggestions can be made. \code{str_suggest_list()} returns
#' a named list of length equal to length of \code{string}. Each slot carries
#' suggestions of one element of the input for \code{string}.
#'
#' @export
#'
str_suggest_vec <- function(string,
                            pool,
                            max.dist = 1,
                            n.try = 0,
                            n.top = 5){

  is_value(x = string, mode = "character")
  is_vec(x = pool, mode = "character")

  pool <- base::unique(pool)

  if(string %in% pool){

    out <- string

  } else {

    string_distances <-
      stringdist::stringdist(a = string, b = pool) %>%
      magrittr::set_names(value = pool)

    options <- pool[string_distances <= max.dist]

    if(base::length(options) == 0 & n.try != 0){

      for(i in 1:n.try){

        max.dist <- max.dist + i

        options <- pool[string_distances <= max.dist]

        if(base::length(options) >= 1){

          break()

        }

      }

    }

    if(base::length(options) == 0){

      out <- base::character()

    } else {

      out <-
        string_distances[options] %>%
        base::sort() %>%
        base::names() %>%
        utils::head(n.top)

    }

  }

  return(out)

}


#' @rdname str_suggest_vec
#' @export
str_suggest_list <- function(string,
                             pool,
                             max.dist = 1,
                             n.try = 0,
                             n.top = 5){

  is_vec(x = string, mode = "character")
  is_vec(x = pool, mode = "character")

  string <- base::unique(string)

  purrr::map(
    .x = string,
    .f = ~ str_suggest_vec(
      string = .x,
      pool = pool,
      max.dist = max.dist,
      n.try = n.try,
      n.top = n.top
    )
  ) %>%
    purrr::set_names(nm = string)

}




