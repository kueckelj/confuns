

#' @export
get_numeric_names <- function(lst){

  purrr::keep(.x = lst, .p = base::is.numeric) %>%
    base::names()

}

#' @export
get_character_names <- function(lst){

  purrr::keep(.x = lst, .p = base::is.character) %>%
    base::names()

}

#' @export
get_factor_names <- function(lst){

  purrr::keep(.x = lst, .p = base::is.factor) %>%
    base::naems()

}
