#' @title Make character values aesthethically pleasing
#'
#' @param string Character value to process.
#' @param pretty.names Logical value.
#'
#' @return
#' @export

make_pretty_names <- function(string, make.pretty = TRUE){


  if(base::isTRUE(make.pretty)){

    is_value(x = string, mode = "character")

    replaced_ <- stringr::str_replace_all(string, pattern = "_", replacement = " ")

    split_by_words <-
      stringr::str_split(replaced_, pattern = " ") %>%
      purrr::flatten_chr()

    capital_letters <-
      purrr::map(split_by_words, .f = ~ stringr::str_extract(.x, pattern = "^.")) %>%
      purrr::flatten_chr() %>%
      base::toupper()

    pretty_names <-
      purrr::pmap(
        .l = list(split_by_words, capital_letters),
        .f = ~ stringr::str_replace(.x, pattern = "^.", replacement = .y)
      ) %>%
      purrr::flatten_chr() %>%
      stringr::str_c(., collapse = " ")

    base::return(pretty_names)

  } else {

    base::return(string)

  }


}
