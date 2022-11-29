

#' @title Yolo
#' @return A more aesthetically pleasing version of the input.
make_pretty_dummy <- function(){}


#' @title Capitalizes all words of a string
#'
#' @description Splits the string in single words by the
#' regex specified in \code{split.by} and capitalizes every
#' resulting word which are eventually collapsed with the
#' string specified in \code{collapse.with}.
#'
#' @param string Character value.
#' @param capital.letters Logical. If set to FALSE capitalization is skipped.
#' @param split.by Character value. Specfies the regex by which the
#' words of the specified string are split.
#' @param collapse.with Character value or `NULL`. If character, specifies the string with
#' which all capitalized words of the input string are reunited to the
#' output string. (Default is an empty space.)
#'
#' @inherit make_pretty_dummy return
#'
#' @export

make_capital_letters <- function(string,
                                 split.by = "_",
                                 collapse.with = " ",
                                 capital.letters = TRUE){

  replaced_ <- stringr::str_replace_all(string, pattern = split.by, replacement = " ")

  splited_words <-
    stringr::str_split(replaced_, pattern = " ") %>%
    purrr::flatten_chr()

  capital_letters <-
    purrr::map(splited_words, .f = ~ stringr::str_extract(.x, pattern = "^.")) %>%
    purrr::flatten_chr() %>%
    base::toupper()



  string <-
    purrr::pmap(
      .l = list(splited_words, capital_letters),
      .f = ~ stringr::str_replace(.x, pattern = "^.", replacement = .y)
    ) %>%
    purrr::flatten_chr() %>%
    stringr::str_c(., collapse = collapse.with)

  base::return(string)

}


#' @title Make string aesthetically pleasing
#'
#' @description Takes input and applies the following functions to it
#'
#' \enumerate{
#'  \item{\code{make_capital_letters()}}
#' }
#'
#'
#' @param string Character value. Returns input as is if it is not a character value.
#' @param strings Character vector.
#' @param fct Factor.
#' @param make.pretty Logical. If set to FALSE the input string is returned.
#' @param ... Additional argument given to \code{make_capital_letters}.
#'
#' @inherit make_pretty_dummy return
#'
#' @export

make_pretty_name <- function(string,
                             make.pretty = TRUE,
                             ...){

  if(base::isTRUE(make.pretty) & base::is.character(string)){

    # 1. Capital letters ------------------------------------------------------

    string <-
      make_capital_letters(
        string = string,
        capital.letters = capital.letters, ...
        )

    # -----

  }

  base::return(string)

}

#' @rdname make_pretty_name
#' @export
make_pretty_names <- function(strings,
                              ...){

  purrr::map(.x = strings, .f = make_pretty_name, ...) %>%
    purrr::flatten_chr()

}

#' @rdname make_pretty_name
#' @export
make_pretty_levels <- function(fct, ...){

  old_levels <- base::levels(fct)

  pretty_levels <-
    make_pretty_names(
      strings = old_levels, ...
    )

  new_levels <-
    purrr::set_names(x = old_levels, nm = pretty_levels)

  new_fct <-
    forcats::fct_recode(.f = fct, !!!new_levels)

  base::return(new_fct)

}




#' @title Make a data.frame more aesthetically pleasing
#'
#' @inherit argument_dummy params
#' @param column.names Logical. If FALSE, column names are skipped.
#' @param group.names Logical. If FALSE, discrete variables are skipped.
#' @param make.pretty Logical. If FALSE, all functions are skipped.
#' @param ... Additional arguments given to \code{make_pretty_names}.
#'
#' @inherit make_pretty_dummy return
#'
#' @export

make_pretty_df <- function(df,
                           column.names = TRUE,
                           group.names = TRUE,
                           make.pretty = TRUE,
                           ...){

  if(base::isTRUE(make.pretty) && base::isTRUE(group.names)){

    discr_vars <- across_options(df, n.across.subset = Inf)

    df <-
      dplyr::mutate_at(df, .vars = discr_vars, .funs = base::as.factor) %>%
      dplyr::mutate_at(., .vars = discr_vars, .funs = make_pretty_levels)

  }

  if(base::isTRUE(make.pretty)  && base::isTRUE(column.names)){

    base::colnames(df) <-
      make_pretty_names(
        strings = base::colnames(df), ...
      )

  }

  base::return(df)

}


#' @title Collapse with glue::glue_collapse
#' @param string Character vector to be collapsed.
#' @export
scollapse <- function(string, sep = "', '", width = Inf, last = "' and '"){

  glue::glue_collapse(x = string, sep = sep, width = width, last = last)

}


