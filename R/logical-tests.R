

#' @title Check data.frame variables
#'
#' @param df A data.frame.
#'
#' @return Logical value.
#' @export

all_vars_discrete <- function(df){

  all_discrete <-
    purrr::map_lgl(.x = df, .f = function(var){

      fct <- base::is.factor(var)

      chr <- base::is.character(var)

      is_discrete <- base::any(fct, chr)

      base::return(is_discrete)

    })

  res <- base::all(all_discrete)

  base::return(res)

}

#' @rdname all_vars_numeric
#' @export
all_vars_numeric <- function(df){

  all_numeric <- purrr::map_lgl(.x = df, .f = base::is.numeric)

  res <- base::all(all_numeric)

  base::return(res)

}
