

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


#' @title Test key variable validity
#'
#' @description Tests if the denoted key variable identifies
#' each observation uniquely.
#'
#' @inherit argument_dummy params
#' @param df A data.frame.
#' @param key.name Character value. The name of the key variable.
#'
#' @return TRUE or FALSE
#' @export
#'

is_key_variable <- function(df, key.name, stop.if.false = FALSE){

  n_obs <- base::nrow(df)

  check_data_frame(
    df = df,
    var.class = purrr::set_names(list(c("character", "factor")), nm = key.name)
  )

  key_var <- df[[key.name]]

  if(dplyr::n_distinct(key_var) == n_obs){

    out <- TRUE

  } else {

    out <- FALSE

  }

  if(base::isFALSE(out) && base::isTRUE(stop.if.false)){

    msg <- glue::glue("Variable '{key.name}' does not identify each observation uniquely.")

    give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)

  }

  return(out)

}
