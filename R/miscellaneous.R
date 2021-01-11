#' @title Assign objects into the global environment
#'
#' @param assign Logical.
#' @param object The object to be assigned.
#' @param name The name of the assigned object.
#'

assign_obj <- function(assign, object, name){

  if(base::isTRUE(assign)){

    base::assign(
      x = name,
      value = object,
      envir = .GlobalEnv
    )

  }

}


#' @title Return function
#'
#' @param fun Character value. One of \emph{'message', 'warning', 'stop'}.
#'
#' @return The respective function
#'

error_handler <- function(fun){

  is_value(x = fun, mode = "character", ref = "fun")
  base::stopifnot(fun %in% c("message", "warning", "stop"))

  if(fun == "messsage"){

    base::message

  } else if(fun == "warning"){

    base::warning

  } else if(fun == "stop"){

    base::stop

  }

}


#' @title Glue a human readable list report
#'
#' @param lst A named list of values.
#' @param separator Character value.
#'
#' @return Glue object.
#' @export

glue_list_report <- function(lst, separator = " = "){

  lst <- purrr::keep(.x = lst, .p = ~ base::is.vector(x = .x))

  report <- base::vector(mode = "character", length = base::length(lst))

  for(slot in base::names(lst)){

    report[slot] <-
      stringr::str_c("\n", slot, separator, base::as.character(lst[[slot]]))

  }

  glue::glue_collapse(report)

}


#' @title Adapt glue reference
#'
#' @description Switch between plural or singular reference.
#'
#' @param input Vector to be checked.
#' @param sg Character value to return if length of \code{input} is 1.
#' @param pl Character value to return if length of \code{input} is > 1.
#' If set to NULL an \emph{'s'} is attached to in put of \code{sg}.
#' @param zero Character value to treturn if lengt of \code{input} is 0.
#'
#' @return Either sg or pl.
#' @export

adapt_reference <- function(input, sg, pl = NULL, zero = ""){

  if(base::length(input) == 1){

    base::return(sg)

  } else if(base::length(input) >= 1){

    if(base::is.null(pl)){

      pl <- stringr::str_c(sg, "s", sep = "")

    }

    base::return(pl)

  } else {

    base::return(zero)

  }

}


#' @title Wrapper around unfactor()
#'
#' @description If input is a factor it will be returned as
#' a character vector.
#'
#' @param input A vector.
#' @param ... Additional arguments given to s4vctrs::unfactor().
#'
#' @return
#' @export
#'

unfactor <- function(input, ...){

  if(base::is.factor(input)){

    input <- S4Vectors::unfactor(input, ...) %>%
      base::as.character()

  }

  base::return(input)

}
