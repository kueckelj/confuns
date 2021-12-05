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
#' @description Combines all slots of the specified list in \code{lst}
#' that are values to a character/glue object.
#'
#' @param lst A named list of values.
#' @param separator Character value or NULL. Denotes the string with
#' which the slot name is combined with the slot's content. If set to
#' NULL neither the slot names nor the separators are mentioned and
#' the slot's contents are combined as they are.
#' @param prefix Character value. Denotes the string with which to prefix
#' each slots.
#'
#' @return Glue object.
#' @export
#'
#' @examples #Not run:
#'
#'  lst_input <- list("arg1" = TRUE, "arg2" = glue::glue_collapse(1:5, sep = ", "))
#'
#'  glue_list_report(lst = lst_input, separator = " = ")
#'

glue_list_report <- function(lst, prefix = "", separator = " = ", combine_via = "\n", ...){

  lst <- purrr::keep(.x = keep_named(lst), .p = function(x){

    res <- base::vector()

    res[1] <- base::length(x) == 1
    res[2] <- !base::is.list(x)

    res_final <- base::all(res)

    return(res_final)

  })

  report <- base::vector(mode = "character", length = base::length(lst))

  for(slot in base::names(lst)){

    ref_slot <- base::ifelse(base::is.character(separator), slot, "")

    report[slot] <-
      stringr::str_c(combine_via, prefix, ref_slot, separator, base::as.character(lst[[slot]]))

  }

  glue::glue_collapse(report)

}

#' @title Pull var safely
#' @export
pull_var <- function(df, var){

  if(base::is.null(var)){

    out <- NULL

  } else {

    out <- df[[var]]

  }

  return(out)

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



# v -----------------------------------------------------------------------


#' @title Obtain valid input options
#'
#' @description These functions return all valid input options for
#' specific arguments.
#'
#' @return Character vector.
#' @export
#'
validInput <- function(){

  NULL

}


