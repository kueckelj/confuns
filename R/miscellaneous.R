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
#' @export
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
