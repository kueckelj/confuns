
#' @title This is a text dummy
#'
#' @description A member of the \code{lazy-check_*()}-family.
#'
#' @details Members of the \code{lazy-check_*()}-family take the arguments
#' of their kind - that are used in the function they are called in - and
#' checks whether these arguments input fit the requirements. They stop and return an
#' error immediately once they stumble upon something invalid. They do not alter or adjust input
#' and return TRUE if the whole function has been executed without anything
#' invalid being found.
#'
#' @return A logical value TRUE if nothing invalid has been detected or an informative
#' error message.

lazy_check_dummy <- function(){}


# is - functions ----------------------------------------------------------

#' @title One dimensional input check
#'
#' @description Checks if input fits the requirements and stops the
#' function if not.
#'
#' @param x Input vector.
#' @param mode Character value. The type of which the input must be.
#' @param ref Character value. Input reference for the error message.
#'
#' @return An invisible TRUE or an informative error message.
#' @export
#'

is_value <- function(x, mode, ref = "x"){

  if(!base::length(x) == 1 ||
     !base::is.vector(x, mode = mode)){

    base::stop(glue::glue("Input '{ref}' must be a {mode} value."))

  }

  base::invisible(TRUE)

}

#' @rdname is_value
#' @export
is_vec <- function(x, mode, ref = "x"){

  if(!base::is.vector(x, mode = mode)){

    base::stop(glue::glue("Input '{ref}' must be a {mode} vector."))

  }

  base::invisible(TRUE)

}



# -----


# lazy check --------------------------------------------------------------

#' @title Check assign input
#'
#' @param assign Logical. If set to TRUE a named list will be assigned to the global
#' environment. This list contains data and information to rebuild or additionally
#' customize the output plot of this function.
#' @param assign_name The name the assigned list is supposed to have specified as
#' a single character value.
#'
#' @inherit lazy_check_dummy description details return
#' @export

check_assign <- function(assign = FALSE,
                         assign_name = character(1)){


  if(!base::is.logical(assign)){

    base::stop("Argument 'assign' needs to be logical.")

  }

  if(base::isTRUE(assign)){

    if(!base::is.character(assign_name) | !base::length(assign_name) == 1){

      base::stop("Argument 'assign_name' needs to be a single character value.")

    }

    if(assign_name == ""){

      base::stop("Argument 'assign_name' must not be ''.")

    }

    if(base::exists(x = assign_name, where = .GlobalEnv)){

      base::stop(stringr::str_c("It already exists an object named '",
                                assign_name, "' in the global environment.",
                                sep = ""))

    }


  }

  base::return(TRUE)

}



# -----


