


#' @title S4 Default Assignment
#'
#' @description Extracts the default list from the respective slot
#' and assigns the content of the list's slot to the environment of
#' the calling function if the argument input was \code{NULL}.
#'
#' @param object The S4 object that has a slot @@default which is
#' a named list.
#' @param default_slot Character value. The name of the slot that contains
#' the default list.
#'
#' @return
#' @export
#'
assignDefault <- function(object, default_slot = "default"){

  ce <- rlang::caller_env()

  default_list <- methods::slot(object, name = default_slot)

  default_args <- base::names()

  cfn <- rlang::caller_fn()

  # get arguments from calling function
  cargs <- rlang::fn_fmls_names(fn = cfn)

  cargs <- cargs[!cargs == "..."]

  # keep arguments from calling function
  default_args <- cargs[cargs %in% default_args]

  # assign default argument values if input was set to NULL
  for(arg in default_args){

    arg_value <-
      base::parse(text = arg) %>%
      base::eval(envir = ce)

    if(base::is.null(arg_value)){

      arg_value <- default_list[[arg]]

      if(!base::is.null(arg_value)){

        base::assign(
          x = arg,
          value = arg_value,
          envir = ce
        )

      }

    }

  }

}
