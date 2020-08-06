#' Assign objects into the global environment
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
