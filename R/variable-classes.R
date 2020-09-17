#' @title Obtain names with classes
#'
#' @description Get information about the inputs
#' variable names and classes.
#'
#' @param data A data.frame or a list.
#'
#' @return  A named character vector.
#' \itemize{
#'  \item{ \strong{Vector-elements:} The name of every variable of \code{data}}.
#'  \item{ \strong{Vector-names:} The class of every variable of \code{data}}.
#'  }
#'
#' @export
#'
#' @examples variable_classes(data = mtcars)
#'

variable_classes <- function(data){

  stopifnot(base::is.list(data) | base::is.data.frame(data))

  dnames <- base::names(data)

  base::names(dnames) <- base::sapply(data, base::class)

  base::return(dnames)

}



#' @title Obtain classes with names
#'
#' @description Get information about the inputs
#' variable names and classes.
#'
#' @param data A data.frame or a list.
#' @param simplify Logical. If set to TRUE the return value is turned into
#' a character vector.
#'
#' @return  A named character vector.
#' \itemize{
#'  \item{ \strong{Vector-elements:} The class of every variable of \code{data}}.
#'  \item{ \strong{Vector-names:} The name of every variable of \code{data}}.
#'  }
#'
#' @export
#'
#' @examples variable_classes2(data = mtcars)
#'

variable_classes2 <- function(data, simplify = TRUE){

  stopifnot(base::is.list(data) | base::is.data.frame(data))

  d_classes <- base::sapply(data, base::class)

  base::names(d_classes) <- base::names(data)

  if(base::is.list(d_classes) & base::isTRUE(simplify)){

    d_classes <- base::sapply(X = d_classes,
                              FUN = stringr::str_c,
                              collapse = "/")

  }

  base::return(d_classes)

}
