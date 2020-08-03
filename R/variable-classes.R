#' @title Names with classes
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



#' @title Classes with names
#'
#' @description Get information about the inputs
#' variable names and classes.
#'
#' @param data A data.frame or a list.
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

variable_classes2 <- function(data){

  stopifnot(base::is.list(data) | base::is.data.frame(data))

  dclasses <- base::sapply(data, base::class)

  base::names(dclasses) <- base::names(data)

  base::return(dclasses)

}
