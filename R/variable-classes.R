#' @title Names with classes
#'
#' @description Takes either a list or a data.frame and returns
#' a named vector whereby the elements of the vector are the names
#' of the \code{data}-variables and the names of the vectors elements
#' are the class of that particular variable.
#'
#' @param data A data.frame or a list.
#'
#' @return A named character vector.
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
