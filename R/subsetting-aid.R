

#' @title Logical naming test
#'
#' @description Returns TRUE if \code{input}'s names evaluate to truthy.
#'
#' @param input R object that is to be checked.
#'
#' @export

is_named <- function(input){

  names <- base::names(input)

  shiny::isTruthy(names)

}



#' @title Discard unnamed elements
#'
#' @description Makes sure that all elements of \code{input} are
#' named.
#'
#' @param input R object that is to be checked.
#'
#' @export

discard_unnamed <- function(input){

  input[purrr::map_lgl(.x = base::names(input), .f = ~ shiny::isTruthy(.x))]

}


#' @rdname discard_unnamed
#' @export
keep_named <- function(input){

  input[purrr::map_lgl(.x = base::names(input), .f = ~ shiny::isTruthy(.x))]

}


