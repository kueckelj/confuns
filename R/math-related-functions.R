#' @title Normalize numeric values
#'
#' @description Scales the values of the provided
#' vector to values between 0 and 1.
#'
#' @param x A numeric vector.
#'
#' @return A numeric with the same length as \code{x}.
#' @export
#'
#' @examples
#' purrr::map_df(.x = mtcars[,sapply(mtcars, is.numeric)],
#'               .f = normalize)

normalize <- function(x){

  base::stopifnot(base::is.numeric(x))

  (x-min(x))/(max(x)-min(x))

}
