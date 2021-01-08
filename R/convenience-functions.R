
#' @title Print top n matrix rows and columns
#'
#' @param mtr A matrix.
#' @param n Numeric value.
#'
#' @return Head of matrix.
#' @export

hm <- function(mtr, n = 6){

  n <- base::rep(n, 2)

  if(base::nrow(mtr) < n[1]){ n[1] <- base::nrow(mtr)}

  if(base::ncol(mtr) < n[2]){ n[2] <- base::ncol(mtr)}

  mtr[1:n[1], 1:n[2]]

}
