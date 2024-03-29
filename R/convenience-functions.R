
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



#' @title Convert a numeric variable to a discrete one
#'
#' @description A wrapper around \code{dplyr::ntile()} to bin a numeric feature
#' into a discrete one.
#'
#' @param df A data.frame containing at least the numeric variable specified in \code{num_variable}.
#' @param num_variable Character value. The name of the numeric variable that you want
#' to convert.
#' @param discr_variable Character value. The name the new discrete variable wil have.
#' @param n_bins Numeric value. The number of bins you want to distribute the
#' values of \code{num_variable} to. Given to argument \code{n} of \code{dplyr::ntile()}.
#'
#' @return The data.frame specified in \code{data} with the additional discrete variable.
#' @export

bin_numeric_variable <- function(df,
                                 num_variable,
                                 discr_variable,
                                 n_bins){

  confuns::is_value(num_variable, "character", "num_variable")
  confuns::is_value(discr_variable, "character", "discr_variable")

  check_list <-
    list(c("numeric", "integer", "double")) %>%
    magrittr::set_names(value = c(num_variable))

  confuns::check_data_frame(
    df = df,
    var.class = check_list,
    ref = "data")

  replace <-
    gtools::quantcut(x = df[[num_variable]], q = n_bins) %>%
    base::levels() %>%
    stringr::str_c( base::seq_along(.), ., sep = ": ") %>%
    rlang::set_names(x = base::as.character(1:base::length(.)), nm = .)

  n_out <- base::length(replace)

  if(n_out != n_bins){

    warning(glue::glue("Output number of bins generated by `gtools:quantcut()` is {n_out}."))

  }

  df[[discr_variable]] <-
    dplyr::ntile(x = df[[num_variable]], n = n_bins) %>%
    base::factor() %>%
    forcats::fct_recode(!!!replace)

  base::return(df)

}
