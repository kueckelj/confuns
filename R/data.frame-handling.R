#' @title Arrange rows
#'
#' @description Arranges the rows of a data.frame according to
#' the positions of their maxima or or minima. The earlier
#' a rows maximum/minimum appears the earlier the row will appear in the
#' returned data.frame.
#'
#' @param df A data.frame with at least one numeric variable.
#' @param according.to Character. Either \emph{'maxima} or \emph{'minima'}.
#'
#' @inherit verbose params
#'
#' @return The arranged data.frame.
#' @export

arrange_rows <- function(df, according.to, verbose){

  base::stopifnot(base::is.data.frame(df))
  base::stopifnot(base::any(base::sapply(df, base::is.numeric)))

  give_feedback(
    msg = glue::glue("Arranging rows according to their {according.to}."),
    verbose = verbose
    )

  if(according.to == "maxima"){

    df$order <- sapply(1:nrow(df), function(i){base::which.max(df[i, base::sapply(df, base::is.numeric)])})
    df <- dplyr::arrange(df, order) %>% dplyr::select(-order) %>% base::as.data.frame()

  } else if(according.to == "minima") {

    df$order <- base::sapply(1:nrow(df), function(i){base::which.max(df[i, base::sapply(df, base::is.numeric)])})
    df <- dplyr::arrange(df, desc(order)) %>% dplyr::select(-order) %>% base::as.data.frame()

  }

  base::return(df)

}



#' @title Across input
#'
#' @description Returns input options for the \code{across} argument.
#'
#' @param df A data.frame
#' @param n.across.subset Numeric value. Denotes the maximal number of unique
#' groups the across options to be returned may contain.
#'

across_options <- function(df, n.across.subset = 25){

  base::stopifnot(base::is.data.frame(df))
  is_value(x = n.across.subset, mode = "numeric")

  numeric_names <-
    dplyr::select_if(df, .predicate = base::is.numeric) %>%
    base::colnames()

  all_across_options <-
    dplyr::select(df, -dplyr::all_of(x = numeric_names)) %>%
    base::colnames()

  if(base::length(all_across_options) == 0){ base::stop("No discrete variables in input for 'df'.") }

  valid_across_options <-
    purrr::map(.x = dplyr::select(df, dplyr::all_of(all_across_options)), .f = dplyr::n_distinct) %>%
    purrr::keep(.p = ~ .x <= n.across.subset) %>%
    base::names()

  if(base::length(valid_across_options) == 0){

    base::stop(glue::glue("No discrete variable in input for 'df' has a lower number of unique values than the defined minimum: {n.across.subset}."))

    }


  base::return(valid_across_options)

}

across_subset_options <- function(df, across){

  base::stopifnot(base::is.data.frame(df))
  is_value(x = across, mode = "character")

  across_var <- dplyr::pull(df, var = across)

  if(base::is.factor(across_var)){

    res <- base::levels(across_var)

  } else if(base::is.character(across_var)){

    res <- base::unique(across_var)

  } else {

    base::stop(glue::glue("Variable '{across}' must be of type 'character' or 'factor'."))

  }

  base::return(res)

}



