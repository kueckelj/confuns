#' @title Arrange rows
#'
#' @description Arranges the rows of a data.frame according to
#' the positions of their maxima or or minima. The earlier
#' a rows maximum/minimum appears the higher the row will appear in the
#' returned data.frame.
#'
#' @param df A data.frame with at least one numeric variable.
#' @param across Character. Either \emph{'maxima} or \emph{'minima'}.
#'
#' @inherit verbose params
#'
#' @return An arranged data.frame.
#' @export

arrange_rows <- function(df, across, verbose){

  base::stopifnot(base::is.data.frame(df))
  base::stopifnot(base::any(sapply(df, base::is.numeric)))
  base::stopifnot(base::all(across == "maxima") | base::all(across == "minima"))

  if(verbose){
    base::message(glue::glue("Arranging rows according to their {across}."))
  }

  if(across == "maxima"){

    df$order <- sapply(1:nrow(df), function(i){base::which.max(df[i, sapply(df, base::is.numeric)])})
    df <- dplyr::arrange(df, order) %>% dplyr::select(-order) %>% as.data.frame()

  } else if(across == "minima") {

    df$order <- sapply(1:nrow(df), function(i){base::which.max(df[i, sapply(df, base::is.numeric)])})
    df <- dplyr::arrange(df, desc(order)) %>% dplyr::select(-order) %>% as.data.frame()

  }

  base::return(df)

}
