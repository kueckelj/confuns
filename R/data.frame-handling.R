#' @title Arrange rows
#'
#' @description Arranges the rows of a data.frame according to
#' the positions of their variable-maximum or -minimum.
#'
#' @param df A data.frame with numeric variables that.
#' @param across Character. Either \emph{'max} or \emph{'min'}.
#'
#' @inherit verbose params
#'
#' @return An arranged data.frame.
#' @export
#'

arrange_rows <- function(df, across, verbose){

  base::stopifnot(base::is.data.frame(df))
  base::stopifnot(base::any(sapply(df, base::is.numeric)))
  base::stopifnot(base::all(across == "max") | base::all(across == "min"))

  if(verbose){
    base::message(glue::glue("Arranging rows according to their {across}."))
  }

  if(across == "max"){

    df$order <- sapply(1:nrow(df), function(i){base::which.max(df[i, sapply(df, base::is.numeric)])})
    df <- dplyr::arrange(df, order) %>% dplyr::select(-order) %>% as.data.frame()

  } else if(across == "min") {

    df$order <- sapply(1:nrow(df), function(i){base::which.max(df[i, sapply(df, base::is.numeric)])})
    df <- dplyr::arrange(df, desc(order)) %>% dplyr::select(-order) %>% as.data.frame()

  }

  base::return(df)

}
