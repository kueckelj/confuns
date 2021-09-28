

#' Title
#'
#' @param df
#' @param to
#' @param verbose
#' @param with.time
#'
#' @return
#' @export
#'
rescale_df <- function(df, to = c(0.01, 1), verbose = TRUE, with.time = TRUE){

  give_feedback(msg = "Rescaling.", verbose = verbose, with.time = with.time)

  df <-
     dplyr::mutate(
       .data = df,
       dplyr::across(
         .cols = where(base::is.numeric),
         .fns = ~ scales::rescale(x = .x, to = to)
       )
     )

  give_feedback(msg = "Done.", verbose = verbose, with.time = with.time)

  return(df)

}
