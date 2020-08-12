#' @title UMAP calculation
#'
#' @description Calculates and adds the umap-dimensions to the specified
#' data.frame.
#'
#' @param df A data.frame.
#' @param scale Logical. If set to TRUE all numeric variables will be scaled
#' with \code{base::scale()} before the call to \code{umap::umap()} happens.
#'
#' @return The input data.frame specified in \code{df} with two additional
#' numeric variables named \emph{umap1} and \emph{umap2}.
#'
#' @export
#'
#' @examples umap_add_layout(df = mtcars, scale = TRUE)

umap_add_layout <- function(df, scale = FALSE){

  base::stopifnot(base::is.data.frame(df))
  is_value(scale, mode = "logical", ref = "scale")

  num_df <-
    base::as.data.frame(df) %>%
    dplyr::select_if(.predicate = base::is.numeric)

  # scale
  if(base::isTRUE(scale)){

    num_mtr <-
      base::as.matrix(x = num_df) %>%
      base::scale()

  }

  # umap
  umap_obj <- umap::umap(d = num_mtr)

  # return
  dplyr::mutate(.data = df,
                umap1 = base::as.numeric(umap_obj$layout[,1]),
                umap2 = base::as.numeric(umap_obj$layout[,2])
  )


}


