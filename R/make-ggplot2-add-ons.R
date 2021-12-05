
#' @title Make facet add on
#' @export
make_facet_add_on <- function(across, ...){

  if(!base::is.null(across)){

    if(base::length(across) == 1){

      out <-
        ggplot2::facet_wrap(
          facets = stats::as.formula(stringr::str_c(". ~ ", across)),
          ...
        )

    } else {

      across1 <- across[1]
      across2 <- across[2]

      out <-
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!rlang::sym(across1)),
          cols = ggplot2::vars(!!rlang::sym(across2)),
          ...
        )

    }

  } else {

    out <- NULL

  }

  return(out)

}
