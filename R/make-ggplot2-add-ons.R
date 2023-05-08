
#' @title Make facet add on
#' @export
make_facet_add_on <- function(across, nrow = NULL, ncol = NULL, ...){

  if(!base::is.null(across)){

    if(base::length(across) == 1){

      out <-
        ggplot2::facet_wrap(
          facets = stats::as.formula(stringr::str_c(". ~ ", across)),
          nrow = nrow,
          ncol = ncol,
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



#' @export
make_scattermore_add_on <- function(mapping,
                                    alpha.by,
                                    color.by,
                                    pt.alpha,
                                    pt.color,
                                    pt.size,
                                    sctm.interpolate = FALSE,
                                    sctm.pixels = c(512, 512),
                                    na.rm = FALSE,
                                    ...){

  if(base::is.character(color.by) & base::is.character(alpha.by)){

    point_add_on <-
      scattermore::geom_scattermore(
        na.rm = na.rm,
        mapping = mapping,
        pointsize = pt.size,
        interpolate = sctm.interpolate,
        pixels = sctm.pixels,
        ...
      )

  } else if(base::is.character(color.by)){

    point_add_on <-
      scattermore::geom_scattermore(
        na.rm = na.rm,
        mapping = mapping,
        alpha = pt.alpha,
        pointsize = pt.size,
        interpolate = sctm.interpolate,
        pixels = sctm.pixels,
        ...
      )

  } else if(base::is.character(alpha.by)){

    point_add_on <-
      scattermore::geom_scattermore(
        na.rm = na.rm,
        mapping = mapping,
        pointsize = pt.size,
        color = pt.color,
        interpolate = sctm.interpolate,
        pixels = sctm.pixels,
        ...
      )

  } else {

    point_add_on <-
      scattermore::geom_scattermore(
        na.rm = na.rm,
        mapping = mapping,
        color = pt.color,
        alpha = pt.alpha,
        pointsize = pt.size,
        interpolate = sctm.interpolate,
        pixels = sctm.pixels,
        ...
      )

  }

  return(point_add_on )

}
