
#' Title
#'
#' @param df
#' @param x
#' @param y
#' @param across
#' @param across.subset
#' @param relevel
#' @param ncol
#' @param nrow
#' @param scales
#' @param space
#' @param pt_alpha
#' @param pt_clr
#' @param pt_fill
#' @param pt_shape
#' @param pt_size
#' @param smooth_clr
#' @param smooth_method
#' @param smooth_se
#'
#' @return
#' @export
#'
plot_scatterplot <- function(df,
                             x,
                             y,
                             across = NULL,
                             across.subset = NULL,
                             relevel = TRUE,
                             ncol = NULL,
                             nrow = NULL,
                             scales = "fixed",
                             space = "fixed",
                             pt_alpha = 0.9,
                             pt_clr = "black",
                             pt_fill = "black",
                             pt_shape = 21,
                             pt_size = 2,
                             smooth_clr = "blue",
                             smooth_method = NULL,
                             smooth_se = FALSE){

  check_data_frame(
    df = df,
    var.class = purrr::map(.x = c(x,y), .f = function(i){ return("numeric") }) %>% purrr::set_names(nm = c(x,y)),
    verbose = TRUE,
    with.time = FALSE
  )


  if(base::length(across) == 2){

    if(base::length(relevel == 1)){

      relevel <- base::rep(relevel, 2)

    }

    if(!base::is.null(across.subset) & !confuns::is_list(input = across.subset)){

      msg <- "If input for argument 'across' is of length two the input for argument 'across.subset' must be a named list or NULL."

      give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)

    }

    df <- check_across_subset(df = df, across = across[1], across.subset = across.subset[[across[1]]], relevel = relevel[1])

    df <- check_across_subset(df = df, across = across[2], across.subset = across.subset[[across[2]]], relevel = relevel[2])

  } else {

    df <- check_across_subset(df = df, across = across, across.subset = across.subset, relevel = relevel[1])

  }

  p <-
    ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_point(alpha = pt_alpha, color = pt_clr, fill = pt_fill, shape = pt_shape, size = pt_size) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank()
    )


  if(!base::is.null(across)){

    if(base::length(across) == 1){

      p <-
        p +
        ggplot2::facet_wrap(facets = stats::as.formula(stringr::str_c(". ~ ", across)),
                            scales = scales,
                            nrow = nrow,
                            ncol = ncol)

    } else {

      across1 <- across[1]
      across2 <- across[2]

      p <-
        p +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!rlang::sym(across1)),
          cols = ggplot2::vars(!!rlang::sym(across2)),
          scales = scales,
          space = space)

    }

  }


  if(base::is.character(smooth_method)){

    p <- p + ggplot2::geom_smooth(formula = y ~ x, method = smooth_method, se = smooth_se, color = smooth_clr)

  }

  base::return(p)

}











