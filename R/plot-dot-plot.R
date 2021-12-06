
#' @title Plot 1d dot plot
#'
#' @description Plots a dot plot of one categorical variables. Numeric
#' values can be displayed via alpha, color, size and x-axis.
#'
#' @param x Character value. The numeric continuous varable.
#' @param y Charcter value. The grouping variable.
#' @inherit argument_dummy params
#'
#' @details A ggplot.
#'
#' @export

plot_dot_plot_1d <- function(df,
                             x,
                             y,
                             across = NULL,
                             across.subset = NULL,
                             relevel = TRUE,
                             reorder = TRUE,
                             reorder.rev = FALSE,
                             alpha.by = NULL,
                             alpha.trans = "identity",
                             color.by = NULL,
                             shape.by = NULL,
                             size.by = NULL,
                             size.trans = "reverse",
                             pt.alpha = 0.9,
                             pt.color = "black",
                             pt.clrp = "default",
                             pt.clrsp = "plasma",
                             pt.shape = 19,
                             pt.size = 3,
                             scales = "free_y",
                             nrow = NULL,
                             ncol = NULL,
                             ...){

  df <-
    check_across_subset2(
      df = df,
      across = across,
      across.subset = across.subset,
      relevel = relevel
    )

  if(base::isTRUE(reorder)){

    if(base::isTRUE(reorder.rev)){

      df <- dplyr::mutate(df, reorder_var = -!!rlang::sym(x))

    } else {

      df <- dplyr::mutate(df, reorder_var = !!rlang::sym(x))

    }

    df <-
      dplyr::group_by(df, !!rlang::sym(across)) %>%
      dplyr::mutate(
        {{y}} := tidytext::reorder_within(
          x = !!rlang::sym(y),
          by = reorder_var,
          within = !!rlang::sym(across)
        )
      )

    reorder_add_on <- tidytext::scale_y_reordered()

  } else {

    reorder_add_on <- NULL

  }

  facet_add_on <-
    make_facet_add_on(across = across, scales = scales, nrow = nrow, ncol = ncol)

  params <- adjust_ggplot_params(params = list(size = pt.size, color = pt.color, alpha = pt.alpha), sep = ".")

  ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::layer(
      geom = "point",
      stat = "identity",
      position = "identity",
      mapping = ggplot2::aes_string(alpha = alpha.by, color = color.by, size = size.by),
      params = params
    ) +
    ggplot2::scale_alpha(trans = alpha.trans) +
    ggplot2::scale_size(trans = size.trans) +
    scale_color_add_on(
      aes = "color",
      variable = pull_var(df, color.by),
      clrp = pt.clrp,
      clrsp = pt.clrsp,
      ...) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = NULL, y = NULL) +
    facet_add_on +
    reorder_add_on

}


#' @title Plot 2d dot plot
#'
#' @description Plots a dot plot of two categorical variables. Numeric
#' values can be displayed via alpha, color and size.
#'
#' @inherit argument_dummy params
#'
#' @return A ggplot.
#' @export
#'

plot_dot_plot_2d <- function(df,
                             x,
                             y,
                             alpha.by = NULL,
                             alpha.trans = "identity",
                             color.by = "overlap",
                             shape.by = NULL,
                             size.by = "fdr",
                             size.trans = "reverse",
                             pt.alpha = 0.9,
                             pt.color = "black",
                             pt.clrp = "default",
                             pt.clrsp = "plasma",
                             pt.shape = 19,
                             pt.size = 3,
                             ...){

  check_data_frame(
    df = df,
    var.class = purrr::set_names(list("factor", "factor"), nm = c(x, y))
  )

  params <-
    adjust_ggplot_params(
      params = list(alpha = pt.alpha, color = pt.color, shape = pt.shape, size = pt.size),
      sep = "."
    )

  ggplot2::ggplot(data = df, mapping = ggplot2::aes_string(x = x, y = y)) +
    ggplot2::layer(
      geom = "point",
      stat = "identity",
      position = "identity",
      params = params,
      mapping = ggplot2::aes_string(color = color.by, shape = shape.by, size = size.by)
    ) +
    ggplot2::scale_alpha(trans = alpha.trans) +
    ggplot2::scale_size(trans = size.trans) +
    scale_color_add_on(
      aes = "color",
      clrp = pt.clrp,
      clrsp = pt.clrsp,
      variable = df[[color.by]],
      ...
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = NULL)

}
