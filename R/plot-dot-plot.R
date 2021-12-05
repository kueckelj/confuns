
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
                             color.trans = "identity",
                             shape.by = NULL,
                             size.by = "fdr",
                             size.trans = "reverse",
                             pt.alpha = 0.9,
                             pt.color = "black",
                             pt.shape = 19,
                             pt.size = 3,
                             ...){

  check_data_frame(
    df = df,
    var.class = purrr::set_names(list(), nm = c(x, y))
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
      clrsp = pt.clrsp,
      variable = df[[color.by]],
      trans = color.trans,
      ...
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = NULL)

}
