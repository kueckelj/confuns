

# Themes ------------------------------------------------------------------

#' @title Theme for statistic plots
#' @export

theme_statistics <- function(...){

  list(
    ggplot2::theme_classic(),
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(color = "black"),
      axis.text.x = ggplot2::element_text(color = "black"),
      strip.text.y = ggplot2::element_text(angle = 0, face = "italic", size = 14),
      strip.placement = "outside",
      strip.background = ggplot2::element_rect(color = "white", fill = "white"),
      panel.spacing.y = ggplot2::unit(10, "pt"),
      ...
    )
  )


}





# Miscellaneous add ons ---------------------------------------------------

#' @title Legend location

legend_none <- purrr::partial(.f = ggplot2::theme, legend.position = "none")

#' @rdname legend_none
#' @export
legend_right <- purrr::partial(.f = ggplot2::theme, legend.position = "right")

#' @rdname legend_none
#' @export
legend_left <- purrr::partial(.f = ggplot2::theme, legend.position = "left")

#' @rdname legend_none
#' @export
legend_bottom <- purrr::partial(.f = ggplot2::theme, legend.position = "bottom")
