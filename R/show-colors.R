

#' @title Visualize colorpalettes and -spectra
#'
#' @description Visualizes colorpalettes and -spectra as referred
#' to by arguments `clrp` and `clrsp`.
#'
#' @param input Character vector of colorpalettes and -spectra.
#' @param n Numeric. Indicates how many colors to be used.
#' @param title_size Numeric. The title size.
#'
#' @return A ggplot.
#'
#' @export
#'
show_colors <- function(input, n = 20, title_size = 10){

  if(confuns::is_list(input)){

    input <-
      purrr::flatten_chr(input) %>%
      base::unname()

  }

  input <- input[input != "default"]

  input_spectra <- input[input %in% validColorSpectra(flatten = TRUE)]

  if(base::length(input_spectra) != 0){

    plot_list1 <-
      purrr::map(
        .x = input_spectra,
        .f = function(x){

          if(x %in% confuns::diverging){

            vec <- base::seq(-1, 1, len = n)

          } else {

            vec <- 1:n

          }

          df <- base::data.frame(x = vec, y = 1)

          out <-
            ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_tile(mapping = ggplot2::aes(fill = x)) +
            confuns::scale_color_add_on(aes = "fill", clrsp = x, variable = vec) +
            ggplot2::scale_y_continuous() +
            ggplot2::theme_void() +
            ggplot2::theme(
              legend.position = "none",
              plot.title = ggplot2::element_text(hjust = 0.5, size = title_size)
            ) +
            ggplot2::labs(title = x)

          return(out)

        }
      ) %>%
      patchwork::wrap_plots()

  } else {

    plot_list1 <- NULL

  }


  input_palettes <- input[input %in% validColorPalettes(flatten = TRUE)]

  if(base::length(input_palettes) != 0){

    plot_list2 <-
      purrr::map(
        .x = input_palettes,
        .f = function(x){

          vec <- base::as.character(1:n)

          if(x %in% validColorPalettes()[["Viridis Options"]]){

            vec <- base::as.character(vec[1:9])

          } else {

            vec <- as.character(vec)[1:base::length(confuns::color_vector(clrp = x))]

          }

          df <- base::data.frame(x = vec, y = 1)

          out <-
            ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_tile(mapping = ggplot2::aes(fill = x)) +
            confuns::scale_color_add_on(aes = "fill", clrp = x, variable = vec) +
            ggplot2::scale_y_continuous() +
            ggplot2::theme_void() +
            ggplot2::theme(
              legend.position = "none",
              plot.title = ggplot2::element_text(hjust = 0.5, size = title_size)
            ) +
            ggplot2::labs(title = x)

          return(out)

        }
      ) %>%
      patchwork::wrap_plots()

  } else {

    plot_list2 <- NULL

  }

  plot_list1 / plot_list2

}
