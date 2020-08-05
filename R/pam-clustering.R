#' @title Silhouette Width Info
#'
#' @description Plots the silhouette information of
#' the provided pam-object.
#'
#' @param pam.obj A valid pam-object.
#' @param legend.position Character. Given to \code{ggplot2::theme()}.
#'
#' @inherit ggplot2_dummy return
#' @export
#'

plot_pam_silinfo <- function(pam.obj){

  stopifnot(base::all(c("pam", "partition") %in% class(pam.obj)))

  sil_data <-
    pam.obj$silinfo$widths %>%
    base::as.data.frame() %>%
    dplyr::mutate(numeration = dplyr::row_number())


  ggplot2::ggplot(data = sil_data, mapping = ggplot2::aes(x = numeration, y = sil_width)) +
    ggplot2::geom_bar(stat = "identity", mapping = ggplot2::aes(fill = as.factor(cluster))) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 0)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = pam.obj$silinfo$avg.width)) +
    ggplot2::scale_color_manual(
      values = c("#C4432A", "#2C6CA3", "#478C3D", "#F7E70A", "#FFA500", "#56D9ED",   "#C934BD",
                 "#3A389C", "#64DB74", "#C9B972", "#4F1211", "#CD4F39", "#00868B", "#8B7355",
                 "#CAFF70", "#525252","#FFD700", "#1C86EE", "#EEAEEE", "#8B2252")) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line.x = element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(face = "bold", size = 12.5),
                   legend.title = ggplot2::element_text(face = "bold", size = 12.5),
                   legend.text = ggplot2::element_text(face = "bold"),
                   legend.position = "bottom",
                   plot.title = ggplot2::element_text(face = "bold", size = 16.5),
                   plot.subtitle = ggplot2::element_text(size = 10)) +
    ggplot2::labs(fill = "Cluster", y = NULL,
                  subtitle = stringr::str_c("Average Silhouette Width: ",
                                            base::round(pam.obj$silinfo$avg.width, 2)
                  ))

}
