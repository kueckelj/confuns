#' @title k_dummy
#' @param k Numeric value. Positive integer specifying the number of clusters.
#' Must be lower than the number of observations in the supplied data.frame.

k_dummy <- function(k){}

#' @title n_dummy
#' @param n The max value for \code{k} of \code{cluster::pam()}. The function
#' computes the silhouette widhts for all pam-results from 2 up to \code{n}.

n_dummy <- function(n){}



# Function regarding Partitioning Around Medoids ---------------------------

#' @title PAM - Plot Silhouette Widths
#'
#' @description Takes a data.frame and plots the pam-silhouette-widths
#' for several k-values.
#'
#' @param df A data.frame.
#' @param ... Additional arguments given to \code{cluster::pam()}.
#' @inherit n_dummy params
#'
#' @inherit ggplot2_dummy return
#' @export

pam_plot_elbow <- function(df, n = 10, ...){

  base::stopifnot(base::is.data.frame(df))
  is_value(x = n, mode = "numeric", ref = "n")

  num_df <- dplyr::select_if(df, base::is.numeric)

  # obtain all sil-widths values
  res_df <-
    purrr::map_dfr(.x = 2:n,
                   .f = function(k){

                     pam_obj <-
                       cluster::pam(x = num_df,
                                    k = k,
                                    ...)

                     ret_df <-
                       data.frame(
                         k = k,
                         asw = pam_obj$silinfo$avg.width
                       )

                     base::return(ret_df)

                   })

  # plot elbow-plot
  ggplot2::ggplot(data = res_df, mapping = ggplot2::aes(x = k, y = asw)) +
    ggplot2::geom_path(color = "black") +
    ggplot2::geom_point(color = "black") +
    ggplot2::scale_x_continuous(breaks = 2:n, labels = 2:n) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Value for 'k'", y = "Average Silhouette Width")

}



#' @title PAM - Silhouette Width Info
#'
#' @description Plots the silhouette width-information with a bar-plot.
#'
#' @param pam.obj A valid pam-object.
#' @param df A data.frame.
#' @inherit n_dummy params
#'
#' @inherit ggplot2_dummy return
#'
#' @details
#'
#' \itemize{
#'  \item{ \code{pam_plot_silinfo()}: takes a pam-object and returns the silhouete-width information
#'  with one bar-plot.}
#'  \item{ \code{pam_plot_silinfo2()}: takes a data.frame, generates the pam-objects
#'  for k = 2-\code{n} and returns all bar-plots in one via \code{ggplot2::facet_wrap()}
#'  }
#'
#' }
#'
#' @export

pam_plot_silinfo <- function(pam.obj){

  stopifnot(base::all(c("pam", "partition") %in% class(pam.obj)))

  sil_data <-
    pam.obj$silinfo$widths %>%
    base::as.data.frame() %>%
    dplyr::mutate(numeration = dplyr::row_number())

  ggplot2::ggplot(data = sil_data, mapping = ggplot2::aes(x = numeration, y = sil_width)) +
    ggplot2::geom_bar(stat = "identity", mapping = ggplot2::aes(fill = as.factor(cluster))) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 0)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = pam.obj$silinfo$avg.width)) +
    ggplot2::scale_fill_manual(
      values = c("#C4432A", "#2C6CA3", "#478C3D", "#F7E70A", "#FFA500", "#56D9ED",   "#C934BD",
                 "#3A389C", "#64DB74", "#C9B972", "#4F1211", "#CD4F39", "#00868B", "#8B7355",
                 "#CAFF70", "#525252","#FFD700", "#1C86EE", "#EEAEEE", "#8B2252")) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line.x = ggplot2::element_blank(),
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

#' @rdname pam_plot_silinfo
pam_plot_silinfo2 <- function(df, n = 10, ...){

  base::stopifnot(base::is.data.frame(df))
  is_value(x = n, mode = "numeric", ref = "n")

  num_df <- dplyr::select_if(df, base::is.numeric)

  res_df <- purrr::map_dfr(.x = 2:n,
                           .f = function(k){

                             pam_obj <-
                               cluster::pam(x = num_df,
                                            k = k,
                                            ...)

                             base::as.data.frame(pam_obj$silinfo$widths) %>%
                             dplyr::mutate(
                               k = base::as.factor(stringr::str_c("k = ", k, sep = "")),
                               avg_sil_width = pam_obj$silinfo$avg.width,
                               numeration = dplyr::row_number())

                           })

  ggplot2::ggplot(data = res_df, mapping = ggplot2::aes(x = numeration, y = sil_width)) +
    ggplot2::geom_bar(stat = "identity", mapping = ggplot2::aes(fill = as.factor(cluster))) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 0)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = avg_sil_width), linetype = "dashed") +
    ggplot2::facet_wrap(facets = . ~ k) +
    ggplot2::scale_fill_manual(
      values = c("#C4432A", "#2C6CA3", "#478C3D", "#F7E70A", "#FFA500", "#56D9ED",   "#C934BD",
                 "#3A389C", "#64DB74", "#C9B972", "#4F1211", "#CD4F39", "#00868B", "#8B7355",
                 "#CAFF70", "#525252","#FFD700", "#1C86EE", "#EEAEEE", "#8B2252")) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(face = "bold", size = 10),
                   legend.title = ggplot2::element_text(face = "bold", size = 10),
                   legend.text = ggplot2::element_text(face = "bold"),
                   legend.position = "bottom",
                   strip.background = ggplot2::element_blank()) +
    ggplot2::labs(fill = "Cluster", y = "Silhouette Width")

}


#' @title PAM - Clustering
#'
#' @description Adds a pam-cluster variable to the supplied
#' data.frame.
#'
#' @param df A data.frame.
#' @param var.name Character value. Specifying the string of the variable
#' containing the clusters.
#' @param var.type Characte value. Specifying the class of the variable
#' containing the clusters. Must be one of \emph{'numeric', 'factor', 'character'}.
#' @param ... Additional arguments given to \code{clusters::pam()}.
#' @inherit assign_obj params
#' @inherit k_dummy params
#'
#' @return The input data.frame specified in \code{df} with an additional
#' variable containing the clusters information.
#'
#' @details Performs clustering by including all numeric variables.
#'
#' @export

pam_add_cluster <- function(df,
                            k,
                            var.name = "pam_cluster",
                            var.type = "factor",
                            assign = FALSE,
                            assign.name = "pam_obj",
                            ...){

  # 1. Control --------------------------------------------------------------

  base::stopifnot(base::is.data.frame(df))
  is_value(x = var.name, mode = "character", ref = "var.name")
  is_value(x = var.type, mode = "character", ref = "var.type")
  is_value(x = k, mode = "numeric", ref = "k")

  check_assign(assign, assign.name)

  # -----

  # 2. Clustering -----------------------------------------------------------

  pam_obj <- cluster::pam(x = dplyr::select_if(df, base::is.numeric),
                          k = k,
                          ...)

  if(var.name == "character"){

    pam_cluster <-
      base::as.character(pam_obj$clustering) %>%
      stringr::str_c("Cluster", ., sep = " ")

  } else if(var.name == "factor"){

    pam_cluster <- base::as.factor(pam_obj$clustering)

  } else {

    pam_cluster <- pam_obj$clustering

  }

  # -----

  # assign if desired
  assign_obj(assign, pam_obj, assign.name)

  # return mutated df
  dplyr::mutate(.data = df, {{var.name}} := pam_cluster)

}



