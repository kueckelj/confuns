

#' @title Obtain meta names
#' @description Extracts names of meta variables.
#'
#' @inherit argument_dummy params
#'
#' @return Character vector.
#'
#' @export
setGeneric(name = "getMetaNames", def = function(object){

  standardGeneric(f = "getMetaNames")

})

#' @rdname getMetaNames
#' @export
setMethod(
  f = "getMetaNames",
  signature = "Analysis",
  definition = function(object){

    dplyr::select(object@meta, -dplyr::all_of(object@key_name)) %>%
    base::names()

  }
)



#' @export
addMetaVars <- function(object, meta_df, overwrite = FALSE){

  key_name <- object@key_name

  new_vars <- base::names(dplyr::select(meta_df, -!!rlang::sym(key_name)))

  confuns::check_none_of(
    input = new_vars,
    against = getVariableNames(object),
    ref.input = "names of input meta variables",
    ref.against = "existing variables",
    overwrite = overwrite
  )

  if(base::isTRUE(overwrite)){

    object@meta <- dplyr::select(object@meta, -dplyr::any_of(new_vars))

  }

  object@meta <-
    dplyr::left_join(x = object@meta, y = meta_df, by = key_name) %>%
    tibble::as_tibble()

  object@variables_meta <-
    dplyr::select(object@meta, -dplyr::all_of(key_name)) %>%
    base::colnames()

  return(object)

}






plot_medoid_barchart <- function(df,
                                 methods.pam = NULL,
                                 facet.by = "cluster",
                                 color.by = "variables",
                                 cluster.subset = NULL,
                                 cluster.relevel = TRUE,
                                 variables.subset = NULL,
                                 variables.relevel = TRUE,
                                 display.medoid.name = FALSE,
                                 sep = ": ",
                                 clr = "black",
                                 clrp = "milo",
                                 verbose = TRUE,
                                 ...){

  medoids_df <-
    check_across_subset(
      df = df,
      across = "method_pam",
      across.subset = methods.pam,
      relevel = TRUE
    ) %>%
    check_across_subset(
      across = "cluster",
      across.subset = cluster.subset,
      relevel = cluster.relevel
    )

  if(facet.by == "variables"){

    ggplot2::ggplot(data = medoids_df, mapping = ggplot2::aes(x = cluster, y = values)) +
      ggplot2::geom_col(mapping = ggplot2::aes(fill = .data[[color.by]]), color = clr) +
      ggplot2::facet_wrap(facets = . ~ variables, scales = "free_x") +
      theme_statistics() +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = NULL, fill = color.by) +
      scale_color_add_on(aes = "fill", variable = medoids_df$cluster, clrp = clrp)

  } else if(facet.by == "cluster"){

    ggplot2::ggplot(data = medoids_df, mapping = ggplot2::aes(x = variables, y = values)) +
      ggplot2::geom_col(mapping = ggplot2::aes(fill = .data[[color.by]]), color = clr) +
      ggplot2::facet_wrap(facets = . ~ cluster) +
      theme_statistics() +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = NULL, fill = color.by) +
      scale_color_add_on(aes = "fill", variable = medoids_df[[color.by]], clrp = clrp)

  }

}
