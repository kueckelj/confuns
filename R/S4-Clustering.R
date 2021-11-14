#' @include S4-AnalysisAspect.R



# S4-classes --------------------------------------------------------------

#' @title The \code{Clustering}-class
#'
#' @description S4-class for convenient cluster analysis.
#'
#' @slot data data.frame. The data on which the analysis bases on.
#' @slot key_name character. The name of the variable that is used to identify
#' each observation uniquely.
#' @slot meta data.frame. Data that was part of the input data but is not supposed
#' to be included in analysis steps.
#' @slot methods list. A list of objects of S4-class \code{ClusteringMethod}.
#' @slot variables_grouping character. The names of all grouping variables
#' of the input data - variables of class character or factor. (Does not include
#' variable of slot @@key_name)
#' @slot variables_logical character. The names of all logical variables of
#' the input data.
#' @slot variables_numeric character. The names of all numeric variables
#' based on which outlier detection is conducted.

Clustering <- setClass(Class = "Clustering",
                       slots = list(),
                       contains = "AnalysisAspect"
                       )

#' @title The \code{ClusteringMethod}-class
#'
#' @description S4-class that contains results from the clustering
#' algorithm it stands for.
#'
#' @slot key_name character. The name of the variable that is used to identify
#' each observation uniquely.
#' @slot method character. The name of the method. Is additionally encoded
#' in the name of the S4-class that inherits class \code{ClusteringMethod} using
#' the syntax \emph{Clustering<method>}.
#' @slot results list. A list of results. Varies from cluster method to
#' cluster method.
#'
#' @export
#'
ClusteringMethod <- setClass(Class = "ClusteringMethod",
                             slots = list(
                               key_name = "character",
                               method = "character",
                               results = "list"
                             ))


# r-objects ---------------------------------------------------------------

valid_methods_clustering <- c("hclust", "kmeans", "pam")

# -----


# functions ---------------------------------------------------------------

#' @rdname initiateAnalysisAspect
#' @export
initiateClustering <- function(data,
                               key_name,
                               key_prefix = "ID",
                               meta_names = character(0),
                               verbose = TRUE){

  object <-
    initiateAnalysisAspect(
      data = data,
      key_name = key_name,
      key_prefix = key_prefix,
      meta_names = meta_names,
      verbose = verbose,
      analysis_aspect = "Clustering"
    )

  return(object)

}

#' @rdname validInput
#' @export
validMethodsClustering <- function(){

  return(valid_methods_clustering)

}


# -----


# own generics ------------------------------------------------------------


# -----




# methods for external generics -------------------------------------------

#' @rdname agglomerateHierarchicalTrees
#' @export
setMethod(
  f = "agglomerateHierarchicalTrees",
  signature = "Clustering",
  definition = function(object,
                        methods_dist = "euclidean",
                        methods_aggl = "ward.D",
                        verbose = TRUE,
                        ...){

    hclust_obj <- getResults(object, method = "hclust")

    check_one_of(
      input = methods_dist,
      against = base::names(hclust_obj@dist_matrices),
      fdb.opt = 2,
      ref.opt.2 = "computed distance matrices"
    )

    dist_matrices <- hclust_obj@dist_matrices[methods_dist]

    results <-
      agglomerate_hierarchical_trees(
        dist.matrices = dist_matrices,
        methods.aggl = methods_aggl,
        verbose = verbose,
        ...
      )

    for(method_dist in base::names(results)){

      method_list <- results[[method_dist]]

      for(method_aggl in base::names(method_list)){

        hclust_obj@results[[method_dist]][[method_aggl]] <-
          method_list[[method_aggl]]

      }

    }

    object@methods[["hclust"]] <- hclust_obj

    return(object)

  }
)

#' @rdname computeClusteringKmeans
#' @export
setMethod(
  f = "computeClusteringKmeans",
  signature = "Clustering",
  definition = function(object,
                        ks,
                        methods_kmeans,
                        verbose = TRUE){

    # input check
    check_one_of(
      input = methods_kmeans,
      against = validMethodsKmeans()
    )

    ks <-
      base::as.integer(ks) %>%
      base::unique() %>%
      purrr::keep(.p = ~ .x > 1)

    kmeans_obj <- object@methods[["kmeans"]]

    if(base::is.null(kmeans_obj)){

      give_feedback(msg = "Creating new object of class ClusteringKmeans.", verbose = verbose)

      kmeans_obj <- ClusteringKmeans(key_name = object@key_name, method = "kmeans")

    }

    data <-
      getDf(object, numeric = TRUE) %>%
      tibble::column_to_rownames(var = object@key_name) %>%
      base::as.matrix()

    results <-
      compute_clustering_kmeans(
        data = data,
        ks = ks,
        methods.kmeans = methods_kmeans,
        verbose = verbose
      )

    for(method in base::names(results)){

      method_list <- results[[method]]

      for(k in base::names(method_list)){

        kmeans_obj@results[[method]][[k]] <- method_list[[k]]

      }

    }

    object@methods[["kmeans"]] <- kmeans_obj

    return(object)

  })

#' @rdname computeClusteringPam
#' @export
setMethod(
  f = "computeClusteringPam",
  signature = "Clustering",
  definition = function(object,
                        ks,
                        methods_pam,
                        verbose = TRUE){

    # input check
    check_one_of(
      input = methods_pam,
      against = validMethodsPam()
    )

    ks <-
      base::as.integer(ks) %>%
      base::unique() %>%
      purrr::keep(.p = ~ .x > 1)

    pam_obj <- object@methods[["pam"]]

    if(base::is.null(pam_obj)){

      give_feedback(msg = "Creating new object of class ClusteringPam.", verbose = verbose)

      pam_obj <- ClusteringPam(key_name = object@key_name, method = "pam")

    }

    data <-
      getDf(object, numeric = TRUE) %>%
      tibble::column_to_rownames(var = object@key_name) %>%
      base::as.matrix()

    results <-
      compute_clustering_pam(
        data = data,
        ks = ks,
        methods.pam = methods_pam,
        verbose = verbose
      )

    for(method in base::names(results)){

      method_list <- results[[method]]

      for(k in base::names(method_list)){

        pam_obj@results[[method]][[k]] <- method_list[[k]]

      }

    }

    object@methods[["pam"]] <- pam_obj

    return(object)

  })

#' @rdname computeDistanceMatrices
#' @export
setMethod(
  f = "computeDistanceMatrices",
  signature = "Clustering",
  definition = function(object,
                        methods_dist = "euclidean",
                        p = 2,
                        force = FALSE,
                        verbose = TRUE){

    hclust_obj <- object@methods[["hclust"]]

    if(base::is.null(hclust_obj)){

      give_feedback(msg = "Creating new object of class ClusteringHclust.", verbose = verbose)

      hclust_obj <- ClusteringHclust(key_name = object@key_name, method = "hclust")

    } else {

      existing_dists <- base::names(hclust_obj@dist_matrices)

      check_none_of(
        input = methods_dist,
        against = existing_dists,
        ref.input = "distance matrices to compute",
        ref.against = "computed distance matrices",
        force = force
      )

    }

    data <-
      getDf(object = object, numeric = TRUE) %>%
      tibble::column_to_rownames(var = object@key_name) %>%
      base::as.matrix()

    results <-
      compute_distance_matrices(
        data = data,
        methods.dist = methods_dist,
        p = p,
        verbose = verbose
      )

    for(method in base::names(results)){

      hclust_obj@dist_matrices[[method]] <- results[[method]]

    }

    object@methods[["hclust"]] <- hclust_obj

    return(object)

  }
)


# get

#' @rdname getAvgSilWidthsDf
#' @export
setMethod(
  f = "getAvgSilWidthsDf",
  signature = "Clustering",
  definition = function(object,
                        ks,
                        methods_pam = "euclidean"){

    pam_obj <- getResults(object, method = "pam")

    avg_sil_widths_df <-
      getAvgSilWidthsDf(
        object = pam_obj,
        ks = ks,
        methods_pam = methods_pam
      )

    return(avg_sil_widths_df)

  }
)

#' @rdname getClusteringKmeans
#' @export
setMethod(f = "getClusteringKmeans", signature = "Clustering", definition = function(object, ...){

  getResults(
    object = object,
    method = "kmeans",
    ...
  )

})

#' @rdname getClusteringPam
#' @export
setMethod(f = "getClusteringPam", signature = "Clustering", definition = function(object, ...){

  getResults(
    object = object,
    method = "pam",
    ...
  )

})


#' @rdname getHclust
#' @export
setMethod(
  f = "getHclust",
  signature = "Clustering",
  definition = function(object,
                        method_dist = "euclidean",
                        method_aggl = "ward.D"){

    hclust_obj <- getResults(object, method = "hclust")

    hclust <-
      getHclust(
        object = hclust_obj,
        method_dist = method_dist,
        method_aggl = method_aggl
      )

    return(hclust)

  }
)

#' @rdname getKmeans
#' @export
setMethod(
  f = "getKmeans",
  signature = "Clustering",
  definition = function(object,
                        k,
                        method_kmeans = "Hartigan-Wong"){

    kmeans_obj <- getResults(object = object, method = "kmeans")

    kmeans <- getKmeans(object = kmeans_obj, k = k, method_kmeans = method_kmeans)

    return(kmeans)

  }
)

#' @rdname getPam
#' @export
setMethod(
  f = "getPam",
  signature = "Clustering",
  definition = function(object,
                        k,
                        method_pam = "euclidean"
                        ){

    pam_obj <- getResults(object, method = "pam")

    pam <- getPam(object = pam_obj, k = k, method_pam = method_pam)

    return(pam)

  })

#' @rdname getSilWidthsDf
#' @export
setMethod(
  f = "getSilWidthsDf",
  signature = "Clustering",
  definition = function(object,
                        ks,
                        method_pam = "euclidean"){

    pam_obj <- getResults(object, method = "pam")

    sil_widths_df <- getSilWidthsDf(object = pam_obj, ks = ks, method_pam = method_pam)

    return(sil_widths_df)

  }
)

# plot

#' @rdname plotAvgSilWidths
#' @export
setMethod(
  f = "plotAvgSilWidths",
  signature = "Clustering",
  definition = function(object,
                        ks,
                        methods_pam = "euclidean",
                        color = "steelblue",
                        display_cols = TRUE,
                        display_points = TRUE,
                        display_line = TRUE,
                        ncol = NULL,
                        nrow = NULL){

    avg_sil_width_df <-
      getAvgSilWidthsDf(object, ks = ks, methods_pam = methods_pam)

    p <-
      ggplot2::ggplot(data = avg_sil_width_df, mapping = ggplot2::aes(x = k, y = avg_widths)) +
      ggplot2::facet_wrap(facets = . ~ method_pam, nrow = nrow, ncol = ncol) +
      ggplot2::labs(x = "Centers (k)", y = "Avg. Silhouette Width") +
      theme_statistics()

    # add layer
    if(base::isTRUE(display_cols)){

      p <- p + ggplot2::geom_col(color = "black", fill = color)

    }

    if(base::isTRUE(display_points)){

      p <- p + ggplot2::geom_point(color = "black")

    }

    if(base::isTRUE(display_line)){

      p <- p + ggplot2::geom_line(color = "black", mapping = ggplot2::aes(group = 1))

    }

    # return plot
    return(p)

  }
)

#' @rdname plotScreeplot
#' @export
setMethod(
  f = "plotScreeplot",
  signature = "Clustering",
  definition = function(object,
                        methods_kmeans,
                        ks = NULL,
                        color = "steelblue",
                        display_cols = TRUE,
                        display_line = TRUE,
                        display_points = TRUE){

    check_one_of(
      input = methods_kmeans,
      against = validMethodsKmeans()
    )

    clustering_kmeans <- getClusteringKmeans(object)

    calculated_methods <- base::names(clustering_kmeans@results)

    methods_kmeans <- methods_kmeans[methods_kmeans %in% calculated_methods]

    res_list <- clustering_kmeans@results[methods_kmeans]

    res_df <-
      purrr::imap_dfr(
        .x = res_list,
        .f = function(method_list, method){

          res_df2 <-
            purrr::imap_dfr(.x = method_list,
                            method_kmeans = method,
                            .f = function(res, k_string, method_kmeans){

                              res_df3 <-
                                data.frame(
                                  method = method_kmeans,
                                  k = stringr::str_remove(k_string, pattern = "^k_") %>% base::as.numeric(),
                                  tot_withinss = res$tot.withinss
                                )

                              base::return(res_df3)

                            })

        }
      ) %>%
      dplyr::group_by(method) %>%
      dplyr::mutate(k = base::as.factor(k))

    if(base::is.numeric(ks)){

      res_df <- dplyr::filter(res_df, k %in% {{ks}})

    }

    # create basic plot
    p <-
      ggplot2::ggplot(data = res_df, mapping = ggplot2::aes(x = k, y = tot_withinss)) +
      ggplot2::facet_wrap(facets = ~ method) +
      ggplot2::labs(y = NULL, x = "Centers (k)") +
      theme_statistics()


    # add layer
    if(base::isTRUE(display_cols)){

      p <- p + ggplot2::geom_col(color = "black", fill = color)

    }

    if(base::isTRUE(display_points)){

      p <- p + ggplot2::geom_point(color = "black")

    }

    if(base::isTRUE(display_line)){

      p <- p + ggplot2::geom_line(color = "black", mapping = ggplot2::aes(group = 1))

    }

    # return plot
    return(p)

  })


#' @rdname plotSilWidths
#' @export
setMethod(
  f = "plotSilWidths",
  signature = "Clustering",
  definition = function(object,
                        ks,
                        method_pam = "euclidean",
                        clrp = "milo",
                        ncol = NULL,
                        nrow = NULL,
                        verbose = TRUE
  ){

    sil_widths_df <- getSilWidthsDf(object, ks = ks, method_pam = method_pam)

    ggplot2::ggplot(data = sil_widths_df, mapping = ggplot2::aes(x = x_axis, y = sil_width)) +
      ggplot2::geom_col(mapping = ggplot2::aes(color = cluster, fill = cluster)) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::facet_wrap(facets = ~ cluster_name, ncol = ncol, nrow = nrow) +
      scale_color_add_on(aes = "fill",  variable = "discrete", clrp = clrp) +
      scale_color_add_on(aes = "color", variable = "discrete", clrp = clrp, guide = FALSE) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "lightgrey"),
        legend.title = ggplot2::element_text(size = 12.5),
        plot.title = ggplot2::element_text(face = "bold", size = 16.5),
        plot.subtitle = ggplot2::element_text(size = 10)
      ) +
      ggplot2::labs(x = NULL, y = NULL, color = NULL, fill = "Cluster")


  }
)

# -----

