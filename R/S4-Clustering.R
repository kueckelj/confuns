#' @include S4-AnalysisAspect.R
NULL


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

cluster_vec_to_df <- function(vec, name, prefix, key){

  df <- base::as.data.frame(vec)

  df[[1]] <- stringr::str_c(prefix, df[[1]], sep = "")

  df <-
    magrittr::set_colnames(df, value = name) %>%
    dplyr::mutate(dplyr::across(.fns = base::as.factor)) %>%
    tibble::rownames_to_column(var = key) %>%
    tibble::as_tibble()

  return(df)

}

#' @rdname initiateAnalysisAspect
#' @export
initiateClustering <- function(data,
                               key_name,
                               key_prefix = NULL,
                               lgl_to_group = TRUE,
                               meta_names = character(0),
                               verbose = TRUE){

  object <-
    initiateAnalysisAspect(
      data = data,
      key_name = key_name,
      key_prefix = key_prefix,
      meta_names = meta_names,
      lgl_to_group = lgl_to_group,
      verbose = verbose,
      analysis_aspect = "Clustering"
    )

  object <- scaleData(object, na_rm = TRUE)

  return(object)

}

#' @rdname validInput
#' @export
validMethodsClustering <- function(){

  return(valid_methods_clustering)

}


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

#' @rdname computeClusteringHclust
#' @export
setMethod(
  f = "computeClusteringHclust",
  signature = "Clustering",
  definition = function(object,
                        methods_dist = "euclidean",
                        methods_aggl = "ward.D",
                        verbose = TRUE,
                        ...){

    hclust_obj <- object@methods[["hclust"]]

    if(base::is.null(hclust_obj)){

      give_feedback(msg = "Creating new object of class ClusteringHclust.", verbose = verbose)

      hclust_obj <- ClusteringHclust(key_name = object@key_name, method = "hclust")

    }

    mtr <- getScaledMtr(object)

    for(method_dist in methods_dist){

      dist_mtr <- getDistMtr(object = hclust_obj, method_dist = method_dist, stop_if_null = FALSE)

      if(base::is.null(dist_mtr)){

        give_feedback(
          msg = glue::glue("Computing temporary distance matrix for method '{method_dist}'."),
          verbose = verbose
        )

        dist_mtr <- stats::dist(x = mtr, method = method_dist)

      }

      for(method_aggl in methods_aggl){

        hclust_obj@results[[method_dist]][[method_aggl]] <- stats::hclust(d = dist_mtr, method = method_aggl)

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
                        methods_kmeans = "Hartigan-Wong",
                        verbose = TRUE,
                        ...){

    # input check
    check_one_of(
      input = methods_kmeans,
      against = validMethodsKmeans()
    )

    ks <-
      base::as.integer(ks) %>%
      base::unique() %>%
      base::sort()

    kmeans_obj <- object@methods[["kmeans"]]

    if(base::is.null(kmeans_obj)){

      give_feedback(msg = "Creating new object of class ClusteringKmeans.", verbose = verbose)

      kmeans_obj <- ClusteringKmeans(key_name = object@key_name, method = "kmeans")

    }

    data <- getScaledMtr(object)

    results <-
      compute_clustering_kmeans(
        data = data,
        ks = ks,
        methods.kmeans = methods_kmeans,
        verbose = verbose,
        ...
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
                        methods_pam = "euclidean",
                        verbose = TRUE,
                        ...){

    # input check
    check_one_of(
      input = methods_pam,
      against = validMethodsPam()
    )

    ks <-
      base::as.integer(ks) %>%
      base::unique() %>%
      base::sort()

    if(1 %in% ks){

      warning(
        "Clustering with k = 1 can be computed but including it in downstream plots might cause errors."
        )

    }

    pam_obj <- object@methods[["pam"]]

    if(base::is.null(pam_obj)){

      give_feedback(msg = "Creating new object of class ClusteringPam.", verbose = verbose)

      pam_obj <- ClusteringPam(key_name = object@key_name, method = "pam")

    }

    data <- getScaledMtr(object)

    results <-
      compute_clustering_pam(
        data = data,
        ks = ks,
        methods.pam = methods_pam,
        verbose = verbose,
        ...
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

    data <- getScaledMtr(object)

    results <-
      compute_dist_matrices(
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


#' @rdname getClusteringHclust
#' @export
setMethod(f = "getClusteringHclust", signature = "Clustering", definition = function(object, ...){

  getResults(
    object = object,
    method = "hclust",
    ...
  )

})

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


#' @rdname getClusterVarsHclust
#' @export
setMethod(
  f = "getClusterVarsHclust",
  signature = "Clustering",
  definition = function(object,
                        ks = NULL,
                        hs = NULL,
                        methods_dist = "euclidean",
                        methods_aggl = "ward.D",
                        prefix = "",
                        naming_k = "{method_dist}_{method_aggl}_k{k}",
                        naming_h = "{method_dist}_{method_aggl}_h{h}"){

    key <- object@key_name

    out_df <- getKeyDf(object)

    for(method_dist in methods_dist){

      for(method_aggl in methods_aggl){

        hclust_obj <- getHclust(object, method_dist = method_dist, method_aggl = method_aggl)

        for(k in ks){

          name <- glue::glue(naming_k)

          df <-
            cluster_vec_to_df(
              vec = stats::cutree(tree = hclust_obj, k = k),
              prefix = prefix,
              name = name,
              key = key
            )

          out_df <- dplyr::left_join(x = out_df, y = df, by = key)

        }

        for(h in hs){

          name <- glue::glue(naming_h)

          df <-
            cluster_vec_to_df(
              vec = stats::cutree(tree = hclust_obj, h = h),
              prefix = prefix,
              name = name,
              key = key
            )

          out_df <- dplyr::left_join(x = out_df, y = df, by = key)

        }

      }

    }

    return(out_df)

  }
)

#' @rdname getClusterVarsKmeans
#' @export
setMethod(
  f = "getClusterVarsKmeans",
  signature = "Clustering",
  definition = function(object,
                        ks,
                        methods_kmeans = "Hartigan-Wong",
                        prefix = "",
                        naming = "{method_kmeans}_k{k}"){

    key <- object@key_name

    out_df <- getKeyDf(object)

    for(k in ks){

      for(method_kmeans in methods_kmeans){

        cluster_vec <- getKmeans(object, k = k, method_kmeans = method_kmeans)$cluster

        name <- glue::glue(naming)

        df <- cluster_vec_to_df(vec = cluster_vec, name = name, prefix = prefix, key = key)

        out_df <- dplyr::left_join(out_df, y = df, by = key)

      }

    }

    return(out_df)

  }
)

#' @rdname getClusterVarsPam
#' @export
setMethod(
  f = "getClusterVarsPam",
  signature = "Clustering",
  definition = function(object,
                        ks,
                        methods_pam = "euclidean",
                        prefix = "",
                        naming = "{method_pam}_k{k}"){

    key <- object@key_name

    out_df <- getKeyDf(object)

    for(k in ks){

      for(method_pam in methods_pam){

        cluster_vec <- getPam(object, method_pam = method_pam, k = k)$clustering

        name <- glue::glue(naming)

        df <- cluster_vec_to_df(vec = cluster_vec, name = name, prefix = prefix, key = key)

        out_df <- dplyr::left_join(x = out_df, y = df, by = key)

      }

    }

    return(out_df)

  }
)


#' @rdname getDendro
#' @export
setMethod(
  f = "getDendro",
  signature = "Clustering",
  definition = function(object,
                        method_dist = "euclidean",
                        method_aggl = "ward.D",
                        k = NULL,
                        h = NULL,
                        type = "rectangle"){

    check_h_k(k = k, h = h, only.one = TRUE, skip.allow = TRUE)

    hclust_obj <-
      getHclust(
        object = object,
        method_dist = method_dist,
        method_aggl = method_aggl
      )

    hcdata <- ggdendro::dendro_data(hclust_obj, type = type)

    if(base::any(base::is.numeric(c(k, h)))){

      seg <- hcdata$segments
      labclust <- stats::cutree(hclust_obj, k = k, h = h)[hclust_obj$order]

      if(base::is.null(k) & base::is.numeric(h)){

        k <- base::max(base::as.numeric(labclust))

      }

      segclust <- base::rep(0L, base::nrow(seg))
      heights <- base::sort(hclust_obj$height, decreasing = TRUE)
      height <- base::mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)

      for (i in 1:k) {
        xi <- hcdata$labels$x[labclust == i]
        idx1 <- seg$x >= base::min(xi) & seg$x <= base::max(xi)
        idx2 <- seg$xend >= base::min(xi) & seg$xend <= base::max(xi)
        idx3 <- seg$yend < height
        idx <- idx1 & idx2 & idx3
        segclust[idx] <- i
      }

      idx <- base::which(segclust == 0L)
      segclust[idx] <- segclust[idx + 1L]
      hcdata$segments$clust <- segclust %>% base::as.factor()
      hcdata$segments$line <- base::as.integer(segclust < 1L) %>% base::as.factor()
      hcdata$labels$clust <- labclust %>% base::as.factor()

    }

    return(hcdata)

  })

#' @rdname getDendroSegmentDf
#' @export
setMethod(
  f = "getDendroSegmentDf",
  signature = "Clustering",
  definition = function(object,
                        methods_dist = "euclidean",
                        methods_aggl = "ward.D",
                        k = NULL,
                        h = NULL,
                        type = "rectangle"){

    check_one_of(
      input = methods_dist,
      against = validMethodsDist()
    )

    check_one_of(
      input = methods_aggl,
      against = validMethodsAggl()
    )

    check_h_k(h = h, k = k, only.one = TRUE, skip.allow = TRUE)

    df <-
      purrr::map_df(.x = methods_dist, .f = function(method_dist){

        purrr::map_df(.x = methods_aggl, .f = function(method_aggl){

          getDendro(
            object = object,
            method_dist = method_dist,
            method_aggl = method_aggl,
            k = k,
            h = h,
            type = type
          ) %>%
            ggdendro::segment() %>%
            dplyr::mutate(
              dist = {{method_dist}},
              aggl = {{method_aggl}}
            )

        })

      }) %>%
      tibble::as_tibble()

    return(df)

  }
)

#' @rdname getDistMtr
#' @export
setMethod(
  f = "getDistMtr",
  signature = "Clustering",
  definition = function(object, method_dist = "euclidean", stop_if_null = FALSE){

    check_one_of(
      input = method_dist,
      against = validMethodsDist()
    )

    out <- object@methods[["hclust"]]@dist_matrices

    if(base::is.null(out) & base::isTRUE(stop_if_null)){

      stop(
        glue::glue(
          "No distance matrix found for method '{method_dist}'."
        )
      )

    }

    return(out)

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
                        method_kmeans = "Hartigan-Wong",
                        stop_if_null = TRUE){

    kmeans_obj <- getResults(object = object, method = "kmeans")

    kmeans <-
      getKmeans(
        object = kmeans_obj,
        k = k,
        method_kmeans = method_kmeans,
        stop_if_null = stop_if_null
        )

    return(kmeans)

  }
)

#' @rdname getMedoidsDf
#' @export
setMethod(
  f = "getMedoidsDf",
  signature = "Clustering",
  definition = function(object,
                        ks,
                        methods_pam = "euclidean",
                        prefix = "",
                        format = "wide"){

    getMedoidsDf(
      object = object@methods[["pam"]],
      ks = ks,
      methods_pam = methods_pam,
      prefix = prefix,
      format = format
    )


  }

)

#' @rdname getPam
#' @export
setMethod(
  f = "getPam",
  signature = "Clustering",
  definition = function(object,
                        k,
                        method_pam = "euclidean",
                        stop_if_null = TRUE
                        ){

    pam_obj <- getResults(object, method = "pam")

    pam <- getPam(object = pam_obj, k = k, method_pam = method_pam, stop_if_null = stop_if_null)

    pam$data <- getScaledMtr(object)

    return(pam)

  })

#' @rdname getSilWidthsDf
#' @export
setMethod(
  f = "getSilWidthsDf",
  signature = "Clustering",
  definition = function(object,
                        ks,
                        method_pam = "euclidean",
                        format = "long"){

    pam_obj <- getResults(object, method = "pam")

    sil_widths_df <-
      getSilWidthsDf(
        object = pam_obj,
        ks = ks,
        method_pam = method_pam,
        format = format)

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
                        display_cols = TRUE,
                        col_alpha = 0.9,
                        col_color = "black",
                        col_fill = "steelblue",
                        display_line = TRUE,
                        line_alpha = 0.9,
                        line_color = "black",
                        line_size = 1.5,
                        display_points = TRUE,
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_size = 4.5,
                        ncol = NULL,
                        nrow = NULL){

    avg_sil_width_df <-
      getAvgSilWidthsDf(object, ks = ks, methods_pam = methods_pam)

    nth <-
      (base::max(avg_sil_width_df[["k"]])/10) %>%
      base::floor()

    xlabs <-
      base::unique(avg_sil_width_df[["k"]]) %>%
      reduce_vec(x = ., nth = nth)

    p <-
      ggplot2::ggplot(data = avg_sil_width_df, mapping = ggplot2::aes(x = k, y = avg_widths)) +
      ggplot2::facet_wrap(facets = . ~ method_pam, nrow = nrow, ncol = ncol) +
      ggplot2::scale_x_continuous(breaks = xlabs, labels = xlabs) +
      ggplot2::labs(x = "Centers (k)", y = "Avg. Silhouette Width") +
      theme_statistics()

    # add layer
    # add layer
    if(base::isTRUE(display_cols)){

      p <-
        p +
        ggplot2::geom_col(
          fill = col_fill,
          color = col_color,
          alpha = col_alpha
        )

    }

    if(base::isTRUE(display_points)){

      p <-
        p +
        ggplot2::geom_point(
          alpha = pt_alpha,
          color = pt_color,
          size = pt_size
        )

    }

    if(base::isTRUE(display_line)){

      p <-
        p +
        ggplot2::geom_line(
          alpha = line_alpha,
          color = line_color,
          size = line_size,
          mapping = ggplot2::aes(group = 1)
        )

    }

    # return plot
    return(p)

  }
)

#' @rdname plotDendrogram
#' @param facet_with Character value. Either 'grid' or 'wrap'. Specifies the function
#' with which the plot-facetting is created. If the number of input combinations for
#' \code{methods_dist} and \code{methods_aggl} length 2 or bigger and \code{facet_with} = \emph{'wrap'}
#' \code{ggplot2::facet_wrap()} is used. Else \code{ggplot2::facet_grid()} is used.
#' @param simple Logical value. If TRUE, the dendrogram is plotted with \code{base::plot()}.
#' This is way quicker but does not allow for ggplot2 specific adjustments.
#'
#' @export
setMethod(
  f = "plotDendrogram",
  signature = "Clustering",
  definition = function(object,
                        methods_dist = "euclidean",
                        methods_aggl = "ward.D",
                        k = NULL,
                        h = NULL,
                        type = "rectangle",
                        facet_with = "grid",
                        direction = "bt",
                        branch_color = "black",
                        branch_size = 1,
                        display_labels = FALSE,
                        labels_angle = 90,
                        labels_hjust = 0,
                        labels_nudge = 0.01,
                        labels_size = 3,
                        labels_vjust = 0.5,
                        display_legend = TRUE,
                        display_title = FALSE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        simple = FALSE,
                        nrow = NULL,
                        ncol = NULL,
                        ...){

    check_one_of(
      input = methods_dist,
      against = validMethodsDist()
    )

    check_one_of(
      input = methods_aggl,
      against = validMethodsAggl()
    )

    if(base::isTRUE(simple)){

      hclust_obj <-
        getHclust(
          object = object,
          method_dist = methods_dist[1],
          method_aggl = methods_aggl[1]
        )

      base::plot(hclust_obj, ...)

    } else if(FALSE){ # base::all(base::is.null(k), base::is.null(h))

      hclust_obj <-
        getHclust(
          object = object,
          method_dist = methods_dist[1],
          method_aggl = methods_aggl[1]
        )

      dendro_plot <- ggdendro::ggdendrogram(data = hclust_obj, labels = display_labels, ...)

      return(dendro_plot)

    } else {

      multiple_dendros <- base::length(c(methods_dist, methods_aggl)) > 2

      if(multiple_dendros){

        dendro_data <- NULL

        segment_df <-
          getDendroSegmentDf(
            object = object,
            methods_dist = methods_dist,
            methods_aggl = methods_aggl,
            k = k,
            h = h,
            type = type
          )

        if(facet_with == "grid"){

          facet_add_on <-
            ggplot2::facet_grid(rows = vars(aggl), cols = vars(dist), scales = "free")

        } else {

          facet_add_on <-
            ggplot2::facet_wrap(facets = dist ~ aggl, nrow = nrow, ncol = ncol, scales = "free")

        }

      } else {

        dendro_data <-
          getDendro(
            object = object,
            method_dist = methods_dist,
            method_aggl = methods_aggl,
            k = k,
            h = h,
            type = type
          )

        segment_df <- ggdendro::segment(x = dendro_data)

        facet_add_on <- NULL

      }

      # basic parameters
      ybreaks <- base::pretty(segment_df$y, n = 5)
      ymin <- base::min(segment_df$y)

      if("clust" %in% base::colnames(segment_df)){

        cluster_levels <- segment_df$clust %>% unique_safely()

        forced_adjustment <- "black"
        base::names(forced_adjustment) <- cluster_levels[1]

        clrp_adjust <- c(clrp_adjust, forced_adjustment)
        breaks <- cluster_levels[2:base::length(cluster_levels)]

        segment_add_on <-
          list(
            ggplot2::geom_segment(
              data = segment_df,
              mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend, color = clust),
              lineend = "round",
              size = branch_size,
              show.legend = TRUE
            ),
            scale_color_add_on(
              variable = segment_df$clust,
              clrp = clrp,
              clrp.adjust = clrp_adjust,
              breaks = breaks,
              ...)
          )

      } else {

        segment_add_on <-
          ggplot2::geom_segment(
            data = segment_df,
            mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
            lineend = "round",
            size = branch_size,
            color = branch_color
          )

      }

      # basic dendro plot
      dendro_plot <-
        ggplot2::ggplot() +
        segment_add_on +
        ggplot2::scale_x_continuous(breaks = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = NULL, y = NULL, color = "Cluster") +
        facet_add_on

      # flip coordinates if desired
      if(direction == "lr"){

        dendro_plot <- dendro_plot + ggplot2::coord_flip()

      }

      # add labels
      if(base::isTRUE(display_labels) & base::isFALSE(multiple_dendros)){

        label_params <-
          define_label_params(
            nbLabels = base::nrow(dendro_data$labels),
            labels.angle = labels_angle,
            labels.hjust = labels_hjust,
            direction = direction,
            fan = FALSE)

        dendro_data$labels$angle <- label_params$angle
        dendro_data$labels$y <- dendro_data$labels$y + labels_vjust

        dendro_plot <-
          dendro_plot +
          ggplot2::geom_text(
            data = ggdendro::label(dendro_data),
            mapping = ggplot2::aes(x = x, y = y, label = label, color = clust, angle = angle),
            hjust = label_params$hjust,
            nudge_y = labels_nudge,
            size = labels_size
          )
      }

      return(dendro_plot)

    }

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
                        col_alpha = 0.9,
                        col_color = "black",
                        col_fill = "steelblue",
                        display_line = TRUE,
                        line_alpha = 0.9,
                        line_color = "black",
                        line_size = 1.5,
                        display_points = TRUE,
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_size = 4.5){

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
      )

    if(base::is.numeric(ks)){

      res_df <- dplyr::filter(res_df, k %in% {{ks}})

    }

    nth <-
      (base::max(res_df[["k"]])/10) %>%
      base::floor()

    xlabs <-
      base::unique(res_df[["k"]]) %>%
      reduce_vec(x = ., nth = nth)

    # create basic plot
    p <-
      ggplot2::ggplot(data = res_df, mapping = ggplot2::aes(x = k, y = tot_withinss)) +
      ggplot2::scale_x_continuous(breaks = xlabs, labels = xlabs) +
      ggplot2::facet_wrap(facets = ~ method) +
      ggplot2::labs(y = NULL, x = "Centers (k)") +
      theme_statistics()


    # add layer
    if(base::isTRUE(display_cols)){

      p <-
        p +
        ggplot2::geom_col(
          fill = col_fill,
          color = col_color,
          alpha = col_alpha
          )

    }

    if(base::isTRUE(display_points)){

      p <-
        p +
        ggplot2::geom_point(
          alpha = pt_alpha,
          color = pt_color,
          size = pt_size
          )

    }

    if(base::isTRUE(display_line)){

      p <-
        p +
        ggplot2::geom_line(
          alpha = line_alpha,
          color = line_color,
          size = line_size,
          mapping = ggplot2::aes(group = 1)
          )

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
      scale_color_add_on(aes = "color", variable = "discrete", clrp = clrp, guide = "none") +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(color = "lightgrey"),
        legend.title = ggplot2::element_text(size = 12.5),
        plot.title = ggplot2::element_text(face = "bold", size = 16.5),
        plot.subtitle = ggplot2::element_text(size = 10)
      ) +
      ggplot2::labs(x = "Clustered Observations", y = "Silhouettte Width", color = NULL, fill = "Cluster")

  }
)

# -----

