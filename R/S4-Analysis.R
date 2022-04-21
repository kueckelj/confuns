
#' @include S4-generics.R
NULL

#' @include S4-Classification.R
NULL

#' @include S4-Correlation.R
NULL

#' @include S4-Clustering.R
NULL

#' @include S4-DimRed.R
NULL

#' @include S4-Outlier.R
NULL

#' @title The \code{AnalysisAspect}-class
#'
#' @description S4-class that provides the basic slots for any analysis
#' aspect such as clustering, dimensional reduction, outlier detection etc.
#'
#' @slot data data.frame. The data on which the analysis bases on.
#' @slot data_scaled data.frame The numeric data scaled via z-score.
#' @slot key_name character. The name of the variable that is used to identify
#' each observation uniquely.
#' @slot meta data.frame. Data that was part of the input data but is not supposed
#' to be included in analysis steps.
#' @slot methods list. A list of objects of S4-classes depending on the analysis
#' aspect.
#' @slot variables_grouping character. The names of all grouping variables
#' of the input data - variables of class character or factor. (Does not include
#' variable of slot @@key_name)
#' @slot variables_logical character. The names of all logical variables of
#' the input data.
#' @slot variables_numeric character. The names of all numeric variables
#' based on which outlier detection is conducted.
#'
#' @export
Analysis <- setClass(Class = "Analysis",
                     slots = list(
                       data = "data.frame",
                       data_scaled = "data.frame",
                       information = "list",
                       instructions = "list",
                       key_name = "character",
                       meta = "data.frame",
                       classification = "Classification",
                       correlation = "Correlation",
                       clustering = "Clustering",
                       dimred = "DimRed",
                       outlierdetection = "OutlierDetection",
                       variables_grouping = "character",
                       variables_meta = "character",
                       variables_logical = "character",
                       variables_numeric = "character",
                       version = "list")
)



# r-objects ---------------------------------------------------------------

analysis_instructions <- list("cluster_with" = c("data", "pca"))


analysis_aspects <-
  list(
    classification = c(),
    clustering = c("hierarchical", "kmeans", "pam"),
    dimred = c("pca", "tsne", "umap")
  )


# functions ---------------------------------------------------------------


#' @title Initiate analysis
#'
#' @description Sets up an object of class \code{AnalysisAspect}.
#'
#' @param data Data.frame containing the data to be analyzed.
#' @param key_name The key variable. If NULL, the key variable is created
#' either by using the rownames and - if rownames are invalid - by combining
#' input for argument \code{key_prefix} with the rownumbers.
#' @param key_prefix Character value. The prefix for the artificial
#' key variable.
#' @param meta_names Names of the data.frame of \code{data} that are supposed
#' to be treated as meta data. Meta data is not integrated in any form
#' of analysis.
#' @param analysis_aspect The actual analysis aspect. Use \code{validAnalysisAspects()}
#' to obtain all valid input options.
#' @param verbose
#'
#' @return An object of class specified in \code{analysis_aspect}.
#'
#' @export
#'
initiateAnalysis <- function(data,
                             key_name,
                             key_prefix = NULL,
                             meta_names = character(0),
                             cluster_with = "data",
                             lgl_to_group = TRUE,
                             verbose = TRUE){

  # input check
  is_value(x = key_name, mode = "character", skip.allow = TRUE, skip.val = NULL)

  data <- base::as.data.frame(data)

  if(base::isTRUE(lgl_to_group)){

    data <- logical_to_group(data, skip = meta_names)

  }

  df <-
    base::as.data.frame(data) %>%
    dplyr::select(-dplyr::all_of(meta_names))

  variables_grouping <-
    dplyr::select(df, -dplyr::any_of(key_name)) %>%
    dplyr::select_if(.predicate = ~ base::is.character(.x) | base::is.factor(.x)) %>%
    base::colnames()

  variables_logical <-
    dplyr::select(df, -dplyr::any_of(key_name)) %>%
    dplyr::select_if(.predicate = ~ base::is.logical(.x)) %>%
    base::colnames()

  variables_numeric <-
    dplyr::select_if(df, .predicate = base::is.numeric) %>%
    base::colnames()

  variables_meta <- meta_names

  object <-
    methods::new(
      Class = "Analysis",
      variables_grouping = variables_grouping,
      variables_logical = variables_logical,
      variables_meta = variables_meta,
      variables_numeric = variables_numeric
    )

  object <-
    setData(
      object = object,
      data = data,
      key_name = key_name,
      key_prefix = key_prefix,
      meta_names = meta_names,
      verbose = verbose
    )

  object <- scaleData(object, na_rm = TRUE)

  # set instructions
  object <- setInstruction(object, cluster_with = cluster_with)

  return(object)

}



# generics ----------------------------------------------------------------


#' @title Obtain objects of class \code{AnalysisAspect}
#'
#' @description Extracts objects of class \code{AnalysisAspect} and adds
#' content of slot @@data, @@data_scaled and @@meta.
#'
#' @inherit argument_dummy params
#'
#' @return An object of class \code{AnalysisAspect}.
#' @export
#'
setGeneric(name = "getAnalysisAspect", def = function(object, ...){

  standardGeneric(f = "getAnalysisAspect")

})


#' @title Set objects of class \code{AnalysisAspect}
#'
#' @description Sets objects of class \code{AnalysisAspect} and removes
#' content of slots @@data, @@data_scaled and @@meta.
#'
#' @inherit argument_dummy params
#'
#' @return The input object.
#' @export
#'
setGeneric(name = "setAnalysisAspect", def = function(object, ...){

  standardGeneric(f = "setAnalysisAspect")

})


#' @title Set instruction
#'
#' @description Sets instructions to adjust automatic behaviour.
#'
#' @inherit argument_dummy params
#'
#' @return The input object.
#' @export
#'
setGeneric(name = "setInstruction", def = function(object, ...){

  standardGeneric(f = "setInstruction")

})




# methods -----------------------------------------------------------------


#' @rdname addGroupingVars
#' @export
setMethod(
  f = "addGroupingVars",
  signature = "Analysis",
  definition = function(object, grouping_df){

    names_grouping_df <-
      dplyr::select(grouping_df, -dplyr::all_of(object@key_name)) %>%
      base::names()

    ovlp <- base::intersect(x = names_grouping_df, y = object@variables_grouping)

    if(base::length(ovlp) >= 1){

      if(!base::isTRUE(overwrite)){

        olvp_ref <- scollapse(ovlp)
        ref <- adapt_reference(ovlp, "is", "are")

        give_feedback(
          msg = glue::glue("The {ref1} '{ovlp_ref}' {ref2} already present. Set `overwrite` to TRUE in order to overwrite."),
          fdb.fn = "stop"
        )

      } else {

        object@variables_grouping <-
          vselect(object@variables_grouping, -dplyr::any_of(ovlp))

        object@data <-
          dplyr::select(object@data, -dplyr::any_of(ovlp))

      }

    }

    object@data <-
      dplyr::left_join(
        x = object@data,
        y = grouping_df,
        by = object@key_name
      )

    object@variables_grouping <-
      c(object@variables_grouping, names_grouping_df)

    return(object)

  }
)

#' @rdname agglomerateHierarchicalTrees
#' @export
setMethod(
  f = "agglomerateHierarchicalTrees",
  signature = "Analysis",
  definition = function(object,
                        methods_dist = methods_dist,
                        methods_aggl = methods_aggl,
                        verbose = verbose,
                        ...){

    aa <-
      getAnalysisAspect(object, aspect = "clustering") %>%
      agglomerateHierarchicalTrees(
        methods_dist = methods_dist,
        methods_aggl = methods_aggl,
        verbose = verbose
      )

    object <- setAnalysisAspect(object, aa = aa)

    return(object)

  }
)

#' @rdname computeClusteringHclust
#' @export
setMethod(
  f = "computeClusteringHclust",
  signature = "Analysis",
  definition = function(object,
                        methods_dist = "euclidean",
                        methods_aggl = "ward.D",
                        verbose = TRUE){

    aa <-
      getAnalysisAspect(object, aspect = "clustering") %>%
      computeClusteringHclust(
        methods_dist = methods_dist,
        methods_aggl = methods_aggl,
        verbose = verbose
      )

    object <- setAnalysisAspect(object, aa = aa)

    return(object)

  }
)

#' @rdname computeClusteringKmeans
#' @export
setMethod(
  f = "computeClusteringKmeans",
  signature = "Analysis",
  definition = function(object,
                        ks,
                        methods_kmeans = "Hartigan-Wong",
                        verbose = TRUE,
                        ...){

    aa <-
      getAnalysisAspect(object, aspect = "clustering") %>%
      computeClusteringKmeans(
        ks = ks,
        methods_kmeans = methods_kmeans,
        verbose = verbose,
        ...
      )

    object <- setAnalysisAspect(object, aa = aa)

    return(object)

  }
)

#' @rdname computeClusteringPam
#' @export
setMethod(
  f = "computeClusteringPam",
  signature = "Analysis",
  definition = function(object,
                        ks,
                        methods_pam = "euclidean",
                        verbose = TRUE,
                        ...){

    aa <-
      getAnalysisAspect(object, aspect = "clustering") %>%
      computeClusteringPam(
        ks = ks,
        methods_pam = methods_pam,
        verbose = verbose
      )

    object <- setAnalysisAspect(object, aa = aa)

    return(object)

  }
)

#' @rdname computeCorrelation
#' @export
setMethod(
  f = "computeCorrelation",
  signature = "Analysis",
  definition = function(object,
                        across = NULL,
                        across_subset = NULL,
                        methods_corr = "pearson",
                        verbose = TRUE){

    aa <-
      getAnalysisAspect(object, aspect = "correlation") %>%
      computeCorrelation(
        across = across,
        across_subset = across_subset,
        methods_corr = methods_corr,
        verbose = verbose
      )

    object <- setAnalysisAspect(object, aa = aa)

    return(object)

  }
)


#' @rdname computeDistanceMatrices
#' @export
setMethod(
  f = "computeDistanceMatrices",
  signature = "Analysis",
  definition = function(object,
                        methods_dist = "euclidean",
                        p = 2,
                        force = FALSE,
                        verbose = TRUE){

    aa <-
      getAnalysisAspect(object, aspect = "clustering") %>%
      computeDistanceMatrices(
        methods_dist = methods_dist,
        p = p,
        force = force,
        verbose = verbose
      )

    object <- setAnalysisAspect(object, aa = aa)

    return(object)

  }
)

#' @rdname computePCA
#' @export
setMethod(
  f = "computePCA",
  signature = "Analysis",
  definition = function(object, n_dims, verbose = TRUE){

    aa <-
      getAnalysisAspect(object, aspect = "dimred") %>%
      computePCA(object = ., n_dims = n_dims, verbose = verbose)

    object <- setAnalysisAspect(object = object, aa = aa)

    return(object)

  }
)

#' @rdname computeTSNE
#' @export
setMethod(
  f = "computeTSNE",
  signature = "Analysis",
  definition = function(object, n_dims, verbose = TRUE){

    aa <-
      getAnalysisAspect(object, aspect = "dimred") %>%
      computeTSNE(object = , n_dims = n_dims, verbose = verbose)

    object <- setAnalysisAspect(object, aa = aa)

    return(object)

  }
)


#' @rdname computeUMAP
#' @export
setMethod(
  f = "computeUMAP",
  signature = "Analysis",
  definition = function(object, verbose = TRUE){

    aa <-
      getAnalysisAspect(object, aspect = "dimred") %>%
      computeUMAP(object = ., verbose = verbose)

    object <- setAnalysisAspect(object, aa = aa)

    return(object)

  }
)


#' @rdname detectOutliers
#' @export
setMethod(
  f = "detectOutliers",
  signature = "Analysis",
  definition = function(object,
                        method,
                        across = NULL,
                        verbose = TRUE){

    aa <-
      getAnalysisAspect(object, aspect = "outlierdetection") %>%
      detectOutliers(
        method = method,
        across = across,
        verbose = verbose
      )

    object <- setAnalysisAspect(object, aa = aa)

    return(object)


  }
)



#' @param aspect Character value. Name of the analsis aspect. Use \code{validAnalysisAspects()}
#' to obtain all valid input options.
#' @rdname getAnalysisAspect
#' @export
setMethod(
  f = "getAnalysisAspect",
  signature = "Analysis",
  definition = function(object, aspect, verbose = TRUE){

    check_one_of(
      input = aspect,
      against = base::names(analysis_aspects)
    )

    aa <- methods::slot(object = object, name = aspect)

    aa_class <- base::class(aa)

    shared_slots <-
      base::intersect(
        x = methods::slotNames(object),
        y = methods::slotNames(aa)
      )

    for(slot in shared_slots){

      methods::slot(object = aa, name = slot) <-
        methods::slot(objec = object, name = slot)

    }

    return(aa)

  }
)

#' @rdname getAvgSilWidthsDf
#' @export
setMethod(
  f = "getAvgSilWidthsDf",
  signature = "Analysis",
  definition = function(object,
                        ks,
                        methods_pam = "euclidean"){

    getAnalysisAspect(object, aspect = "clustering") %>%
      getAvgSilWidthsDf(
        ks = ks,
        methods_pam = methods_pam
      )

  }
)


#' @rdname getClusterVarsHclust
#' @export
setMethod(
  f = "getClusterVarsHclust",
  signature = "Analysis",
  definition = function(object,
                        ks = NULL,
                        hs = NULL,
                        methods_dist = "euclidean",
                        methods_aggl = "ward.D",
                        prefix = "",
                        naming_k = "{method_dist}_{method_aggl}_k{k}",
                        naming_h = "{method_dist}_{method_aggl}_h{h}"){

    getAnalysisAspect(object, aspect = "clustering") %>%
      getClusterVarsHclust(
        ks = ks,
        hs = hs,
        methods_dist = methods_dist,
        prefix = prefix,
        naming_k = naming_k,
        naming_h = naming_h
      )

  }
)

#' @rdname getClusterVarsKmeans
#' @export
setMethod(
  f = "getClusterVarsKmeans",
  signature = "Analysis",
  definition = function(object,
                        ks,
                        methods_kmeans = "Hartigan-Wong",
                        prefix = "",
                        naming = "{method_kmeans}_k{k}"){

    getAnalysisAspect(object, aspect = "clustering") %>%
      getClusterVarsKmeans(
        ks = ks,
        methods_kmeans = methods_kmeans,
        prefix = prefix,
        naming = naming
      )

  }
)

#' @rdname getClusterVarsPam
#' @export
setMethod(
  f = "getClusterVarsPam",
  signature = "Analysis",
  definition = function(object,
                        ks,
                        methods_pam = "euclidean",
                        prefix = "",
                        naming = "{method_pam}_k{k}"){

    getAnalysisAspect(object, aspect = "clustering") %>%
      getClusterVarsPam(
        ks = ks,
        methods_pam = methods_pam,
        prefix = prefix,
        naming = naming
      )

  }
)

#' @rdname getCorrDf
#' @export
setMethod(
  f = "getCorrDf",
  signature = "Analysis",
  definition = function(object,
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        pval_threshold = 0.05,
                        type = "complete",
                        diagonal = TRUE,
                        distinct = FALSE,
                        digits = 2,
                        verbose = TRUE,
                        sep = " & ",
                        ...){

    getAnalysisAspect(object, aspect = "correlation") %>%
      getAnalysisAspect(
        method_corr = method_corr,
        across = across,
        across_subset = across_subset,
        pval_threshold = pval_treshold,
        type = type,
        diagonal = diagonal,
        distinct = distinct,
        digits = digits,
        verbose = verbose,
        sep = sep,
        ...
      )
  }
)

#' @rdname getCorrDf
#' @export
setMethod(
  f = "getCorrMtr",
  signature = "Analysis",
  definition = function(object,
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        type = "complete",
                        diagonal = TRUE,
                        flatten = TRUE){

    getAnalysisAspect(object, aspect = "correlation") %>%
      getCorrMtr(
        method_corr = method_corr,
        across = across,
        across_subset = across_subset,
        type = type,
        diagonal = diagonal,
        flatten = flatten
      )

  }
)

#' @rdname getDendro
#' @export
setMethod(
  f = "getDendro",
  signature = "Analysis",
  definition = function(object,
                        method_dist = "euclidean",
                        method_aggl = "ward.D",
                        k = NULL,
                        h = NULL,
                        type = "rectangle"){

    getAnalysisAspect(object, aspect = "clustering") %>%
      getDendro(
        method_dist = method_dist,
        method_aggl = method_aggl,
        k = k,
        h = h,
        type = type
      )

  }
)

#' @rdname getDendroSegmentDf
#' @export
setMethod(
  f = "getDendroSegmentDf",
  signature = "Analysis",
  definition = function(object,
                        methods_dist = "eucldidean",
                        methods_aggl = "ward.D",
                        k = NULL,
                        h = NULL,
                        type = "rectangle"){

    getAnalysisAspect(object, aspect = "clustering") %>%
      getDendroSegmentDf(
        methods_dist = methods_dist,
        methods_aggl = methods_aggl,
        k = k,
        h = h,
        type = type
      )

  }
)

#' @rdname getDf
#' @export
setMethod(
  f = "getDf",
  signature = "Analysis",
  definition = function(object,
                        complete = TRUE,
                        grouping = FALSE,
                        logical = FALSE,
                        numeric = FALSE,
                        meta = FALSE){

    if(base::any(c(grouping, logical, numeric, meta))){

      complete <- FALSE

    }

    if(base::isTRUE(complete)){

      grouping <- TRUE
      logical <- TRUE
      numeric <- TRUE
      meta <- TRUE

    }

    var_names <- object@key_name

    if(base::isTRUE(grouping)){

      var_names <- c(var_names, object@variables_grouping)

    }

    if(base::isTRUE(logical)){

      var_names <- c(var_names, object@variables_logical)

    }

    if(base::isTRUE(numeric)){

      var_names <- c(var_names, object@variables_numeric)

    }

    df_out <-
      dplyr::select(object@data, dplyr::all_of(x = var_names)) %>%
      tibble::as_tibble()

    if(base::isTRUE(meta)){

      df_out <- dplyr::left_join(x = df_out, y = object@meta, by = object@key_name)

    }

    return(df_out)

  })

#' @rdname getDistMtr
#' @export
setMethod(
  f = "getDistMtr",
  signature = "Analysis",
  definition = function(object,
                        method_dist = "euclidean",
                        stop_if_null = FALSE){

    getAnalysisAspect(object, aspect = "clustering") %>%
      getDistMtr(
        method_dist = method_dist,
        stop_if_null = stop_if_null
        )

  }
)

#' @rdname getEmbeddingDf
#' @export
setMethod(
  f = "getEmbeddingDf",
  signature = "Analysis",
  definition = function(object,
                        method_dimred = "pca",
                        numeric = FALSE,
                        numeric_scaled = FALSE,
                        grouping = FALSE,
                        logical = FALSE,
                        complete = FALSE,
                        sfhit = FALSE){

    getAnalysisAspect(object, aspect = "dimred") %>%
      getEmbeddingDf(
        method_dimred = method_dimred,
        numeric = numeric,
        numeric_scaled = numeric_scaled,
        grouping = grouping,
        logical = logical,
        shift = shift
      )

  }
)

#' @rdname getHclust
#' @export
setMethod(
  f = "getHclust",
  signature = "Analysis",
  definition = function(object,
                        method_dist = "eucldidean",
                        method_aggl = "ward.D",
                        stop_if_null = TRUE){

    getAnalysisAspect(object, aspect = "clustering") %>%
      getHclust(
        method_dist = method_dist,
        method_aggl = method_aggl,
        stop_if_null = stop_if_null
      )
  }
)

#' @rdname getKeyDf
#' @export
setMethod(
  f = "getKeyDf",
  signature = "Analysis",
  definition = function(object, ...){

    object@meta[object@key_name] %>%
      tibble::as_tibble()

  }
)

#' @rdname getKmeans
#' @export
setMethod(
  f = "getKmeans",
  signature = "Analysis",
  definition = function(object,
                        k,
                        method_kmeans = "Hartigan-Wong",
                        stop_if_null = TRUE){

    getAnalysisAspect(object, aspect = "clustering") %>%
      getKmeans(
        k = k,
        method_kmeans = method_kmeans,
        stop_if_null = stop_if_null
      )

  }
)

#' @rdname getMtr
#' @export
setMethod(
  f = "getMtr",
  signature = "Analysis",
  definition = function(object, ...){

    getDf(object, numeric = TRUE) %>%
      tibble::column_to_rownames(var = object@key_name) %>%
      base::as.matrix()

  }
)

#' @rdname getOutlierIDs
#' @export
setMethod(
  f = "getOutlierIDs",
  signature = "Analysis",
  definition = function(object,
                        variables = NULL,
                        across = NULL,
                        across_subset = NULL,
                        flatten = FALSE){

    getAnalysisAspect(object, aspect = "outlierdetection") %>%
      getOutlierIDs(
        variables = variables,
        across = across,
        across_subset = across_subset,
        flatten = flatten
      )
  }
)

#' @rdname getOutlierResults
#' @export
setMethod(
  f = "getOutlierResults",
  signature = "Analysis",
  definition = function(object,
                        method = "IQR",
                        across = NULL,
                        verbose = TRUE){

      getAnalysisAspect(object, aspect = "outlierdetection") %>%
      getOutlierResults(
        method = method,
        across = across,
        verbose = verbose
        )

  }
)

#' @rdname getRcorr
#' @export
setMethod(
  f = "getRcorr",
  signature = "Analysis",
  definition = function(object,
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        as_list = FALSE,
                        stop_if_null = TRUE){

    getAnalysisAspect(object, aspect = "correlation") %>%
      getRcorr(
        method_corr = method_corr,
        across = across,
        across_subset = across_subset,
        as_list = as_list,
        stop_if_null = stop_if_null
      )

  }
)


#' @rdname getScaledDf
#' @export
setMethod(
  f = "getScaledDf",
  signature = "Analysis",
  definition = function(object, na_rm = TRUE){

    if(purrr::is_empty(object@data_scaled)){

      object <- scaleData(object, na_rm = na_rm)

    }

    return(object@data_scaled)

  }
)

#' @rdname getScaledMtr
#' @export
setMethod(
  f = "getScaledMtr",
  signature = "Analysis",
  definition = function(object, na_rm = TRUE){

    getScaledDf(object, na_rm = na_rm) %>%
      tibble::rownames_to_column(var = object@key_name) %>%
      base::as.matrix()

  }
)

#' @rdname getSilWidthsDf
#' @export
setMethod(
  f = "getSilWidthsDf",
  signature = "Analysis",
  definition = function(object,
                        ks,
                        method_pam = "euclidean",
                        format = "long"){

    getAnalysisAspect(object, aspect = "clustering") %>%
      getSilWidthsDf(
        ks = ks,
        method_pam = method_pam,
        format = format
      )

  }
)

#' @rdname getVariableNames
#' @export
setMethod(
  f = "getVariableNames",
  signature = "Analysis",
  definition = function(object,
                        types = c("key", "numeric", "grouping", "logical", "meta"),
                        unname = FALSE){


    grouping_vars <-
      object@variables_grouping %>%
      purrr::set_names(nm = base::rep("grouping", base::length(.)))

    numeric_vars <-
      object@variables_numeric %>%
      purrr::set_names(nm = base::rep("numeric", base::length(.)))

    logical_vars <-
      object@variables_logical %>%
      purrr::set_names(nm = base::rep("logical", base::length(.)))

    key <- object@key_name %>% purrr::set_names(nm = "key")

    meta_vars <-
      object@meta %>%
      dplyr::select(-key) %>%
      base::colnames() %>%
      purrr::set_names(nm = base::rep("meta", base::length(.)))

    out <- c(grouping_vars, numeric_vars, logical_vars, key, meta_vars)

    if(base::is.character(types)){

      out <- out[base::names(out) %in% types]

    }

    if(base::isTRUE(unname)){

      out <- base::unname(out)

    }

    return(out)

  }
)

#' @rdname plotAvgSilWidths
#' @export
setMethod(
  f = "plotAvgSilWidths",
  signature = "Analysis",
  definition = function(object,
                        ks,
                        methods_pam = "euclidean",
                        color = "steelblue",
                        display_cols = TRUE,
                        display_points = TRUE,
                        display_line = TRUE,
                        ncol = NULL,
                        nrow = NULL){

    getAnalysisAspect(object, aspect = "clustering") %>%
      plotAvgSilWidths(
        ks = ks,
        methods_pam = methods_pam,
        color = color,
        display_cols = display_cols,
        display_points = display_points,
        display_line = display_line,
        ncol = ncol,
        nrow = nrow
      )
  }
)


#' @rdname plotBoxplot
#' @export
setMethod(
  f = "plotBoxplot",
  signature = "Analysis",
  definition = function(object,
                        variables,
                        phase = NULL,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        test_groupwise = NULL,
                        test_pairwise = NULL,
                        ref_group = NULL,
                        step_increase = 0.01,
                        vjust = 0,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        display_points = FALSE,
                        pt_alpha = 0.8,
                        pt_clr = "black",
                        pt_num = 100,
                        pt_size = 1.25,
                        pt_shape = 21,
                        verbose = TRUE,
                        ...){

    df <- getDf(object, complete = TRUE)

    plot_boxplot(
      df = df,
      variables = variables,
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      test.pairwise = test_pairwise,
      test.groupwise = test_groupwise,
      ref.group = ref_group,
      step.increase = step_increase,
      vjust = vjust,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      display.points = display_points,
      pt.alpha = pt_alpha,
      pt.color = pt_clr,
      pt.num = pt_num,
      pt.shape = pt_shape,
      pt.size = pt_size,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      verbose = verbose,
      ...
    )


  }
)


#' @rdname plotCorrplot
#' @export
setMethod(
  f = "plotCorrplot",
  signature = "Analysis",
  definition = function(object,
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        variables_subset = variables_subset,
                        relevel = FALSE,
                        pval_threshold = 0.05,
                        type = "lower",
                        diagonal = TRUE,
                        color_low = "darkred",
                        color_high = "steelblue",
                        color_limits = c(-1, 1),
                        shape = "tile",
                        size_by_corr = TRUE,
                        size_max = 15,
                        size_limits = c(-1, 1),
                        display_value = TRUE,
                        values_alpha = 0.9,
                        values_color = "black",
                        values_digits = 2,
                        values_size = 4,
                        display_grid = TRUE,
                        grid_color = "grey",
                        grid_size = 0.5,
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE){

    getAnalysisAspect(object, aspect = "correlation") %>%
      plotCorrplot(
        method_corr = method_corr,
        across = across,
        across_subset = across_subset,
        variables_subset = variables_subset,
        relevel = relevel,
        pval_threshold = pval_threshold,
        type = type,
        diagonal = diagonal,
        color_low = color_low,
        color_high = color_high,
        color_limits = color_limits,
        shape = shape,
        size_by_corr = size_by_corr,
        size_max = size_max,
        size_limits = size_limits,
        display_values = display_values,
        values_alpha = values_alpha,
        values_color = values_color,
        values_digits = values_digits,
        grid_color = grid_color,
        grid_size = grid_size,
        nrow = nrow,
        ncol = ncol,
        verbose = verbose
      )

  }
)

#' @rdname plotDendrogram
#' @export
setMethod(
  f = "plotDendrogram",
  signature = "Analysis",
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
                        ncol = NULL){

    getAnalysisAspect(object, aspect = "clustering") %>%
      plotDendrogram(
        methods_dist = methods_dist,
        methods_aggl = methods_aggl,
        k = k,
        h = h,
        type = type,
        facet_with = facet_with,
        direction = direction,
        branch_color = branch_color,
        branch_size = branch_size,
        display_labels = display_labels,
        labels_angle = labels_angle,
        labels_hjust = labels_hjust,
        labels_nudge = labels_nudge,
        labels_size = labels_size,
        labels_vjust = labels_vjust,
        display_legend = display_legend,
        display_title = display_title,
        clrp = clrp,
        clrp_adjust = clrp_adjust,
        simple = simple,
        nrow = nrow,
        ncol = ncol
      )

  }
)

#' @rdname plotDensityplot
#' @export
setMethod(
  f = "plotDensityplot",
  signature = "Analysis",
  definition = function(object,
                        variables,
                        phase = NULL,
                        across = NULL,
                        across_subset = NULL,
                        relevel = NULL,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE,
                        ...){

    df <- getDf(object, complete = TRUE)

    plot_density(
      df = df,
      variables = variables,
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      scales = scales,
      display.facets = display_facets,
      nrow = nrow,
      ncol = ncol,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      verbose = verbose,
      ...
    )

  }
)

#' @rdname plotDensityplot
#' @export
setMethod(
  f = "plotHistogram",
  signature = "Analysis",
  definition = function(object,
                        variables,
                        phase = NULL,
                        across = NULL,
                        across_subset = NULL,
                        relevel = NULL,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE,
                        ...){

    df <- getDf(object, complete = TRUE)

    plot_histogram(
      df = df,
      variables = variables,
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      scales = scales,
      display.facets = display_facets,
      nrow = nrow,
      ncol = ncol,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      verbose = verbose,
      ...
    )

  }
)

#' @rdname plotDensityplot
#' @export
setMethod(
  f = "plotRidgeplot",
  signature = "Analysis",
  definition = function(object,
                        variables,
                        phase = NULL,
                        across = NULL,
                        across_subset = NULL,
                        relevel = NULL,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE,
                        ...){

    df <- getDf(object, complete = TRUE)

    plot_ridgeplot(
      df = df,
      variables = variables,
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      scales = scales,
      display.facets = display_facets,
      nrow = nrow,
      ncol = ncol,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      verbose = verbose,
      ...
    )

  }
)

#' @rdname plotPCA
#' @export
setMethod(
  f = "plotPCA",
  signature = "Analysis",
  definition = function(object,
                        n_dims = 2,
                        alpha_by = NULL,
                        color_by = NULL,
                        shape_by = NULL,
                        size_by = NULL,
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_fill = "black",
                        pt_shape = 19,
                        pt_size = 1,
                        color_aes = "color",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        clrsp = "inferno",
                        ...){

    getAnalysisAspect(object, aspect = "dimred") %>%
      plotPCA(
        n_dims = n_dims,
        alpha_by = alpha_by,
        color_by = color_by,
        shape_by = shape_by,
        size_by = size_by,
        pt_alpha = pt_alpha,
        pt_color = pt_color,
        pt_fill = pt_fill,
        pt_shape = pt_shape,
        pt_size = pt_size,
        color_aes = color_aes,
        clrp = clrp,
        clrp_adjust = clrp_adjust,
        clrsp = clrsp,
        ...
      )
  }
)

#' @rdname plotScatterplot
#' @export
setMethod(
  f = "plotScatterplot",
  signature = "AnalysisAspect",
  definition = function(object,
                        x,
                        y,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        ncol = NULL,
                        nrow = NULL,
                        scales = "fixed",
                        space = "fixed",
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_clrp = "milo",
                        pt_fill = "black",
                        pt_shape = 19,
                        pt_size = 1.5,
                        color_aes = "color",
                        color_by = NULL,
                        color_trans = "identity",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        clrsp = "inferno",
                        order_by = NULL,
                        order_desc = FALSE,
                        shape_by = NULL,
                        size_by = NULL,
                        display_smooth = FALSE,
                        smooth_alpha = 0.9,
                        smooth_color = "blue",
                        smooth_method = "lm",
                        smooth_se = FALSE,
                        smooth_size = 1,
                        display_corr = FALSE,
                        corr_method = "pearson",
                        corr_p_min = 0.00005,
                        corr_pos_x = NULL,
                        corr_pos_y = NULL,
                        corr_text_sep = "\n",
                        corr_text_size = 1,
                        transform_with = NULL,
                        ...){

    df <- getDf(object, complete = TRUE)

    plot_scatterplot(
      df = df,
      x = x,
      y = y,
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      ncol = ncol,
      nrow = nrow,
      scales = scales,
      space = space,
      pt.alpha = pt_alpha,
      pt.color = pt_color,
      pt.clrp = pt_clrp,
      pt.fill = pt_fill,
      pt.shape = pt_shape,
      pt.size = pt_size,
      color.aes = color_aes,
      color.by = color_by,
      color.trans = color_trans,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      clrsp = clrsp,
      order.by = order_by,
      order.desc = order_desc,
      shape.by = shape_by,
      size.by = size_by,
      display.smooth = display_smooth,
      smooth.alpha = smooth_alpha,
      smooth.color = smooth_color,
      smooth.method = smooth_method,
      smooth.se = smooth_se,
      smooth.size = smooth_size,
      display.corr = display_corr,
      corr.method = corr_method,
      corr.p.min = corr_p_min,
      corr.pos.x = corr_pos_x,
      corr.pos.y = corr_pos_y,
      corr.text.sep = "\n",
      corr.text.size = corr_text_size,
      transform.with = transform_with,
      ...
    )

  }
)

#' @rdname plotScreeplot
#' @export
setMethod(
  f = "plotScreeplot",
  signature = "Analysis",
  definition = function(object,
                        method_kmeans = "Hartigan-Wong",
                        ks = NULL,
                        color = "steelblue",
                        display_cols = TRUE,
                        display_points = TRUE,
                        display_line = TRUE){

    getAnalysisAspect(object, aspect = "clustering") %>%
      plotScreeplot(
        methods_kmeans = methods_kmeans,
        ks = ks,
        color = color,
        display_cols = display_cols,
        display_line = display_line,
        display_points = display_points
      )

  }
)

#' @rdname plotSilWidths
#' @export
setMethod(
  f = "plotSilWidths",
  signature = "Analysis",
  definition = function(object,
                        ks,
                        method_pam = "euclidean",
                        clrp = "milo",
                        ncol = NULL,
                        nrow = NULL,
                        vebose = TRUE){

    getAnalysisAspect(object, aspect = "clustering") %>%
      plotSilWidths(
        ks = ks,
        method_pam = method_pam,
        clrp = clrp,
        ncol = ncol,
        nrow = nrow,
        verbose = verbose
      )
  }
)

#' @rdname plotTSNE
#' @export
setMethod(
  f = "plotTSNE",
  signature = "Analysis",
  definition = function(object,
                        n_dims = 2,
                        alpha_by = NULL,
                        color_by = NULL,
                        shape_by = NULL,
                        size_by = NULL,
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_fill = "black",
                        pt_shape = 19,
                        pt_size = 1,
                        color_aes = "color",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        clrsp = "inferno",
                        ...){

    getAnalysisAspect(object, aspect = "dimred") %>%
      plotTSNE(
        n_dims = n_dims,
        alpha_by = alpha_by,
        color_by = color_by,
        shape_by = shape_by,
        size_by = size_by,
        pt_alpha = pt_alpha,
        pt_color = pt_color,
        pt_fill = pt_fill,
        pt_shape = pt_shape,
        pt_size = pt_size,
        color_aes = color_aes,
        clrp = clrp,
        clrp_adjust = clrp_adjust,
        clrsp = clrsp,
        ...
      )
  }
)

#' @rdname plotUMAP
#' @export
setMethod(
  f = "plotUMAP",
  signature = "Analysis",
  definition = function(object,
                        alpha_by = NULL,
                        color_by = NULL,
                        shape_by = NULL,
                        size_by = NULL,
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_fill = "black",
                        pt_shape = 19,
                        pt_size = 1,
                        color_aes = "color",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        clrsp = "inferno",
                        ...){

    getAnalysisAspect(object, aspect = "dimred") %>%
      plotUMAP(
        alpha_by = alpha_by,
        color_by = color_by,
        shape_by = shape_by,
        size_by = size_by,
        pt_alpha = pt_alpha,
        pt_color = pt_color,
        pt_fill = pt_fill,
        pt_shape = pt_shape,
        pt_size = pt_size,
        color_aes = color_aes,
        clrp = clrp,
        clrp_adjust = clrp_adjust,
        clrsp = clrsp,
        ...
      )
  }
)

#' @rdname plotBoxplot
#' @export
setMethod(
  f = "plotViolinplot",
  signature = "Analysis",
  definition = function(object,
                        variables,
                        phase = NULL,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        test_groupwise = NULL,
                        test_pairwise = NULL,
                        ref_group = NULL,
                        step_increase = 0.01,
                        vjust = 0,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        display_points = FALSE,
                        pt_alpha = 0.8,
                        pt_clr = "black",
                        pt_num = 100,
                        pt_size = 1.25,
                        pt_shape = 21,
                        verbose = TRUE,
                        ...){

    df <- getDf(object, complete = TRUE)

    plot_violinplot(
      df = df,
      variables = variables,
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      test.pairwise = test_pairwise,
      test.groupwise = test_groupwise,
      ref.group = ref_group,
      step.increase = step_increase,
      vjust = vjust,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      display.points = display_points,
      pt.alpha = pt_alpha,
      pt.color = pt_clr,
      pt.num = pt_num,
      pt.shape = pt_shape,
      pt.size = pt_size,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      verbose = verbose,
      ...
    )


  }
)

#' @rdname scaleData
#' @export
setMethod(
  f = "scaleData",
  signature = "Analysis",
  definition = function(object, na_rm = TRUE, verbose = TRUE){

    give_feedback(msg = "Scaling data.", verbose = verbose)

    object@data_scaled <-
      getDf(object, numeric = TRUE, complete = FALSE) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::all_of(x = object@variables_numeric),
          .fns = normalize_zscore,
          na.rm = na_rm
        )
      )

    give_feedback(msg = "Done.", verbose = verbose)


    return(object)
  }
)

#' @param aa An object of class \code{AnalysisAspect}.
#' @rdname setAnalysisAspect
#' @export
setMethod(
  f = "setAnalysisAspect",
  signature = "Analysis",
  definition = function(object, aa){

    aa@data <- base::data.frame()
    aa@data_scaled <- base::data.frame()
    aa@meta <- base::data.frame()

    slot <- base::tolower(base::class(aa))

    methods::slot(object = object, name = slot) <- aa

    return(object)

  }
)

#' @rdname setData
#' @export
setMethod(
  f = "setData",
  signature = "Analysis",
  definition = function(object,
                        data,
                        key_name = NULL,
                        key_prefix = "id",
                        meta_names = character(0),
                        verbose = TRUE){

    object <-
      set_data_hlpr(
        object = object,
        data = data,
        key.name = key_name,
        key.prefix = key_prefix,
        meta.names = meta_names,
        slot.data = "data",
        slot.key.name = "key_name",
        slot.meta = "meta",
        verbose = verbose
      )

    return(object)

  }
)

#' @rdname setInstruction
#' @export
setMethod(
  f = "setInstruction",
  signature = "Analysis",
  definition = function(object, ...){

    instructions <- keep_named(list(...))

    check_one_of(
      input = base::names(instructions),
      against = base::names(analysis_instructions),
      ref.input = "instructions"
    )

    for(instr in base::names(instructions)){

      x <- instructions[[instr]]

      confuns::give_feedback(
        msg = glue::glue("Setting instruction {instr}: {x}")
      )

      object@instructions[[instr]] <- x

    }

    return(object)

  }
)













