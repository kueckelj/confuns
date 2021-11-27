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

DimRed <- setClass(Class = "DimRed",
                   slots = list(),
                   contains = "AnalysisAspect"
)


#' @title The \code{DimRedMethod} class
#'
#' @description Abstracts conduction of different dimensional reduction
#' methods or algorithms.
#'
#' @param embedding Data.frame. Contains the dimensional reduction embedding.
#' Variable names correspond to the method suffixed with a number.
#' @param key_name Character. Name of the key variable.
#' @param method Character. Name of the dimensional reduction method.
DimRedMethod <- setClass(Class = "DimRedMethod",
                         slots = list(
                           embedding = "data.frame",
                           key_name = "character",
                           method = "character"
                         )
                         )

#' @title The \code{DimRedPCA}
#'
#' @description S4-class that abstracts principal component analysis.
#' @param n_dims Numeric value. The number of principal component vectors
#' returned.
#'
#' @seealso \code{DimRedMethod}
#'

DimRedPCA <- setClass(Class = "DimRedPCA",
                      slots = list(
                        n_dims = "numeric",
                        results = "list"
                      ),
                      contains = "DimRedMethod"
)

#' @title The \code{DimRedTSNE}
#'
#' @description S4-class that abstracts t stoachastic neighbour embedding.
#'
#' @seealso \code{DimRedMethod}
#'

DimRedTSNE <- setClass(Class = "DimRedTSNE",
                       slots = list(
                         n_dims = "numeric",
                         results = "list"
                       ),
                       contains = "DimRedMethod"
)

#' @title The \code{DimRedUMAP}
#'
#' @description S4-class that abstracts manifold approximation and projection.
#' @param n_dims Numeric value. The number of principal component vectors
#' returned.
#'
#' @seealso \code{DimRedMethod}
#'

DimRedUMAP <- setClass(Class = "DimRedUMAP",
                       slots = list(
                         n_dims = "numeric",
                         results = "list"
                       ),
                       contains = "DimRedMethod"
)

# -----


# r-objects ---------------------------------------------------------------


dimred_short <- c("pca" = "pc", "tsne" = "tsne", "umap" = "umap")

valid_methods_dimred <- c("pca", "tsne", "umap")



# -----



# functions ---------------------------------------------------------------

#' @rdname initiateAnalysisAspect
#' @export
initiateDimRed <- function(data,
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
      analysis_aspect = "DimRed"
    )

  object <- scaleData(object, na_rm = TRUE)

  return(object)

}


#' @rdname validInput
#' @export
validMethodsDimRed <- function(){

  return(valid_methods_dimred)

}

# -----



# methods for external generics -------------------------------------------

#' @rdname computePCA
#' @export
setMethod(
  f = "computePCA",
  signature = "DimRed",
  definition = function(object, n_dims = 2, verbose = TRUE, ...){

    mtr <- getScaledMtr(object)

    dimred_obj <- object@methods[["pca"]]

    if(base::is.null(dimred_obj)){

      give_feedback(msg = "Creating new object of class 'DimRedPCA'", verbose = verbose)

      dimred_obj <- DimRedPCA(n_dims = n_dims, key_name = object@key_name, method = "pca")

    }

    give_feedback(msg = glue::glue("Computing PCA ({n_dims} components)."), verbose = verbose)

    out <- irlba::prcomp_irlba(x = mtr, n = n_dims, ...)

    key <- object@key_name

    dimred_obj@embedding <-
      base::as.data.frame(out$x) %>%
      magrittr::set_colnames(
        x = .,
        value = stringr::str_c("pc", 1:base::ncol(.), sep = "_")
      ) %>%
      dplyr::mutate(
        {{key}} := base::rownames(mtr)
      ) %>%
      dplyr::select(
        !!rlang::sym(key), dplyr::everything()
      ) %>%
      tibble::as_tibble()

    dimred_obj@results <-
      purrr::map(.x = base::names(out), .f = ~ out[[.x]]) %>%
      purrr::set_names(nm = base::names(out))

    object@methods[["pca"]] <- dimred_obj

    give_feedback(msg = "Done.", verbose = verbose)

    return(object)

  }
)

#' @rdname computeUMAP
#' @export
setMethod(f = "computeUMAP", signature = "DimRed", definition = function(object, ...){

  mtr <- getScaledMtr(object)

  dimred_obj <- object@methods[["umap"]]

  if(base::is.null(dimred_obj)){

    give_feedback(msg = "Creating new object of class 'DimRedUMAP'", verbose = verbose)

    dimred_obj <- DimRedUMAP(key_name = object@key_name, method = "umap")

  }

  give_feedback(msg = glue::glue("Computing UMAP."), verbose = verbose)

  out <- umap::umap(d = mtr)

  out$data <- NULL

  key <- object@key_name

  dimred_obj@embedding <-
    base::as.data.frame(out$layout) %>%
    magrittr::set_colnames(
      x = .,
      value = stringr::str_c("umap", 1:base::ncol(.), sep = "_")
    ) %>%
    dplyr::mutate(
      {{key}} := base::rownames(mtr)
    ) %>%
    dplyr::select(
      !!rlang::sym(key), dplyr::everything()
    ) %>%
    tibble::as_tibble()

  dimred_obj@results <-
    purrr::map(.x = base::names(out), .f = ~ out[[.x]]) %>%
    purrr::set_names(nm = base::names(out))

  object@methods[["umap"]] <- dimred_obj

  give_feedback(msg = "Done.", verbose = verbose)

  return(object)

})

#' @rdname computeTSNE
#' @export
setMethod(f = "computeTSNE", signature = "DimRed", definition = function(object, n_dims = 2, ...){

  mtr <- getScaledMtr(object)

  dimred_obj <- object@methods[["tsne"]]

  if(base::is.null(dimred_obj)){

    give_feedback(msg = "Creating new object of class 'DimRedTSNE'", verbose = verbose)

    dimred_obj <- DimRedTSNE(n_dims = n_dims, key_name = object@key_name, method = "tsne")

  }

  give_feedback(msg = glue::glue("Computing TSNE."), verbose = verbose)

  out <- tsne::tsne(X = mtr, k = n_dims)

  key <- object@key_name

  dimred_obj@embedding <-
    base::as.data.frame(out) %>%
    magrittr::set_colnames(
      x = .,
      value = stringr::str_c("tsne", 1:base::ncol(.), sep = "_")
    ) %>%
    dplyr::mutate(
      {{key}} := base::rownames(mtr)
    ) %>%
    dplyr::select(
      !!rlang::sym(key), dplyr::everything()
    ) %>%
    tibble::as_tibble()

  object@methods[["tsne"]] <- dimred_obj

  give_feedback(msg = "Done.", verbose = verbose)

  return(object)

})


#' @rdname getEmbeddingDf
#' @export
setMethod(
  f = "getEmbeddingDf",
  signature = "DimRed",
  definition = function(object,
                        method_dimred = "pca",
                        numeric = FALSE,
                        numeric_scaled = FALSE,
                        grouping = FALSE,
                        logical = FALSE,
                        complete = FALSE,
                        shift = FALSE){

    dimred_obj <- getResults(object, method = method_dimred)

    key <- object@key_name

    if(base::isTRUE(numeric_scaled)){

      df <- getScaledDf(object, numeric = TRUE, grouping = grouping, logical = logical, complete = complete)

    } else {

      df <- getDf(object, numeric = numeric, grouping = grouping, logical = logical, complete = complete)

    }


    if(base::isTRUE(shift)){

      n_dims <- dimred_obj@n_dims

      if(n_dims %% 2 != 0){

        n_dims <- n_dims-1

      }

      all_dims <- 1:n_dims

      uneven_dims <- all_dims[all_dims %% 2 != 0]

      edf <-
        purrr::map_df(.x = uneven_dims, .f = function(dim){

          prefix <- dimred_short[method_dimred] %>% base::unname()

          dims <- stringr::str_c(prefix, c(dim, dim + 1), sep = "_")

          dim1 <- dims[1]
          dim2 <- dims[2]

          dplyr::select(.data = dimred_obj@embedding, {{key}}, dplyr::all_of(dims)) %>%
            dplyr::mutate(
              nth_dims = {{dim}},
              dims = base::toupper(dims) %>% stringr::str_c(collapse = " & ") %>% stringr::str_remove_all(pattern = "_")
            ) %>%
            dplyr::rename(x = !!rlang::sym(dim1), y = !!rlang::sym(dim2))

        }) %>%
        dplyr::mutate(dims = base::as.factor(dims))

    } else {

      edf <- dimred_obj@embedding

    }

    out <-  dplyr::left_join(x = edf, y = df, by = object@key_name)

    return(out)

  }
)


#' @rdname plotPCA
#' @export
setMethod(
  f = "plotPCA",
  signature = "DimRed",
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

    base::stopifnot(n_dims >= 2)
    base::stopifnot(n_dims %% 2 == 0)

    if(n_dims == 2){

      edf <- getEmbeddingDf(object, method_dimred = "pca", complete = TRUE)

      x <- "pc_1"
      y <- "pc_2"
      across <- NULL

      labs_add_on <- ggplot2::labs(x = "PC 1", y = "PC 2")

    } else {

      edf <-
        getEmbeddingDf(object, method_dimred = "pca", complete = TRUE, shift = TRUE) %>%
        dplyr::filter(nth_dims <= {{n_dims}})

      x <- "x"
      y <- "y"
      across <- "dims"

      labs_add_on <- ggplot2::labs(x = NULL, y = NULL)

    }

    plot_scatterplot(
      df = edf,
      x = x,
      y = y,
      across = across,
      alpha.by = alpha_by,
      color.by = color_by,
      shape.by = shape_by,
      size.by = size_by,
      pt.alpha = pt_alpha,
      pt.color = pt_color,
      pt.fill = pt_fill,
      pt.shape = pt_shape,
      pt.size = pt_size,
      color.aes = color_aes,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      clrsp = clrsp,
      ...
    ) +
      labs_add_on

  }
)

#' @rdname plotTSNE
#' @export
setMethod(
  f = "plotTSNE",
  signature = "DimRed",
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

    base::stopifnot(n_dims >= 2)
    base::stopifnot(n_dims %% 2 == 0)

    if(n_dims == 2){

      edf <- getEmbeddingDf(object, method_dimred = "tsne", complete = TRUE)

      x <- "tsne_1"
      y <- "tsne_2"
      across <- NULL

      labs_add_on <- ggplot2::labs(x = "TSNE 1", y = "TSNE 2")

    } else {

      edf <-
        getEmbeddingDf(object, method_dimred = "tsne", complete = TRUE, shift = TRUE) %>%
        dplyr::filter(nth_dims <= {{n_dims}})

      x <- "x"
      y <- "y"
      across <- "dims"

      labs_add_on <- ggplot2::labs(x = NULL, y = NULL)

    }

    plot_scatterplot(
      df = edf,
      x = x,
      y = y,
      across = across,
      alpha.by = alpha_by,
      color.by = color_by,
      shape.by = shape_by,
      size.by = size_by,
      pt.alpha = pt_alpha,
      pt.color = pt_color,
      pt.fill = pt_fill,
      pt.shape = pt_shape,
      pt.size = pt_size,
      color.aes = color_aes,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      clrsp = clrsp,
      ...
    ) +
      labs_add_on

  }
)

#' @rdname plotUMAP
#' @export
setMethod(
  f = "plotUMAP",
  signature = "DimRed",
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

    edf <- getEmbeddingDf(object, method_dimred = "umap", complete = TRUE)

    plot_scatterplot(
      df = edf,
      x = "umap_1",
      y = "umap_2",
      alpha.by = alpha_by,
      color.by = color_by,
      shape.by = shape_by,
      size.by = size_by,
      pt.alpha = pt_alpha,
      pt.color = pt_color,
      pt.fill = pt_fill,
      pt.shape = pt_shape,
      pt.size = pt_size,
      color.aes = color_aes,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      clrsp = clrsp,
      ...
    ) +
      ggplot2::labs(x = "UMAP 1", y = "UMAP 2")


  }
)


