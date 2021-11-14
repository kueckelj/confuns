



# a -----------------------------------------------------------------------

#' @title Agglomerate hierarchical trees
#'
#' @description Agglomerates hierarchical trees from a list
#' of distance matrices.
#'
#' @inherit argument_dummy params
#' @param ... Additional arguments given to \code{stats::hclust()}.
#'
#' @return The input object.
#' @export
#'

setGeneric(name = "agglomerateHierarchicalTrees", def = function(object, ...){

  standardGeneric(f = "agglomerateHierarchicalTrees")

})


# c -----------------------------------------------------------------------

#' @title Compute cluster with kmeans
#'
#' @description Compute cluster with method \emph{kmeans}.
#'
#' @inherit argument_dummy params
#'
#' @return The input object.
#' @export
#'
setGeneric(name = "computeClusteringKmeans", def = function(object, ...){

  standardGeneric(f = "computeClusteringKmeans")

})


#' @title Compute cluster with pam
#'
#' @description Compute cluster with method \emph{pam}.
#'
#' @inherit argument_dummy params
#'
#' @return The input object.
#' @export
#'
setGeneric(name = "computeClusteringPam", def = function(object, ...){

  standardGeneric(f = "computeClusteringPam")

})


#' @title Compute distance matrices
#'
#' @description Compute distance matrices of the input data.
#'
#' @inherit argument_dummy params
#'
#' @return The input object.
#' @export
#'
setGeneric(name = "computeDistanceMatrices", def = function(object, ...){

  standardGeneric(f = "computeDistanceMatrices")

})



# g -----------------------------------------------------------------------


#' @title Obtain average sil-width data
#'
#' @description Extracts a data.frame that contains information
#' about the average silhouette widths of different clustering
#' results with mehod \emph{pam}.
#'
#' @inherit argument_dummy params
#'
#' @return Data.frame.
#' @export
#'

setGeneric(name = "getAvgSilWidthsDf", def = function(object, ...){

  standardGeneric(f = "getAvgSilWidthsDf")

})


#' @title Obtain object data
#'
#' @description Extracts the objects data as a data.frame.
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame with subclass \code{tibble}.
#' @export
#'

setGeneric(name = "getDf", def = function(object, ...){

  standardGeneric(f = "getDf")

})


#' @title Obtain object of class \code{ClusteringKmeans}
#'
#' @description Extracts an object of class \code{ClusteringKmeans}.
#'
#' @inherit argument_dummy params
#'
#' @seealso getKmeans()
#'
#' @return An object of class \code{ClusteringKmeans}.
#' @export

setGeneric(name = "getClusteringKmeans", def = function(object, ...){

  standardGeneric(f = "getClusteringKmeans")

})

#' @title Obtain object of class \code{ClusteringPam}
#'
#' @description Extracts an object of class \code{ClusteringPam}.
#'
#' @inherit argument_dummy params
#'
#' @seealso getPam()
#'
#' @return An object of class \code{ClusteringPam}.
#' @export

setGeneric(name = "getClusteringPam", def = function(object, ...){

  standardGeneric(f = "getClusteringPam")

})


#' @title Obtain object of class \code{hclust}
#'
#' @description Extracts object of class \code{hclust}
#' as described in \code{stats::hclust()}.
#'
#' @inherit argument_dummy params
#'
#' @return An object of class \code{hclust.}
#' @export
#'

setGeneric(name = "getHclust", def = function(object, ...){

  standardGeneric(f = "getHclust")

})

#' @title Obtain object of class \code{kmeans}
#'
#' @description Extracts an object of class \code{kmeans}
#' as described in \code{?stats::kmeans}.
#'
#' @inherit argument_dummy params
#'
#' @seealso getClusteringKmeans()
#'
#' @return An object of class \code{kmeans}.
#' @export

setGeneric(name = "getKmeans", def = function(object, ...){

  standardGeneric(f = "getKmeans")

})


#' @title Obtain object of class \code{pam}
#'
#' @description Extracts an object of class \code{pam}
#' as described in \code{?cluster::pam}.
#'
#' @inherit argument_dummy params
#'
#' @seealso getClusteringPam()
#'
#' @return An object of class \code{pam}.
#' @export

setGeneric(name = "getPam", def = function(object, ...){

  standardGeneric(f = "getPam")

})


#' @title Obtain analysis results
#'
#' @description Generic extractor for results mainly used for
#' programming purpose as it provides informative error
#' messages if the requested content is missing.
#'
#' @inherit argument_dummy params
#' @param ...
#'
#' @return Depends on the objects class.
#' @export
#'

setGeneric(name = "getResults", def = function(object, ...){

  standardGeneric(f = "getResults")

})


#' @title Obtain sil-width data
#'
#' @description Extracts a data.frame that contains information
#' about the silhouette width of every observation in different
#' clustering results with method \emph{pam}.
#'
#' @inherit argument_dummy params
#'
#' @return Data.frame.
#' @export
#'
setGeneric(name = "getSilWidthsDf", def = function(object, ...){

  standardGeneric(f = "getSilWidthsDf")

})



# p -----------------------------------------------------------------------

#' @title Visualize avg sil-width data
#'
#' @description Plots information about the average silhouette
#' widths of different clustering results with method \emph{pam}.
#'
#' @inherit argument_dummy params
#' @param ...
#'
#' @return A ggplot.
#' @export
#'

setGeneric(name = "plotAvgSilWidths", def = function(object, ...){

  standardGeneric(f = "plotAvgSilWidths")

})


#' @title Plot a screeplot
#'
#' @description Visualizes quality of clustering results
#' suggested by kmeans with a screeplot.
#'
#' @inherit argument_dummy params
#'
#' @details If \code{ks} and \code{methods_kmeans} are specified only
#' the resulting combinations are included. Else all found combinations
#' are included.
#'
#' @return A ggplot.
#' @export
#'
setGeneric(name = "plotScreeplot", def = function(object, ...){

  standardGeneric(f = "plotScreeplot")

})

#' @title Visualize sil-width data
#'
#' @description Plots information about the silhouette width of
#' every observation in different clustering results with method \emph{pam}.
#'
#' @inherit argument_dummy params
#'
#' @return A ggplot.
#' @export
#'

setGeneric(name = "plotSilWidths", def = function(object, ...){

  standardGeneric(f = "plotSilWidths")

})



# s -----------------------------------------------------------------------

#' @title Set data and key variables
#'
#' @description Sets the objects slot @@data and slot @@key_name and makes
#' sure that the key variable exists and is valid.
#'
#' @inherit argument_dummy params
#'
#' @details If \code{key_name} is NULL the key variable is constructed
#' as a combination of the input data.frames rownames and the string
#' provided with \code{key_prefix}. If the the datas rownames are NULL
#' or contain \emph{""} the rownumbers are used instead. The newly constructed
#' key variable is named \emph{data_ids}.
#'
#' @return A data.frame with subclass \code{tibble}.
#' @export
#'
setGeneric(name = "setData", def = function(object, ...){

  standardGeneric(f = "setData")

})



#' @title Set data helper
#'
#' @description Helper for generic function \code{setData()}.
#'
#' @inherit argument_dummy params
#'
#' @return The input object.
#' @export
#'
set_data_hlpr <- function(object,
                          data,
                          key.name,
                          key.prefix,
                          meta.names,
                          slot.data = "data",
                          slot.key.name = "key_name",
                          slot.meta = "meta",
                          verbose = TRUE){

  data <- base::as.data.frame(data)

  if(base::is.character(key.name)){

    check_data_frame(
      df = data,
      var.class = purrr::set_names(x = list(c("character", "factor")), nm = key.name)
    )

    is_key_variable(df = data, key.name = key.name, stop.if.false = TRUE)

    data[[key.name]] <- base::as.character(data[[key.name]])

    base::rownames(data) <- NULL

  } else if(base::is.null(key.name)){

    give_feedback(
      msg = "Constructing new key variable named 'data_ids'.",
      verbose = verbose
    )

    give_feedback(
      msg = "Trying rownames.",
      verbose = verbose
    )

    key_var <- base::rownames(data)

    if(base::is.null(key_var) | base::any(key_var == "")){

      give_feedback(
        msg = "Rownames are empty or incomplete. Using rownumbers and prefix '{key.prefix}' to construct key variable.",
        verbose = verbose
      )

      rnums <- 1:base::nrow(data)

      key_var <- stringr::str_c(key.prefix, rnums, sep = "")

    } else {

      give_feedback(
        msg = "Rownames valid. Using rownames as key variable.",
        verbose = verbose
      )

    }

    key.name <- "data_ids"

    give_feedback(
      msg = "Key name is 'data_ids'. Rename with 'renameKeyVariable()'."
    )

    data[[key.name]] <- key_var

  }

  meta <- dplyr::select(data, {{key.name}}, dplyr::all_of(meta.names))

  data <- dplyr::select(data, -dplyr::all_of(meta.names))

  methods::slot(object, name = slot.data) <- data

  methods::slot(object, name = slot.key.name) <- key.name

  methods::slot(object, name = slot.meta) <- meta

  return(object)

}
