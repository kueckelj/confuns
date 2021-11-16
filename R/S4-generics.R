



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
#' @description Computes cluster with method \emph{kmeans}.
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
#' @description Computes cluster with method \emph{pam}.
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
#' @description Computes distance matrices of the input data.
#'
#' @inherit argument_dummy params
#'
#' @return The input object.
#' @export
#'
setGeneric(name = "computeDistanceMatrices", def = function(object, ...){

  standardGeneric(f = "computeDistanceMatrices")

})


#' @title Compute correlation
#'
#' @description Computes correlation matrix as well as corresponding
#' p-values.
#'
#' @inherit argument_dummy params
#' @param ... Additional arguments given to \code{Hmisc::rcorr()}.
#'
#' @details Computation is conducted with \code{Hmisc::rcorr()}.
#'
#' @return The input object.
#'
#' @export
#'
setGeneric(name = "computeCorrelation", def = function(object, ...){

  standardGeneric(f = "computeCorrelation")

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

#' @title Obtain correlation data.frame
#'
#' @description Extracts correlation results in form of a data.frame
#' that has been constructed by \emph{melting} a correlation matrix
#' and its corresponding p-values to a data.frame via \code{reshape2::melt()}.
#'
#' @param distinct Logical. If TRUE, redundant correlation observations are dropped.
#' Additionally, correlation observations of the same variables are dropped.
#'
#' (Effectively, sets \code{type} to \emph{'lower'} and \code{diagonal} to FALSE.)
#'
#' @param digits Numeric. Given to \code{base::round()} and indicates the number
#' of digits to which the correlation value is rounded. Defaults to 2.
#' @param sep Character value. Denots the string with which the variable pairs
#' are combined in variable \emph{var_pair} of the output data.frame.
#' @inherit argument_dummy params
#'
#' @return A data.frame of class \code{corr_df} with the following columns:
#'
#' \itemize{
#'  \item{\emph{across}:}{ Factor or NULL. If factor, the group names of the grouping variable.},
#'  \item{\emph{var1}:}{ Factor. First one of the variable pair.}
#'  \item{\emph{var2}:}{ Factor. Second one of the correlated variable pair.},
#'  \item{\emph{var_pair}:}{Factor. Combination of \emph{var1} and \emph{var2}},
#'  \item{\emph{corr}:}{ Numeric. The correlation vaule.},
#'  \item{\emph{pval}:}{ Numeric. The corresponding p-value.},
#'  \item{emph{pval_threshold}:}{ Numeric. The denoted p-value threshold.},
#'  \item{\emph{signif}:}{ Logical. Indicates if the p-value of the correlated pair is below the
#'  denoted threshold.}
#'  \item{\emph{method_corr}:}{ Character. The correlation method.}
#'  }
#'
#' If \code{across} has been specified the variable paris of \emph{var1} and \emph{var2}
#' are not unique but appear multiple times - one time for each group.
#'
#' @export

setGeneric(name = "getCorrDf", def = function(object, ...){

  standardGeneric(f = "getCorrDf")

})

#' @title Obtain correlation matrix
#'
#' @description Extracts correlation results in form of correlation
#' matrices.
#'
#' @inherit argument_dummy params
#'
#' @details If \code{across} is a character a list of correlation matrices
#' is returned. Slots are named according to the groups of the denoted
#' grouping variable. If \code{across_subset} is NULL all groups are considered.
#' If \code{across_subset} is of length one and \code{flatten} is TRUE the
#' output list is flattened and a single correlation matrix is returned.
#'
#' @export
#'
setGeneric(name = "getCorrMtr", def = function(object, ...){

  standardGeneric(f = "getCorrMtr")

})



#' @title Obtain object of class \code{dendro}
#'
#' @description Extracts the data from the computed \code{hclust} object
#' with which a dendrogram can be plotted.
#'
#' @inherit argument_dummy params
#'
#' @return An object of class \code{dendro}. Contains data for argument
#' \code{data} of function \code{{ggdendro::ggdendrogram()}}.
#'
#' @export
#'

setGeneric(name = "getDendro", def = function(object, ...){

  standardGeneric(f = "getDendro")

})

#' @title Obtain dendrogram segments
#'
#' @description Extracts a data.frame that contains data to plot
#' a dendrogram with \code{ggplot2::geom_segment()}.
#'
#' @inherit argument_dummy params
#'
#' @return Data.frame.
#' @export
#'
setGeneric(name = "getDendroSegmentDf", def = function(object, ...){

  standardGeneric(f = "getDendroSegmentDf")

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


#' @title Obtain data matrix
#'
#' @description Extracts the numeric data of the object in form
#' of a matrix. The key variable is used as rownames.
#'
#' @inherit argument_dummy params
#'
#' @export
#'
setGeneric(name = "getMtr", def = function(object, ...){

  standardGeneric(f = "getMtr")

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

#' @title Obtain object of class \code{rcorr}
#'
#' @description Extracts correlation results in form of class \code{rcorr}.
#'
#' @param as_list Logical value. If TRUE, \code{rcorr} objects are returned
#' as simple lists.
#' @inherit argument_dummy params
#' @inherit Hmisc::rcorr return
#'
#' @details If \code{across} is NULL one object of class \code{rcorr} is
#' returned - the one for the complete data set. If \code{across} is a character the list of \code{rcorr} is
#' extracted and subsetted by the groups of interest denoted in \code{across_subset}.
#' If \code{across_subset} is NULL the complete list is returned. If \code{across_subset}
#' is of length one and \code{flatten} is TRUE the output list is flattened and
#' a single \code{rcorr} object is returned.
#'
#' @export
#'

setGeneric(name = "getRcorr", def = function(object, ...){

  standardGeneric(f = "getRcorr")

})


#' @title Obtain analysis results
#'
#' @description Generic extractor for results mainly used for
#' programming purpose as it provides informative error
#' messages if the requested content is missing.
#'
#' @inherit argument_dummy params
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

#' @title Plot avg sil-width data
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

#' @title Plot correlation plot
#'
#' @inherit argument_dummy params
#'
#' @return A ggplot.
#' @export
#'

setGeneric(name = "plotCorrplot", def = function(object, ...){

  standardGeneric(f = "plotCorrplot")

})

#' @title Plot dendrograms
#'
#' @description Plots a dendrogram with either ggplot or base plot.
#'
#' @inherit argument_dummy params
#'
#' @return A ggplot or a base plot.
#' @export
#'

setGeneric(name = "plotDendrogram", def = function(object, ...){

  standardGeneric(f = "plotDendrogram")

})

#' @title Plot a screeplot
#'
#' @description Plots quality of clustering results
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

#' @title Plot sil-width data
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
