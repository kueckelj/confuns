



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

#' @title Compute \code{hclust} objects
#'
#' @description Computes \code{hclust} objects for all combinations of distance
#' and agglomeration methods. Does not store distance matrices in the object!
#'
#' @note If you want to store distance matrices use \code{computeDistanceMatrices()}
#' and subsequently \code{agglomerateHierarchicalTrees()}. \code{computeClusteringHclust()}
#' is a wrapper around both that skips storing the distance matrices.
#'
#' @inherit argument_dummy params
#'
#' @return The input object.
#' @export
#'
setGeneric(name = "computeClusteringHclust", def = function(object, ...){

  standardGeneric(f = "computeClusteringHclust")

})

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
#' @details If \code{methods_dist} is of length 2 or more a distance matrix
#' for every distance method is computed and stored in the object. Note that
#' distance matrices can become quite big in size. If you are only interested
#' in the subsequent hierarchical clustering results and not in the distance
#' matrices use \code{computeClusteringHclust()}. It is a wrapper around
#' \code{computeDistanceMatrices()} and the subsequent \code{agglomerateHierarchicalTrees()}.
#' However, only the cluster results of \code{agglomerateHierarchicalTrees()} are
#' stored in form of \code{hclust} objects.
#'
#' @seealso \code{getDistMtr()}, \code{agglomerateHierarchicalTrees()}, \code{computeHierarchicalClustering()}
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
#' @inherit across_an2 params
#' @inherit argument_dummy params
#'
#' @details If argument \code{across} is NULL. All numeric variables are simply
#' correlated.
#'
#' If \code{across} is character: Prior to correlation, the data set is split into subsets for
#' each group a grouping variable contains. Then the variables are correlated separately
#' for each group. This is done for every grouping variable denoted in the input
#' vector of \code{across.}
#'
#' Results can be retrieved or visualized by specifying the grouping
#' variable of interest in the \code{across} argument and the group(s) of interest
#' with the \code{across_subset} argument of the respective function.
#'
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


#' @title Compute PCA
#'
#' @description Computes principal components and stores the embedding
#' in form of a data.frame.
#'
#' @param n_dims Numeric value. Integer that indicates the number of
#' principal components to compute. Must be lower than the number
#' of numeric variables based on which principal components are
#' computed.
#' @param ... Additional arguments given to \code{irlba::prcomp_irlba()}.
#' @inherit argument_dummy params

#' @export
setGeneric(name = "computePCA", def = function(object, ...){

  standardGeneric(f = "computePCA")

})

#' @title Compute TSNE
#'
#' @description Computes t stochastic neighbour embedding stores the embedding
#' in form of a data.frame.
#'
#' @param n_dims Numeric value. Integer that indicates the number of
#' dimensions of the resulting embedding. Must be lower than the number
#' of numeric variables based on which principal components are
#' computed. Given to argument \code{k} of function \code{tsne::tsne()}.
#' @param ... Additional arguments given to \code{tsne::tsne()}.
#' @inherit argument_dummy params

#' @export
setGeneric(name = "computeTSNE", def = function(object, ...){

  standardGeneric(f = "computeTSNE")

})

#' @title Compute UMAP
#'
#' @description Computes a manifold approximation and projection.
#'
#' @param ... Additional arguments given to \code{umap::umap()}.
#' @inherit argument_dummy params
#'
#' @return The input object.
#' @export
#'
setGeneric(name = "computeUMAP", def = function(object, ...){

  standardGeneric(f = "computeUMAP")

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

#' @title Obtain \code{Clustering} object
#'
#' @description Extracts an object of class \code{Clustering} containing
#' clustering results for all used methods so far.
#'
#' @inherit argument_dummy params
#'
#' @return S4-object of class \code{Clustering}.
#'
#' @export

setGeneric(name = "getClustering", def = function(object, ...){

  standardGeneric(f = "getClustering")

})

#' @title Obtain object of class \code{ClusteringHclust}
#'
#' @description Extracts an object of class \code{ClusteringHclust}.
#'
#' @inherit argument_dummy params
#'
#' @seealso getHclust()
#'
#' @return An object of class \code{ClusteringHclust}.
#' @export

setGeneric(name = "getClusteringHclust", def = function(object, ...){

  standardGeneric(f = "getClusteringHclust")

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


#' @title Obtain cluster variables (hclust)
#'
#' @description Extracts a data.frame that contains grouping variables
#' according to clustering results.
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame.
#' @export

setGeneric(name = "getClusterVarsHclust", def = function(object, ...){

  standardGeneric(f = "getClusterVarsHclust")

})

#' @title Obtain cluster variables (kmeans)
#'
#' @description Extracts a data.frame that contains grouping variables
#' according to clustering results.
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame.
#' @export

setGeneric(name = "getClusterVarsKmeans", def = function(object, ...){

  standardGeneric(f = "getClusterVarsKmeans")

})

#' @title Obtain cluster variables (PAM)
#'
#' @description Extracts a data.frame that contains grouping variables
#' according to clustering results.
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame.
#' @export
#'
setGeneric(name = "getClusterVarsPam", def = function(object, ...){

  standardGeneric(f = "getClusterVarsPam")

})


#' @title Obtain \code{Correlation} object
#'
#' @description Extracts \code{Correlation} object that contains all correlation
#' results computed so far.
#'
#' @inherit argument_dummy params
#'
#' @return S4-bject of class \code{Correlation}.
#'
#' @export
#'
setGeneric(name = "getCorrelation", def = function(object, ...){

  standardGeneric(f = "getCorrelation")

})

#' @title Obtain correlation data.frame
#'
#' @description Extracts correlation results in form of a data.frame
#' that has been constructed by \emph{melting} a correlation matrix
#' and its corresponding p-values to a data.frame via \code{reshape2::melt()}.
#'
#' @inherit corr_dummy params
#'
#' @param sep Character value. Denots the string with which the variable pairs
#' are combined in variable \emph{var_pair} of the output data.frame.
#' @inherit argument_dummy params
#'
#' @return A data.frame of class \code{corr_df} with the following columns:
#'
#' \itemize{
#'  \item{\emph{across}:}{ Factor or NULL. If factor, the group names of the grouping variable
#'  denoted in \code{across}.},
#'  \item{\emph{var1}:}{ Factor. First variable of the correlated variable pair.}
#'  \item{\emph{var2}:}{ Factor. Second variable  of the correlated variable pair.},
#'  \item{\emph{var_pair}:}{Factor. Combination of \emph{var1} and \emph{var2}},
#'  \item{\emph{corr}:}{ Numeric. The correlation vaule.},
#'  \item{\emph{pval}:}{ Numeric. The corresponding p-value.},
#'  \item{emph{pval_threshold}:}{ Numeric. The denoted p-value threshold.},
#'  \item{\emph{signif}:}{ Logical. Indicates if the p-value of the correlated pair is below the
#'  denoted threshold.}
#'  \item{\emph{method_corr}:}{ Character. The correlation method.}
#'  }
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


#' @title Obtain \code{DimRed} object
#'
#' @description Extracts an object of class \code{DimRed} that
#' contains dimensional reductions computed so far.
#'
#' @inherit argument_dummy params
#'
#' @return S4-bject of class \code{DimRed}.
#'
#' @export
setGeneric(name = "getDimRed", def = function(object, ...){

  standardGeneric(f = "getDimRed")

})

#' @title Obtain distance matrix
#'
#' @description Extracts distance matrix from S4 object.
#'
#' @inherit argument_dummy params
#'
#' @return A distance matrix or NULL (if none has been computed yet).
#'
#' @export

setGeneric(name = "getDistMtr", def = function(object, ...){

  standardGeneric(f = "getDistMtr")

})


#' @title Obtain \code{DimRed} embedding
#'
#' @description Extracts the dimensional reduction embedding in form
#' of a data.frame. Naming of the variables corresponds
#' to the chosen method.
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame.
#' @export
#'
setGeneric(name = "getEmbeddingDf", def = function(object, ...){

  standardGeneric(f = "getEmbeddingDf")

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


#' @title Obtain empty data.frame
#'
#' @description Extracts a data.frame that contains only the key variable.
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame.
#' @export
#'
setGeneric(name = "getKeyDf", def = function(object, ...){

  standardGeneric(f = "getKeyDf")

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


#' @title Obtain scaled data
#'
#' @description Extracts the scaled data in a data.frame. Grouping and
#' logical variables can be joined
#'
#' @inherit getDf params
#'
#' @return A data.frame.
#' @export
#'
setGeneric(name = "getScaledDf", def = function(object, ...){

  standardGeneric(f = "getScaledDf")

})

#' @title Obtain scaled matrix
#'
#' @description Extracts the scaled data as a matrix. Key variable is used
#' for the rownames.
#'
#' @inherit argument_dummy params
#'
#' @return A matrix.
#' @export
#'
setGeneric(name = "getScaledMtr", def = function(object, ...){

  standardGeneric(f = "getScaledMtr")

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
#'
#' @param color_low,color_high Character values. Specifies the colors that are
#' used to generate the color spectrum with which the gradient from negative
#' \code{color_low}) to positive correlation (\code{color_high}) is displayed.
#' @param size_by_corr Logical value. If TRUE, size is used in addition to coloring
#' to indicate the correlation. Ignored if \emph{shape} = \emph{'tile'}
#'
#' @param type Character value. Denotes how the underlying correlation matrix is
#' handled.
#' If \code{type} = \emph{'complete'}, the matrix is let as is.
#' If \code{type} = \emph{'lower'}, the part below the diagonal is used. The upper part is set to NA.
#' If \code{type} = \emph{'upper}, the part above the diagonal is used. the lower part is set to NA.
#'
#'
#' @param variables_subset Character vector or NULL. If character, specifies
#' the subset of variable names that is included in the plot. To exclude single
#' variables the variable name can be provided prefixed with a \emph{'-'}.
#' @inherit corr_dummy params
#' @inherit across_vis1 params
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
#' @inherit hclust_dummy params
#' @inherit argument_dummy params
#'
#' @return A ggplot or a base plot.
#' @export
#'

setGeneric(name = "plotDendrogram", def = function(object, ...){

  standardGeneric(f = "plotDendrogram")

})



#' @title Plot PCA
#'
#' @description Plots PCA results in a scatterplot.
#'
#' @param n_dims Numeric value. Integer that indicates the number of principal
#' component vectors that are plotted.
#' @inherit argument_dummy params
#'
#' @return A ggplot.
#' @export
#'
setGeneric(name = "plotPCA", def = function(object, ...){

  standardGeneric(f = "plotPCA")

})


#' @title Plot a scatterplot
#'
#' @description Plots two numeric variables on the x- and y-axis.
#'
#' @param x,y Character values. The variables to plotted on the x- and y-axis.
#' @param display_smooth Logical value. Indicates if a smoothed line is displayed.
#' @param display_corr Logical value. Indicates if correlation values of the x-
#' and y-variable are computed and displayed as text in the plot.
#' @param corr_method Character value. Specifies the correlation method.
#' @param corr_p_min Numeric value. Specifies the
#' @param corr_pos_x,corr_pos_y Numeric value or NULL. If numeric, specifies the exact
#' position of the text on the x- or y-axis.
#' @param corr_text_sep Character value. The string with which p-value and correlation
#' value are separated.
#' @param corr_text_size,pt_size,smooth_size Numeric value. The size with which
#' results are displayed.
#' @inherit across_vis2 params
#' @inherit argument_dummy params
#'
#' @return A ggplot.
#'
#' @export
setGeneric(name = "plotScatterplot", def = function(object, ...){

  standardGeneric(f = "plotScatterplot")

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

#' @title Plot TSNE
#'
#' @description Plots TSNE results in a scatterplot.
#'
#' @param n_dims Numeric value. Integer that indicates the number of dimensions that are plotted.
#' @inherit argument_dummy params
#'
#' @return A ggplot.
#' @export
#'
setGeneric(name = "plotTSNE", def = function(object, ...){

  standardGeneric(f = "plotTSNE")

})

#' @title Plot UMAP
#'
#' @description Plots UMAP results in a scatterplot.
#'
#' @inherit argument_dummy params
#'
#' @return A ggplot.
#' @export
#'
setGeneric(name = "plotUMAP", def = function(object, ...){

  standardGeneric(f = "plotUMAP")

})

# s -----------------------------------------------------------------------

#' @title Scale data
#'
#' @description Scales all numeric variables via zscore.
#'
#' @param na_rm Logical. If TRUE, NAs are ignored.
#' @inherit argument_dummy params
#'
#' @return The input object.
#' @export
#'
setGeneric(name = "scaleData", def = function(object, ...){

  standardGeneric(f = "scaleData")

})

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
#' @return The input object.
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

      if(base::is.character(key.prefix)){

        key_var <- stringr::str_c(key.prefix, key_var, sep = "")

      }

    }

    key.name <- "data_ids"

    give_feedback(
      msg = "Key name is 'data_ids'. Rename with 'renameKeyVariable()'."
    )

    data[[key.name]] <- key_var

    base::rownames(data) <- NULL

  }

  meta <- dplyr::select(data, {{key.name}}, dplyr::all_of(meta.names))

  data <- dplyr::select(data, -dplyr::all_of(meta.names))

  methods::slot(object, name = slot.data) <- data

  methods::slot(object, name = slot.key.name) <- key.name

  methods::slot(object, name = slot.meta) <- meta

  return(object)

}
