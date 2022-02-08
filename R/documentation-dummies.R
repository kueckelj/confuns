#' @title normal df_dummy
#'
#' @param df A data.frame.
#'
df <- function(df){

}


#' @title ggplot2_dummy
#'
#' @return Returns a ggplot-object that can be additionally customized according
#' to the rules of the ggplot2-framework.

ggplot2_dummy <- function(){

}


#' hclust_dummy
#'
#' @param type Character value. Indicates the shape of the dendrogram.
#' Input \emph{'rectangle'} will draw rectangular lines, while \emph{'triangle'} will draw triangular lines.
#'
hclust_dummy <- function(type){}

#' @title normalize_dummy
#' @param normalize Logical. If set to TRUE numeric values will be scaled to
#' values between one and zero.

normalize_dummy <- function(normalize){

}


#' @title corr_dummy
#'
#' @param diagonal Logical value.  Indicates if the diagonal values of the
#' matrices are kept ( = TRUE) or set to NA (= FALSE). Ignored if \code{type}
#' = \emph{'complete'}.
#' @param digits Numeric. Given to \code{base::round()} and indicates the number
#' of digits to which the correlation value is rounded. Defaults to 2.
#' @param distinct Logical value. If TRUE only one observation per variable pair remains
#' in the output data.frame. Ignored if \code{type} = \emph{'complete'}.
#' @param shape Character value. Specifies the shape of the geometric objects with
#' which the variable pairs are displayed. Either \emph{'circle', 'rect'} or \emph{'tile'}.
#' @param type Character value. Denotes how the underlying correlation matrix is
#' handled. Three options:
#'
#' \itemize{
#'  \item{\emph{'complete'}:}{ The matrix stays as is.},
#'  \item{\emph{'lower'}:}{ The part below the diagonal is used. The upper part is set to NA.},
#'  \item{\emph{'upper'}:}{The part above the diagonal is used. the lower part is set to NA.}
#'  }
#'
corr_dummy <- function(diagonal, digits, distinct, type){}


#' @title verbose
#' @param verbose Logical. If set to TRUE informative messages regarding
#' the computational progress will be printed.
#'
#' (Warning messages will always be printed.)

verbose <- function(verbose){

}




#' @title across an1 dummy
#'
#' @param across Character value or NULL. If character, the name of the grouping variable
#' that contains the group assignment across which analysis should
#' be conducted. If NULL, no grouping is done.
#' @param across.subset,across_subset Character vector or NULL. If NULL, all groups
#' are considered. If character, specifies the groups of the
#' grouping variable that are of interest. Prefixing group names with
#' \emph{'-'} excludes them.
#'
across_an1 <- function(across, across_subset){}

#' @title across an2 dummy
#'
#' @param across Character vector or NULL. If character, the name of the grouping variables
#' that contain the group assignment across which analysis should
#' be conducted. If NULL, no grouping is done.
#' @param across.subset,across_subset Character vector or NULL. If NULL, all groups
#' are considered. If character, specifies the groups of the
#' grouping variable that are of interest. Prefixing group names with
#' \emph{'-'} excludes them.
#'
across_an2 <- function(across, across_subset){}

#' @title across vis1 dummy
#'
#' @param across Character value or NULL. If character, specifies the name of the grouping variable
#' that contains the group assignment across which results are displayed. If NULL,
#' no grouping is done.
#' @param acrosss.subset,across_subset Character vector or NULL. If character, specifies groups of the
#' grouping variable that are of interest. Prefixing group names with \emph{'-'} excludes them.
#' If NULL, all groups are considered.
#'
across_vis1 <- function(across, across_subset){}


#' @title across vis2 dummy
#'
#' @param across Character vector or NULL. If character, the names of the grouping variables
#' that contain the group assignments across which results are displayed. If NULL, no grouping
#' is done.
#'
#' @param across_subset List of character vectors. Must be named according
#' to the input of argument \code{across}. Named slots of the list should be character
#' vectors containing the groups of interest. Prefixing group names with
#' \emph{'-'} excludes them.
#'
across_vis2 <- function(across, across_subset){}


#' @title Argument dummy
#'
#' @param across Character value. Specifies the discrete variable in the data.frame
#' across which the variables of interest are to be analyzed or displayed.
#' @param across.subset Character vector. The groups of interest that the grouping variable
#' denoted in \code{across} contains.
#'
#' If there are more groups you are interested in than they are groups you are not interested
#' in specify those that you are not interested in prefixed with an \emph{'-'}.
#' Variables prefixed that way are discarded and the remaining are kept.
#'
#' @param alpha.by,alpha_by,shape.by,shape_by,size.by,size_by Character
#' value or NULL. If character, specifies the variable that is mapped to the respective
#' aesthetic of the plot.
#' @param alpha.trans,alpha_trans,color.trans,color_trans,shape.trans,shape_trans,size.trans,size_trans
#' Character value. Name of the transformation method applied to the continuous scale. Use
#' \code{validScaleTransformations()} to obtain all valid input options. Use \emph{'identity'}
#' for no transformation.
#'
#' @param color.by,color_by Character value or NULL. If character, specifies the
#' variable that is displayed by color.
#'
#' @param clrp Character value. Specifies the color palette to be used to represent
#' groups of discrete variables. Run \code{validColorPalettes()} to obtain valid
#' input options.
#'
#' @param clrsp,pt.clrsp,pt_clrsp Character value. Specifies the color spectrum to be used to represent
#' continuous values of numeric variables. Run \code{validColorSpectra()} to obtain
#' valid input options.
#'
#' @param clrp.adjust,clrp_adjust Named character vector. Can be used to adjust colors
#' with which groups are displayed. Names indicate groups, values
#' indicate the colors with which specific groups are supposed to be displayed.
#'
#' @param color.aes,color_aes Either \emph{'color'} or \emph{'fill'}. Specifies the
#' aesthetic that is used to visualize the variable that is specified in \code{color_by}.
#' Ignored if \code{color_by} is NULL.
#' @param display_labels Lgoical value. Indicates if labels are displayed.
#' @param display_legend,display_title Logical values. Indicate if legend or
#' title is displayed.
#' @param display.points Logical value. If set to TRUE points are used additionally
#' to display the results.
#' @param display.facets Logical value. If set to TRUE the plot is split via
#' \code{ggplot2::facet_wrap()} such that each variable gets it's own subplot.
#' @param display.grid,display_grid Logical value. If TRUE, a grid is displayed.
#' @param facet.with,facet_with Character value. Either \emph{'grid'} or \emph{'wrap'}.
#' Specifies the function with which the plot-facetting is created. If \code{across}
#' is of length 2 and \code{facet_with} = \emph{'wrap'} \code{ggplot2::facet_wrap()}
#' is used. Else \code{ggplot2::facet_grid()} is used.
#' @param force Logical value. Must be set to TRUE to allow overwriting.
#' @param grid.alpha,grid_alpha,grid.size,grid_size Numeric values. Specify transparency
#' and thickness of the lines of the grid.
#' @param grid.color,grid_color Character value. Specifies the color of the grid lines.
#'
#' @param h Numeric value. Denotes the height at which the dendrogram is cut.
#' @param hs Numeric vector. Denotes the heights at which the dendrogram is cut.
#'
#' @param key.name,key_name Character value or NULL. Denotes the variable that is used
#' to identify each observation uniquely.
#' @param key.prefix,key_prefix Character value. The string with which
#' the IDs in the newly constructed key variable are prefixed if \code{key_name} is
#' NULL.
#'
#' @param k Numeric value. Denotes the number of clusters. Must be bigger than 1. Must
#' not be bigger than the number of observations of the data set.
#' @param ks Numeric vector. Denotes all options for k-clusters. Values <1 are discarded.
#' Is converted into an integer vector.
#' @param labels.angle,labels_angle Numeric value. The angle with which labels are displayed.
#' @param labels.size,labels_size Numeric value. The size with which labels are displayed.
#' @param labels.hjust,labels.vjust,labels_hjust,labels_vjust Numeric values. Adjust labels positioning.
#'
#' @param line.alpha,line_alpha,line.size,line_size Numeric value. Specifies the transperancy
#' and the thickness of lines in the plot.
#' @param line.color,line_color Character value. Specifies the color of lines in the plot.
#' @param line.type,line_type Character value. Specifies the type of the lines. Use
#' \code{validLineTypes()} to obtain all valid input options.
#'
#' @param method.aggl,method_aggl Character value. Specifies the agglomerative method
#' of interest. Use \code{validMethodsAggl()} to obtain all valid input options.
#'
#' @param methods.aggl,methods_aggl Character vector. Specifies the agglomerative
#' methods of interest. Use \code{validMethodsAggl()} to obtain all valid input options.
#'
#' @param method.corr,method_corr Character value. Specifies the correlation
#' method of interest. Use \code{validMethodsCorrelation()} to obtain all valid
#' input options.
#' @param methods.corr,methods_corr Character vector. Specifies the correlation
#' methods of interest. Use \code{validMethodsCorrelation()} to obtain all valid
#' input options.
#'
#' @param method.dist,method_dist Character value. Specifies the distance method
#' of interest. Use \code{validMethodsDist()} to obtain all valid input options.
#' @param methods.dist,methods_dist Character vector. Specifies the distance methods
#' of interest. Use \code{validMethodsDist()} to obtain all valid input options.
#'
#' @param method.kmeans,method_kmeans Character value. Specifies the kmeans method
#' of interest. Use \code{validMethodsKmeans()} to obtain all valid input options.
#' @param methods.kmeans,methods_kmeans Character vector. Specifies the kmeans methods
#' of interest. Use \code{validMethodsKmeans()} to obtain all valid input options.
#'
#' @param method.pam,method_pam Character value. Specifies the pam-method of interest.
#' Use \code{validMethodsPam()} to obtain all valid input options.
#' @param methods.pam,methods_pam Character value. Specifies the pam-method of interest.
#' Use \code{validMethodsPam()} to obtain all valid input options.
#'
#' @param object Any object for whose class a method has been defined.
#'
#' @param order.by,order_by Character value or NULL. If character, specifies the
#' numeric variable by which the points are arranged before plotting.
#'
#' @param order.desc Logical value. If set to TRUE the way the points are ordered
#' is reversed.
#'
#' @param pt.alpha,pt_alpha Numeric value. Specifies the transparency of points.
#' @param pt.color,pt_color Character value. Specifies the color with which all points are displayed .
#' @param pt.num,pt_num Numeric value. Species the number of points (sample size)
#' to prevent overplotting.
#' @param pt.shape,pt_shape Numeric or character value. If numeric, the respective
#' shape is taken for all points. If character, the respective variable is
#' mapped to the shape-aesthetic.
#' @param pt.size,pt_shape Numeric values. Specifies size of points.
#'
#' @param pval_threshold Numeric value or NULL. If numeric, denotes the maximum p-value to be considered as
#' significant. Must no be higher than 0.05.
#'
#' @param ref.group Character value. Specifies the reference group for the pairwise statistical test. Must
#' be among the groups the variable specified in \code{across} contains. If set to NULL the
#' first group found is taken.
#'
#' @param relevel Logical value. If set to TRUE the input order of \code{across.subset} and
#' \code{variables} determines the order in which the groups of interest are displayed.
#'
#' @param scales,nrow,ncol Given to \code{ggplot2::facet_wrap()}. Affects the way the subplots
#' are displayed.
#' @param smooth.alpha,smooth_alpha Numeric value. The transparency of the smoothed line.
#' @param smooth.color,smooth_color Character value. The color of the smoothed line.
#' @param smooth.method,smooth_method Character value. The smoothing method. Given to argument
#' \code{method} of \code{ggplot2::geom_smooth()}.
#' @param smooth.se,smooth_se Logical value. Indicates if the confidence intervals are
#' displayed.
#' @param stop_if_null Logical value. If TRUE and the function does not find the object to
#' extract an informative error is raised. Else the empty value - usually NULL - is returned.
#' @param transform.with,transform_with List or NULL. If list, can be used to transform continuous variables before plotting.
#' Names of the slots of the provided list refer to the variables. The content of the slot refers to the transforming functions.
#' Slot content can either be a character vector of function names. Use \code{validVarTransformations()} to obtain all valid character value inputs.
#' Or it can be a list of functions (and function names).
#'
#' @param test.pairwise Character value or NULL. If character, one of \emph{'t.test', 'wilcox.test'}.
#' @param test.groupwise Character value or NULL. If character, one of \emph{'anova', 'kruskal.test'}.
#'
#' @param valid.classes Character vector to specify the classes the input
#' might have in order not to be discarded.
#'
#' @param values_alpha,values_size Numeric values. Denote transparency and size of
#' the displayed values.
#' @param values_color Character value. Denotes the color in which the values
#' are displayed.
#' @param values_digits Numeric value. Denotes the number of digits to which
#' the displayed values are rounded.
#' @param variables Character vector. Specifies the variables
#' of interest. If set to NULL all valid variables of the input data.frame are
#' considered (as long as the total number does not exceeds the limit).
#'
#' If there are more variables you are interested in than they are variables
#' you are not interested in specify those that you are not interested in prefixed
#' with an \emph{'-'}. Variables prefixed that way are discarded and the remaining are kept.
#' @param x,y Character value. Variable to be plotted on the respective axis.
#'
#' @inherit df params
#' @inherit check_across_subset params
#' @inherit ggplot2_dummy return
#' @inherit normalize params
#' @inherit verbose params
#'
#' @export

argument_dummy <- function(df, normalize, verbose){}


