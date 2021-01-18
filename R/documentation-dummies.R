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


#' @title normalize_dummy
#' @param normalize Logical. If set to TRUE numeric values will be scaled to
#' values between one and zero.

normalize_dummy <- function(normalize){

}


#' @title verbose
#' @param verbose Logical. If set to TRUE informative messages regarding
#' the computational progress will be printed.
#'
#' (Warning messages will always be printed.)

verbose <- function(verbose){

}




#' @title Argument dummy
#'
#' @param across Character value. Denotes the discrete variable in the data.frame
#' across which the variables of interest are to be analyzed or displayed.
#' @param across.subset Character vector. The groups of interest that the grouping variable
#' denoted in \code{across} contains.
#'
#' If there are more groups you are interested in than they are groups you are not interested
#' in specify those that you are not interested in prefixed with an \emph{'-'}.
#' Variables prefixed that way are discarded and the remaining are kept.
#'
#' @param display.facets Logical value. If set to TRUE the plot is split via
#' \code{ggplot2::facet_wrap()} such that each variable gets it's own subplot.
#'
#' @param pt.alpha Numeric value. Denotes the transperancy of the points.
#' @param pt.color Character value. Denotes the color with
#' which the points are displayed .
#' @param pt.num Numeric value. Regulates the number of points (sample size)
#' to prevent overplotting.
#' @param pt.shape Numeric or character value. If numeric, the respective
#' shape is taken for all points. If character, the respective variable is
#' mapped to the shape-aesthetic.
#' @param pt.size Numeric values. Denote size of the points.
#'
#' @param ref.group Character value. Denotes the reference group for the statistical tests. Must
#' be one value of the variable specified in \code{across}.
#'
#' @param relevel Logical value. If set to TRUE the input of \code{across.subset}
#' determines the new order in which the results are displayed.
#'
#' @param scales,nrow,ncol Given to \code{ggplot2::facet_wrap()}.
#'
#' @param test.pairwise Character value or NULL. If character, one of \emph{'none', 't.test', 'wilcox.test'}.
#' @param test.groupwise Character value or NULL. If character, one of \emph{'none', 'anova', 'kruskal.test'}.
#'
#' @param valid.classes Character vector to specify the classes the input
#' might have in order not to be discarded.
#'
#' @param variables Character vector. Denotes the variables
#' of interest. If set to NULL all valid variables of input for \code{df} are
#' considered (as long as the total number does not exceeds the limit).
#'
#' If there are more variables you are interested in than they are variables
#' you are not interested in specify those that you are not interested in prefixed
#' with an \emph{'-'}. Variables prefixed that way are discarded and the remaining are kept.
#'
#'
#' @inherit df params
#' @inherit check_across_subset params
#' @inherit ggplot2_dummy return
#' @inherit normalize params
#' @inherit verbose params
#'
#'

argument_dummy <- function(df, normalize, verbose){}


