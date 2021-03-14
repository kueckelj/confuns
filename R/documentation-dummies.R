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
#' @param across Character value. Specifies the discrete variable in the data.frame
#' across which the variables of interest are to be analyzed or displayed.
#' @param across.subset Character vector. The groups of interest that the grouping variable
#' denoted in \code{across} contains.
#'
#' If there are more groups you are interested in than they are groups you are not interested
#' in specify those that you are not interested in prefixed with an \emph{'-'}.
#' Variables prefixed that way are discarded and the remaining are kept.
#'
#' @param clrp Character value. Specifies the color palette to be used to represent
#' groups of discrete variables. Run \code{all_color_palettes()} to obtain valid
#' input options.
#'
#' @param clrsp Chracter value. Specfies the color spectrum to be used to represent
#' continuous values of numeric variables. Run \code{all_color_spectra()} to obtain
#' valid input options.
#'
#' @param display.points Logical value. If set to TRUE points are used additionally
#' to display the results.
#' @param display.facets Logical value. If set to TRUE the plot is split via
#' \code{ggplot2::facet_wrap()} such that each variable gets it's own subplot.
#'
#' @param pt.alpha Numeric value. Specifies the transparency of points.
#' @param pt.color Character value. Specifies the color with which all points are displayed .
#' @param pt.num Numeric value. Species the number of points (sample size)
#' to prevent overplotting.
#' @param pt.shape Numeric or character value. If numeric, the respective
#' shape is taken for all points. If character, the respective variable is
#' mapped to the shape-aesthetic.
#' @param pt.size Numeric values. Specifies size of points.
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
#'
#' @param test.pairwise Character value or NULL. If character, one of \emph{'t.test', 'wilcox.test'}.
#' @param test.groupwise Character value or NULL. If character, one of \emph{'anova', 'kruskal.test'}.
#'
#' @param valid.classes Character vector to specify the classes the input
#' might have in order not to be discarded.
#'
#' @param variables Character vector. Specifies the variables
#' of interest. If set to NULL all valid variables of the input data.frame are
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


