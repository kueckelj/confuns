% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/documentation-dummies.R
\name{argument_dummy}
\alias{argument_dummy}
\title{Argument dummy}
\usage{
argument_dummy(df, normalize, verbose)
}
\arguments{
\item{df}{A data.frame.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}

\item{across}{Character value. Specifies the discrete variable in the data.frame
across which the variables of interest are to be analyzed or displayed.}

\item{across.subset}{Character vector. The groups of interest that the grouping variable
denoted in \code{across} contains.

If there are more groups you are interested in than they are groups you are not interested
in specify those that you are not interested in prefixed with an \emph{'-'}.
Variables prefixed that way are discarded and the remaining are kept.}

\item{alpha.by, alpha_by, shape.by, shape_by, size.by, size_by}{Character
value or NULL. If character, specifies the variable that is mapped to the respective
aesthetic of the plot.}

\item{alpha.trans, alpha_trans, color.trans, color_trans, shape.trans, shape_trans, size.trans, size_trans}{Character value. Name of the transformation method applied to the continuous scale. Use
\code{validScaleTransformations()} to obtain all valid input options. Use \emph{'identity'}
for no transformation.}

\item{color.by, color_by}{Character value or NULL. If character, specifies the
variable that is displayed by color.}

\item{clrp}{Character value. Specifies the color palette to be used to represent
groups of discrete variables. Run \code{validColorPalettes()} to obtain valid
input options.}

\item{clrsp, pt.clrsp, pt_clrsp}{Character value. Specifies the color spectrum to be used to represent
continuous values of numeric variables. Run \code{validColorSpectra()} to obtain
valid input options.}

\item{clrp.adjust, clrp_adjust}{Named character vector. Can be used to adjust colors
with which groups are displayed. Names indicate groups, values
indicate the colors with which specific groups are supposed to be displayed.}

\item{color.aes, color_aes}{Either \emph{'color'} or \emph{'fill'}. Specifies the
aesthetic that is used to visualize the variable that is specified in \code{color_by}.
Ignored if \code{color_by} is NULL.}

\item{display_labels}{Lgoical value. Indicates if labels are displayed.}

\item{display_legend, display_title}{Logical values. Indicate if legend or
title is displayed.}

\item{display.points}{Logical value. If set to TRUE points are used additionally
to display the results.}

\item{display.facets}{Logical value. If set to TRUE the plot is split via
\code{ggplot2::facet_wrap()} such that each variable gets it's own subplot.}

\item{display.grid, display_grid}{Logical value. If TRUE, a grid is displayed.}

\item{facet.with, facet_with}{Character value. Either \emph{'grid'} or \emph{'wrap'}.
Specifies the function with which the plot-facetting is created. If \code{across}
is of length 2 and \code{facet_with} = \emph{'wrap'} \code{ggplot2::facet_wrap()}
is used. Else \code{ggplot2::facet_grid()} is used.}

\item{force}{Logical value. Must be set to TRUE to allow overwriting.}

\item{grid.alpha, grid_alpha, grid.size, grid_size}{Numeric values. Specify transparency
and thickness of the lines of the grid.}

\item{grid.color, grid_color}{Character value. Specifies the color of the grid lines.}

\item{h}{Numeric value. Denotes the height at which the dendrogram is cut.}

\item{hs}{Numeric vector. Denotes the heights at which the dendrogram is cut.}

\item{key.name, key_name}{Character value or NULL. Denotes the variable that is used
to identify each observation uniquely.}

\item{key.prefix, key_prefix}{Character value. The string with which
the IDs in the newly constructed key variable are prefixed if \code{key_name} is
NULL.}

\item{k}{Numeric value. Denotes the number of clusters. Must be bigger than 1. Must
not be bigger than the number of observations of the data set.}

\item{ks}{Numeric vector. Denotes all options for k-clusters. Values <1 are discarded.
Is converted into an integer vector.}

\item{labels.angle, labels_angle}{Numeric value. The angle with which labels are displayed.}

\item{labels.size, labels_size}{Numeric value. The size with which labels are displayed.}

\item{labels.hjust, labels.vjust, labels_hjust, labels_vjust}{Numeric values. Adjust labels positioning.}

\item{line.alpha, line_alpha, line.size, line_size}{Numeric value. Specifies the transparency
and the thickness of lines in the plot.}

\item{line.color, line_color}{Character value. Specifies the color of lines in the plot.}

\item{line.type, line_type}{Character value. Specifies the type of the lines. Use
\code{validLineTypes()} to obtain all valid input options.}

\item{method.aggl, method_aggl}{Character value. Specifies the agglomerative method
of interest. Use \code{validMethodsAggl()} to obtain all valid input options.}

\item{methods.aggl, methods_aggl}{Character vector. Specifies the agglomerative
methods of interest. Use \code{validMethodsAggl()} to obtain all valid input options.}

\item{method.corr, method_corr}{Character value. Specifies the correlation
method of interest. Use \code{validMethodsCorrelation()} to obtain all valid
input options.}

\item{methods.corr, methods_corr}{Character vector. Specifies the correlation
methods of interest. Use \code{validMethodsCorrelation()} to obtain all valid
input options.}

\item{method.dist, method_dist}{Character value. Specifies the distance method
of interest. Use \code{validMethodsDist()} to obtain all valid input options.}

\item{methods.dist, methods_dist}{Character vector. Specifies the distance methods
of interest. Use \code{validMethodsDist()} to obtain all valid input options.}

\item{method.kmeans, method_kmeans}{Character value. Specifies the kmeans method
of interest. Use \code{validMethodsKmeans()} to obtain all valid input options.}

\item{methods.kmeans, methods_kmeans}{Character vector. Specifies the kmeans methods
of interest. Use \code{validMethodsKmeans()} to obtain all valid input options.}

\item{method.pam, method_pam}{Character value. Specifies the pam-method of interest.
Use \code{validMethodsPam()} to obtain all valid input options.}

\item{methods.pam, methods_pam}{Character value. Specifies the pam-method of interest.
Use \code{validMethodsPam()} to obtain all valid input options.}

\item{object}{Any object for whose class a method has been defined.}

\item{order.by, order_by}{Character value or NULL. If character, specifies the
numeric variable by which the points are arranged before plotting.}

\item{order.desc}{Logical value. If set to TRUE the way the points are ordered
is reversed.}

\item{pt.alpha, pt_alpha}{Numeric value. Specifies the transparency of points.}

\item{pt.color, pt_color}{Character value. Specifies the color with which all points are displayed .}

\item{pt.num, pt_num}{Numeric value. Species the number of points (sample size)
to prevent overplotting.}

\item{pt.shape, pt_shape}{Numeric or character value. If numeric, the respective
shape is taken for all points. If character, the respective variable is
mapped to the shape-aesthetic.}

\item{pt.size, pt_shape}{Numeric values. Specifies size of points.}

\item{pval_threshold}{Numeric value or NULL. If numeric, denotes the maximum p-value to be considered as
significant. Must no be higher than 0.05.}

\item{ref.group}{Character value. Specifies the reference group for the pairwise statistical test. Must
be among the groups the variable specified in \code{across} contains. If set to NULL the
first group found is taken.}

\item{relevel}{Logical value. If set to TRUE the input order of \code{across.subset} and
\code{variables} determines the order in which the groups of interest are displayed.}

\item{scales, nrow, ncol}{Given to \code{ggplot2::facet_wrap()}. Affects the way the subplots
are displayed.}

\item{smooth.alpha, smooth_alpha}{Numeric value. The transparency of the smoothed line.}

\item{smooth.color, smooth_color}{Character value. The color of the smoothed line.}

\item{smooth.method, smooth_method}{Character value. The smoothing method. Given to argument
\code{method} of \code{ggplot2::geom_smooth()}.}

\item{smooth.se, smooth_se}{Logical value. Indicates if the confidence intervals are
displayed.}

\item{stop_if_null}{Logical value. If TRUE and the function does not find the object to
extract an informative error is raised. Else the empty value - usually NULL - is returned.}

\item{transform.with, transform_with}{List or NULL. If list, can be used to transform continuous variables before plotting.
Names of the slots of the provided list refer to the variables. The content of the slot refers to the transforming functions.
Slot content can either be a character vector of function names. Use \code{validVarTransformations()} to obtain all valid character value inputs.
Or it can be a list of functions (and function names).}

\item{test.pairwise}{Character value or NULL. If character, one of \emph{'t.test', 'wilcox.test'}.}

\item{test.groupwise}{Character value or NULL. If character, one of \emph{'anova', 'kruskal.test'}.}

\item{valid.classes}{Character vector to specify the classes the input
might have in order not to be discarded.}

\item{values_alpha, values_size}{Numeric values. Denote transparency and size of
the displayed values.}

\item{values_color}{Character value. Denotes the color in which the values
are displayed.}

\item{values_digits}{Numeric value. Denotes the number of digits to which
the displayed values are rounded.}

\item{variables}{Character vector. Specifies the variables
of interest. If set to NULL all valid variables of the input data.frame are
considered (as long as the total number does not exceeds the limit).

If there are more variables you are interested in than they are variables
you are not interested in specify those that you are not interested in prefixed
with an \emph{'-'}. Variables prefixed that way are discarded and the remaining are kept.}

\item{x, y}{Character value. Variable to be plotted on the respective axis.}
}
\value{
Returns a ggplot-object that can be additionally customized according
to the rules of the ggplot2-framework.
}
\description{
Argument dummy
}
\keyword{internal}
