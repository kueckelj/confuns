% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R, R/S4-AnalysisAspect.R,
%   R/S4-Analysis.R
\name{plotScatterplot}
\alias{plotScatterplot}
\alias{plotScatterplot,AnalysisAspect-method}
\title{Plot a scatterplot}
\usage{
plotScatterplot(object, ...)

\S4method{plotScatterplot}{AnalysisAspect}(
  object,
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
  corr_p_min = 5e-05,
  corr_pos_x = NULL,
  corr_pos_y = NULL,
  corr_text_sep = "\\n",
  corr_text_size = 1,
  transform_with = NULL,
  ...
)

\S4method{plotScatterplot}{AnalysisAspect}(
  object,
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
  corr_p_min = 5e-05,
  corr_pos_x = NULL,
  corr_pos_y = NULL,
  corr_text_sep = "\\n",
  corr_text_size = 1,
  transform_with = NULL,
  ...
)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}

\item{x, y}{Character value. The variable to plot on the respective axis.}

\item{across}{Character vector or NULL. If character, the names of the grouping variables
that contain the group assignments across which results are displayed. If NULL, no grouping
is done.}

\item{across_subset}{List of character vectors. Must be named according
to the input of argument \code{across}. Named slots of the list should be character
vectors containing the groups of interest. Prefixing group names with
\emph{'-'} excludes them.}

\item{relevel}{Logical value. If set to TRUE the input order of \code{across.subset} and
\code{variables} determines the order in which the groups of interest are displayed.}

\item{scales, nrow, ncol}{Given to \code{ggplot2::facet_wrap()}. Affects the way the subplots
are displayed.}

\item{clrp}{Character value. Specifies the color palette to be used to represent
groups of discrete variables. Run \code{validColorPalettes()} to obtain valid
input options.}

\item{display_smooth}{Logical value. Indicates if a smoothed line is displayed.}

\item{display_corr}{Logical value. Indicates if correlation values of the x-
and y-variable are computed and displayed as text in the plot.}

\item{corr_method}{Character value. Specifies the correlation method.}

\item{corr_p_min}{Numeric value. Specifies the}

\item{corr_pos_x, corr_pos_y}{Numeric value or NULL. If numeric, specifies the exact
position of the text on the x- or y-axis.}

\item{corr_text_sep}{Character value. The string with which p-value and correlation
value are separated.}

\item{corr_text_size, pt_size, smooth_size}{Numeric value. The size with which
results are displayed.}
}
\value{
A ggplot.
}
\description{
Plots two numeric variables on the x- and y-axis.
}
