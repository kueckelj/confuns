% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R, R/S4-DimRed.R, R/S4-Analysis.R
\name{plotTSNE}
\alias{plotTSNE}
\alias{plotTSNE,DimRed-method}
\alias{plotTSNE,Analysis-method}
\title{Plot TSNE}
\usage{
plotTSNE(object, ...)

\S4method{plotTSNE}{DimRed}(
  object,
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
  ...
)

\S4method{plotTSNE}{Analysis}(
  object,
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
  ...
)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}

\item{n_dims}{Numeric value. Integer that indicates the number of dimensions that are plotted.}

\item{clrp}{Character value. Specifies the color palette to be used to represent
groups of discrete variables. Run \code{validColorPalettes()} to obtain valid
input options.}
}
\value{
A ggplot.
}
\description{
Plots TSNE results in a scatterplot.
}
