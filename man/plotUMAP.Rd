% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R, R/S4-DimRed.R, R/S4-Analysis.R
\name{plotUMAP}
\alias{plotUMAP}
\alias{plotUMAP,DimRed-method}
\alias{plotUMAP,Analysis-method}
\title{Plot UMAP}
\usage{
plotUMAP(object, ...)

\S4method{plotUMAP}{DimRed}(
  object,
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

\S4method{plotUMAP}{Analysis}(
  object,
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

\item{clrp}{Character value. Specifies the color palette to be used to represent
groups of discrete variables. Run \code{validColorPalettes()} to obtain valid
input options.}
}
\value{
A ggplot.
}
\description{
Plots UMAP results in a scatterplot.
}
