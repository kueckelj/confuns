% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R, R/S4-Clustering.R,
%   R/S4-Analysis.R
\name{plotAvgSilWidths}
\alias{plotAvgSilWidths}
\alias{plotAvgSilWidths,Clustering-method}
\alias{plotAvgSilWidths,Analysis-method}
\title{Plot avg sil-width data}
\usage{
plotAvgSilWidths(object, ...)

\S4method{plotAvgSilWidths}{Clustering}(
  object,
  ks,
  methods_pam = "euclidean",
  display_cols = TRUE,
  col_alpha = 0.9,
  col_color = "black",
  col_fill = "steelblue",
  display_line = TRUE,
  line_alpha = 0.9,
  line_color = "black",
  line_size = 1.5,
  display_points = TRUE,
  pt_alpha = 0.9,
  pt_color = "black",
  pt_size = 4.5,
  ncol = NULL,
  nrow = NULL
)

\S4method{plotAvgSilWidths}{Analysis}(
  object,
  ks,
  methods_pam = "euclidean",
  display_cols = TRUE,
  col_alpha = 0.9,
  col_color = "black",
  col_fill = "steelblue",
  display_line = TRUE,
  line_alpha = 0.9,
  line_color = "black",
  line_size = 1.5,
  display_points = TRUE,
  pt_alpha = 0.9,
  pt_color = "black",
  pt_size = 4.5,
  ncol = NULL,
  nrow = NULL
)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}

\item{...}{}

\item{ks}{Numeric vector. Denotes all options for k-clusters. Values <1 are discarded.
Is converted into an integer vector.}
}
\value{
A ggplot.
}
\description{
Plots information about the average silhouette
widths of different clustering results with method \emph{pam}.
}
