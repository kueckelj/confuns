% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R, R/S4-Clustering.R,
%   R/S4-Analysis.R
\name{getDendro}
\alias{getDendro}
\alias{getDendro,Clustering-method}
\alias{getDendro,Analysis-method}
\title{Obtain object of class \code{dendro}}
\usage{
getDendro(object, ...)

\S4method{getDendro}{Clustering}(
  object,
  method_dist = "euclidean",
  method_aggl = "ward.D",
  k = NULL,
  h = NULL,
  type = "rectangle"
)

\S4method{getDendro}{Analysis}(
  object,
  method_dist = "euclidean",
  method_aggl = "ward.D",
  k = NULL,
  h = NULL,
  type = "rectangle"
)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}

\item{k}{Numeric value. Denotes the number of clusters. Must be bigger than 1. Must
not be bigger than the number of observations of the data set.}

\item{h}{Numeric value. Denotes the height at which the dendrogram is cut.}
}
\value{
An object of class \code{dendro}. Contains data for argument
\code{data} of function \code{{ggdendro::ggdendrogram()}}.
}
\description{
Extracts the data from the computed \code{hclust} object
with which a dendrogram can be plotted.
}
