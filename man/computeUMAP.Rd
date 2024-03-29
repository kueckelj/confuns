% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R, R/S4-DimRed.R, R/S4-Analysis.R
\name{computeUMAP}
\alias{computeUMAP}
\alias{computeUMAP,DimRed-method}
\alias{computeUMAP,Analysis-method}
\title{Compute UMAP}
\usage{
computeUMAP(object, ...)

\S4method{computeUMAP}{DimRed}(object, verbose = TRUE, ...)

\S4method{computeUMAP}{Analysis}(object, verbose = TRUE)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}

\item{...}{Additional arguments given to \code{umap::umap()}.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}
}
\value{
The input object.
}
\description{
Computes a manifold approximation and projection.
}
