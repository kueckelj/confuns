% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R, R/S4-Analysis.R
\name{addClusterVarsHclust}
\alias{addClusterVarsHclust}
\alias{addClusterVarsHclust,Analysis-method}
\title{Add hierarchical cluster variables}
\usage{
addClusterVarsHclust(object, ...)

\S4method{addClusterVarsHclust}{Analysis}(
  object,
  ks = NULL,
  hs = NULL,
  methods_dist = "euclidean",
  methods_aggl = "Ward.D",
  prefix = "",
  naming_k = "{method_dist}_{method_aggl}{k}",
  naming_h = "{method_dist}_{method_aggl}{h}",
  overwrite = FALSE
)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}

\item{ks}{Numeric vector. Denotes all options for k-clusters. Values <1 are discarded.
Is converted into an integer vector.}

\item{hs}{Numeric vector. Denotes the heights at which the dendrogram is cut.}
}
\description{
Adds hierarchical cluster results in form of grouping variables
to the data of the input object.
}
\details{
Iterates over all possible combinations of the arguments \code{ks}, \code{hs},
\code{methods_dist} and \code{methods_aggl} and adds the corresponding grouping
variables to the objects data. Cluster variables are named corresponding to the input
of arguments \code{naming_k} and \code{naming_h}.
}
