% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R
\name{addClusterVarsKmeans}
\alias{addClusterVarsKmeans}
\title{Add kmeans cluster variables}
\usage{
addClusterVarsKmeans(object, ...)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}
}
\description{
Adds kmeans cluster results in form of grouping variables
to the data of the input object.
}
\details{
Iterates over all possible combinations of the arguments \code{ks}
and \code{methods_kmeans} and adds the corresponding grouping
variables to the objects data. Cluster variables are named corresponding to the input
of argument \code{naming}.
}