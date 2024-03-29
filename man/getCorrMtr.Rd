% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R, R/S4-Correlation.R
\name{getCorrMtr}
\alias{getCorrMtr}
\alias{getCorrMtr,Correlation-method}
\title{Obtain correlation matrix}
\usage{
getCorrMtr(object, ...)

\S4method{getCorrMtr}{Correlation}(
  object,
  method_corr = "pearson",
  across = NULL,
  across_subset = NULL,
  type = "complete",
  diagonal = TRUE,
  flatten = TRUE
)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}

\item{across}{Character value. Specifies the discrete variable in the data.frame
across which the variables of interest are to be analyzed or displayed.}

\item{type}{Character value. Denotes how the underlying correlation matrix is
handled. Three options:

\itemize{
\item{\emph{'complete'}:}{ The matrix stays as is.},
\item{\emph{'lower'}:}{ The part below the diagonal is used. The upper part is set to NA.},
\item{\emph{'upper'}:}{The part above the diagonal is used. the lower part is set to NA.}
}}

\item{diagonal}{Logical value.  Indicates if the diagonal values of the
matrices are kept ( = TRUE) or set to NA (= FALSE). Ignored if \code{type}
= \emph{'complete'}.}
}
\description{
Extracts correlation results in form of correlation
matrices.
}
\details{
If \code{across} is a character a list of correlation matrices
is returned. Slots are named according to the groups of the denoted
grouping variable. If \code{across_subset} is NULL all groups are considered.
If \code{across_subset} is of length one and \code{flatten} is TRUE the
output list is flattened and a single correlation matrix is returned.
}
