% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R, R/S4-AnalysisAspect.R,
%   R/S4-Analysis.R
\name{getScaledMtr}
\alias{getScaledMtr}
\alias{getScaledMtr,AnalysisAspect-method}
\alias{getScaledMtr,Analysis-method}
\title{Obtain scaled matrix}
\usage{
getScaledMtr(object, ...)

\S4method{getScaledMtr}{AnalysisAspect}(object, ...)

\S4method{getScaledMtr}{Analysis}(object, na_rm = TRUE)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}
}
\value{
A matrix.
}
\description{
Extracts the scaled data as a matrix. Key variable is used
for the rownames.
}
