% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R, R/S4-AnalysisAspect.R,
%   R/S4-Analysis.R
\name{scaleData}
\alias{scaleData}
\alias{scaleData,AnalysisAspect-method}
\alias{scaleData,Analysis-method}
\title{Scale data}
\usage{
scaleData(object, ...)

\S4method{scaleData}{AnalysisAspect}(object, na_rm = TRUE, verbose = TRUE, ...)

\S4method{scaleData}{Analysis}(object, na_rm = TRUE, verbose = TRUE)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}

\item{na_rm}{Logical. If TRUE, NAs are ignored.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}
}
\value{
The input object.
}
\description{
Scales all numeric variables via zscore.
}
