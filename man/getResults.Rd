% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R, R/S4-AnalysisAspect.R,
%   R/S4-Outlier.R
\name{getResults}
\alias{getResults}
\alias{getResults,AnalysisAspect-method}
\alias{getResults,OutlierDetection-method}
\title{Obtain analysis results}
\usage{
getResults(object, ...)

\S4method{getResults}{AnalysisAspect}(object, method)

\S4method{getResults}{OutlierDetection}(object, method)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}
}
\value{
Depends on the objects class.
}
\description{
Generic extractor for results mainly used for
programming purpose as it provides informative error
messages if the requested content is missing.
}
