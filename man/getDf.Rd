% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-generics.R, R/S4-AnalysisAspect.R,
%   R/S4-Analysis.R
\name{getDf}
\alias{getDf}
\alias{getDf,AnalysisAspect-method}
\alias{getDf,Analysis-method}
\title{Obtain object data}
\usage{
getDf(object, ...)

\S4method{getDf}{AnalysisAspect}(
  object,
  complete = TRUE,
  grouping = FALSE,
  logical = FALSE,
  numeric = FALSE,
  meta = FALSE
)

\S4method{getDf}{Analysis}(
  object,
  complete = TRUE,
  grouping = FALSE,
  logical = FALSE,
  numeric = FALSE,
  meta = FALSE
)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}

\item{grouping, logical, numeric}{Logical value. Indicate if the respective variable
types should be part of the output data.frame.}

\item{meta}{Logical value. Indicates if the data.frame in slot @meta should
be joined to the output data.frame.}
}
\value{
A data.frame with subclass \code{tibble}.
}
\description{
Extracts the objects data as a data.frame.
}
