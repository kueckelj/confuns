% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-Outlier.R, R/S4-Analysis.R
\name{getOutlierIDs}
\alias{getOutlierIDs}
\alias{getOutlierIDs,OutlierDetectionIQR-method}
\alias{getOutlierIDs,OutlierDetection-method}
\alias{getOutlierIDs,Analysis-method}
\title{Obtain outlier ids}
\usage{
getOutlierIDs(object, ...)

\S4method{getOutlierIDs}{OutlierDetectionIQR}(
  object,
  variables = NULL,
  across = NULL,
  across_subset = NULL,
  flatten = FALSE,
  ...
)

\S4method{getOutlierIDs}{OutlierDetection}(
  object,
  method = "IQR",
  variables = NULL,
  across = NULL,
  across_subset = NULL,
  flatten = FALSE
)

\S4method{getOutlierIDs}{Analysis}(
  object,
  variables = NULL,
  across = NULL,
  across_subset = NULL,
  flatten = FALSE
)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}

\item{variables}{Character vector. Specifies the variables
of interest. If set to NULL all valid variables of the input data.frame are
considered (as long as the total number does not exceeds the limit).

If there are more variables you are interested in than they are variables
you are not interested in specify those that you are not interested in prefixed
with an \emph{'-'}. Variables prefixed that way are discarded and the remaining are kept.}

\item{across}{Character value. Specifies the discrete variable in the data.frame
across which the variables of interest are to be analyzed or displayed.}

\item{flatten}{Logical value. If \code{across} is specified
\code{flatten} can be set to TRUE in order to force the
flatten teh list into a character vector of IDs.}
}
\value{
Character vector if \code{across} = NULL. List of character
vectors if \code{across} is a character value.
}
\description{
Extracts IDs that identify the observations
that were determined as outliers.
}
