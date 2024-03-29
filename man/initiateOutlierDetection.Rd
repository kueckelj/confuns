% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-Outlier.R
\name{initiateOutlierDetection}
\alias{initiateOutlierDetection}
\title{Set up \code{OutlierDetection} object}
\usage{
initiateOutlierDetection(
  data,
  key_name,
  key_prefix = NULL,
  lgl_to_group = TRUE,
  meta_names = character(0),
  verbose = TRUE
)
}
\arguments{
\item{data}{A data.frame. (Rownames are dropped.)}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}
}
\value{
An object of S4-class \code{OutlierDetection}.
}
\description{
Sets up an object of class \code{OutlierDetection}.
}
\details{
If \code{key_name} is NULL the key variable is constructed
as a combination of the input data.frames rownames and the string
provided with \code{key_prefix}. If the the datas rownames are NULL
or contain \emph{""} the rownumbers are used instead. The newly constructed
key variable is named \emph{data_ids}.
}
