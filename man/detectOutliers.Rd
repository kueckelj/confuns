% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S4-Outlier.R, R/S4-Analysis.R
\name{detectOutliers}
\alias{detectOutliers}
\alias{detectOutliers,OutlierDetection-method}
\alias{detectOutliers,Analysis-method}
\title{Detect outliers}
\usage{
detectOutliers(object, ...)

\S4method{detectOutliers}{OutlierDetection}(object, method, across = NULL, verbose = TRUE, ...)

\S4method{detectOutliers}{Analysis}(object, method, across = NULL, verbose = TRUE)
}
\arguments{
\item{object}{Any object for whose class a method has been defined.}

\item{method}{Character value. The outlier detection method. Valid
input options are \emph{'IQR'} and \emph{'Mahalanobis'}.}

\item{across}{Character value or NULL. If character, denotes
the grouping variable of interest.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}
}
\value{
The input object.
}
\description{
Runs outlier detection according to the method
denoted in argument \code{method}.
}
