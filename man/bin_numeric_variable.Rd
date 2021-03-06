% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convenience-functions.R
\name{bin_numeric_variable}
\alias{bin_numeric_variable}
\title{Convert a numeric variable to a discrete one}
\usage{
bin_numeric_variable(df, num_variable, discr_variable, n_bins)
}
\arguments{
\item{df}{A data.frame containing at least the numeric variable specified in \code{num_variable}.}

\item{num_variable}{Character value. The name of the numeric variable that you want
to convert.}

\item{discr_variable}{Character value. The name the new discrete variable wil have.}

\item{n_bins}{Numeric value. The number of bins you want to distribute the
values of \code{num_variable} to. Given to argument \code{n} of \code{dplyr::ntile()}.}
}
\value{
The data.frame specified in \code{data} with the additional discrete variable.
}
\description{
A wrapper around \code{dplyr::ntile()} to bin a numeric feature
into a discrete one.
}
