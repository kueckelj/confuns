% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logical-tests.R
\name{is_key_variable}
\alias{is_key_variable}
\title{Test key variable validity}
\usage{
is_key_variable(df, key.name, stop.if.false = FALSE)
}
\arguments{
\item{df}{A data.frame.}

\item{key.name}{Character value. The name of the key variable.}
}
\value{
TRUE or FALSE
}
\description{
Tests if the denoted key variable identifies
each observation uniquely.
}
