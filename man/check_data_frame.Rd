% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input-check.R
\name{check_data_frame}
\alias{check_data_frame}
\title{Check data.frame validity}
\usage{
check_data_frame(
  df,
  var.class = list(),
  ref = NULL,
  verbose = TRUE,
  with.time = FALSE,
  fdb.fn = "stop"
)
}
\arguments{
\item{df}{A data.frame.}

\item{var.class}{A named list. The names have to match the
variable names of the data.frame that are to be validated. The respective
elements specify the class the data.frame variable must have specified
as character strings.}

\item{ref}{Character value. Input reference for the error message.
If set to NULL the value of \code{x} is evaluated via non standard evalulation.}

\item{verbose}{Logical value. Indicates whether any kind of feedback is supposed to
be given. \code{verbose} set to FALSE shuts down any error, warning or general messages
and results in the functions returning what is specified in \code{return}.}

\item{with.time}{Logical value. Indicates whether the current time is to be
added to the feedback message.}

\item{fdb.fn}{Character value. Determines the function to call if a feedback
message needs to be given. One of \emph{'stop', 'warning'} or \emph{'message'}.}
}
\value{
An informative message, warning or error or TRUE if valid.
}
\description{
Checks whether the input data.frame contains variables
of certain classes and names.
}
\examples{
 # make sure that the input data.frame has
 # the numeric variables 'mpg' and 'cyl'.

 check_data_frame(df = mtcars,
                  var.class = list(mpg = "numeric",
                                   cyl = "numeric"))
}
