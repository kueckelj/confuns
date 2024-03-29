% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscellaneous-data-frame.R
\name{process_and_shift_df}
\alias{process_and_shift_df}
\title{Process a data.frame

A wrapper around \code{check_df_variables}, \code{check_across_subset()}
and \code{dplyr::pivot_longer()}.}
\usage{
process_and_shift_df(
  df,
  variables = NULL,
  valid.classes = NULL,
  across = NULL,
  across.subset = NULL,
  relevel = TRUE,
  keep = NULL,
  ref_df = NULL,
  verbose = TRUE
)
}
\arguments{
\item{df}{A data.frame.}

\item{variables}{Character vector. Specifies the variables
of interest. If set to NULL all valid variables of the input data.frame are
considered (as long as the total number does not exceeds the limit).

If there are more variables you are interested in than they are variables
you are not interested in specify those that you are not interested in prefixed
with an \emph{'-'}. Variables prefixed that way are discarded and the remaining are kept.}

\item{valid.classes}{Character vector to specify the classes the input
might have in order not to be discarded.}

\item{across}{Character value. Specifies the discrete variable in the data.frame
across which the variables of interest are to be analyzed or displayed.}

\item{across.subset}{Character vector. The groups of interest that the grouping variable
denoted in \code{across} contains.

If there are more groups you are interested in than they are groups you are not interested
in specify those that you are not interested in prefixed with an \emph{'-'}.
Variables prefixed that way are discarded and the remaining are kept.}

\item{relevel}{Logical value. If set to TRUE the input order of \code{across.subset} and
\code{variables} determines the order in which the groups of interest are displayed.}

\item{keep}{Character vector or NULL. If character, specifies variables
that are to be kept even if they are not of those classes denoted in
\code{valid.classes}. Variables specified like that are not included in
the pivoting process!}

\item{ref_df}{Character value. Given to argument \code{ref} of
function \code{check_data_frame()}.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}
}
\description{
Process a data.frame

A wrapper around \code{check_df_variables}, \code{check_across_subset()}
and \code{dplyr::pivot_longer()}.
}
