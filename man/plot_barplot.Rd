% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-statistics-single.R
\name{plot_barplot}
\alias{plot_barplot}
\title{Plot distribution of discrete/categorical variables}
\usage{
plot_barplot(
  df,
  variables = NULL,
  across = NULL,
  across.subset = NULL,
  relevel = TRUE,
  display.facets = TRUE,
  nrow = NULL,
  ncol = NULL,
  clrp = "milo",
  clrp.adjust = NULL,
  position = "dodge",
  ...
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

\item{across}{Character value. Specifies the discrete variable in the data.frame
across which the variables of interest are to be analyzed or displayed.}

\item{across.subset}{Character vector. The groups of interest that the grouping variable
denoted in \code{across} contains.

If there are more groups you are interested in than they are groups you are not interested
in specify those that you are not interested in prefixed with an \emph{'-'}.
Variables prefixed that way are discarded and the remaining are kept.}

\item{relevel}{Logical value. If set to TRUE the input order of \code{across.subset} and
\code{variables} determines the order in which the groups of interest are displayed.}

\item{display.facets}{Logical value. Only relevant if \code{across} is set
to NULL. Denotes if a subplot for each variable is supposed to be created.}

\item{clrp}{Character value. Specifies the color palette to be used to represent
groups of discrete variables. Run \code{validColorPalettes()} to obtain valid
input options.}

\item{clrp.adjust}{Named character vector or NULL. If character, it adjusts the
colors that are used to represent the groups. Names of the input vector must refer
to the group and the respective named element denotes the color with which to
represent the group.}

\item{...}{Additional arguments given to the respective \code{ggplot2::geom_<plot_type>()}
function.}
}
\value{
Returns a ggplot-object that can be additionally customized according
to the rules of the ggplot2-framework.
}
\description{
This function visualizes the distribution of discrete
variable - argument \code{variables} accepts only values that refer
to discrete variables.
}
