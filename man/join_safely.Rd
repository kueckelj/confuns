% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join-safely.R
\name{join_safely}
\alias{join_safely}
\title{Title}
\usage{
join_safely(
  old.df,
  new.df,
  ref.new.df,
  variable.names,
  by,
  valid.classes = "any",
  overwrite = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{old.df}{Baseline data.frame to which new variables are to be joined.}

\item{new.df}{Data.frame containing new variables.}

\item{variable.names}{Character vector of variable names to be joined.}

\item{by}{Character value. Denotes key variable.}

\item{overwrite}{Logical value.}

\item{verbose}{Logical value.}
}
\value{
Joined data.frame.
}
\description{
Title
}
