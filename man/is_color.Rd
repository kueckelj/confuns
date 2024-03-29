% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input-check.R
\name{is_color}
\alias{is_color}
\title{Check if elements in a character vector represent valid colors.}
\usage{
is_color(vector)
}
\arguments{
\item{vector}{A character vector containing color names or hexadecimal color
codes.}
}
\value{
A logical vector of the same length as 'vector', where each element
is 'TRUE' if the corresponding element in 'vector' represents a valid
color, and 'FALSE' otherwise.
}
\description{
This function checks if each element in a character vector represents a valid
color. It performs two checks:
\itemize{
\item It tests whether each element in the vector matches the pattern of a valid
hexadecimal color code (e.g., "#RRGGBB").
\item It tests whether each element in the vector is one of the recognized color
names in R.
}
}
\examples{
is_color(c("#FF0000", "blue", "invalid", "green"))

}
