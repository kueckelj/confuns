% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gene_infos.R
\name{print_gene_info}
\alias{print_gene_info}
\title{Print gene information}
\usage{
print_gene_info(
  genes,
  use_synonyms = TRUE,
  ignore_case = TRUE,
  check = TRUE,
  warn = TRUE,
  ...
)
}
\arguments{
\item{genes}{Character vector of gene names.}

\item{use_synonyms}{Logical value. If \code{TRUE}, genes that were not matched via
their official gene symbol are looked for via their known synonyms.}

\item{ignore_case}{Logical value. If \code{TRUE}, ignores case when matching gene
names. (Gene symbols are stored with capitalized letters. Providing \emph{'Gfap'}
as input for \code{genes} will fail if \code{ignore_case} is \code{FALSE} as the symbol
under which the gene is stored is written as \emph{'GFAP'}).}

\item{check}{Logical value. If \code{TRUE}, input is checked for availability. If
FALSE, unknown elements are silently dropped.}

\item{warn}{Logical. If \code{TRUE}, a warning is printed if elements
of argument \code{input} are neither identified as a gene symbol nor
as matching synonym.}

\item{...}{Additional arguments given to \code{\link[=filter_genes]{filter_genes()}}. Is only applied if ...
is not empty.}
}
\value{
Invisible \code{TRUE}. Texts are immediately printed using \code{base::writeLines()}.
}
\description{
Prints human readable summary texts of genes in the console.
}
\details{
See documentation for \code{?gene_info_df} to read about how the source
data.frame has been generated.
}
\examples{

 # genes are checked for validity by default
 print_gene_info(genes = c("GFAP", "MAG", "OLIG1", "XYZ"))

 # set check = FALSE to drop unknown genes silently
 print_gene_info(genes = c("GFAP", "MAG", "OLIG1", "XYZ"), check = FALSE)

 # use arguments of filter_genes()
 print_gene_info(c("KCNH2", "NRROS"))
 print_gene_info(c("KCNH2", "NRROS"), catchphrase = "development")


}
