



#' Collection of gene information
#'
#' Collection of gene information stored in tidy data fashion.
#' Information has been retrieved from the gene data base of the
#' \emph{National Library of Medicine}. Lastly updated on 27.10.2022.
#'
#' @source \url{https://www.ncbi.nlm.nih.gov/gene/?term=}
#'
#' \describe{
#'   \item{id}{Integer. Gene ID in the data base. E.g. gene GFAP has ID 2670.
#'    Combine with \url{https://www.ncbi.nlm.nih.gov/gene/2670} to get to the
#'    webpage from which the information have been retrieved.}
#'   \item{symbol}{Character. HGNC symbol of the gene. (Common gene abbreviation.)}
#'   \item{full_name}{Character. Full gene name.}
#'   \item{synonyms}{Character. Synonyms with which to refer to the gene. Separated by \emph{';'}}
#'   \item{summary}{Character. Human readable summary text of the gene.}
#'   \item{type}{Character. Describes the general function of the gene. E.g. protein coding, pseudo,
#'   scRNA, snoRNA, snRNA, tRNA, etc.}
#'   \item{organism}{Character. The organism in which the gene appears. Currently only Homo sapiens.}
#' }
#'
#' @docType data
#' @usage data(gene_info_df)
#'
"gene_info_df"