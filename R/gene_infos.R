





# functions to assemble gene_info_df (not exported) -----------------------

pattern_end_1 <- "\\[provided by .*, .{1,10}\\d\\d\\d\\d\\]"
pattern_end_2 <- "and.{1,10}other tissues"
pattern_end_3 <- stringr::str_c(pattern_end_1, pattern_end_2, sep = "|")

pattern_hgnc_symbol <- "[A-Z|0-9|\\-]*"


assemble_gene_search_dir <- function(gene){

  if(base::is.character(gene)){

    out <-
      stringr::str_c(
        "https://www.ncbi.nlm.nih.gov/gene?term=(",
        gene,
        "%5BGene%20Name%5D)%20AND%20Homo%20sapiens%5BOrganism%5D"
      )

  } else if(base::is.numeric(gene)) {

    out <- stringr::str_c("https://www.ncbi.nlm.nih.gov/gene/",gene)

  }

  return(out)

}

# crop down html to gene card
crop_gene_info <- function(text){

  confuns::str_extract_after(string = text, pattern = "Symbol") %>%
    confuns::str_extract_before(pattern = pattern_end_3)

}

# extract html webpage and return as character vector
get_html_text <- function(gene){

  assemble_gene_search_dir(gene) %>%
    rvest::read_html() %>%
    rvest::html_element(css = "div") %>%
    rvest::html_text() %>%
    base::as.character() %>%
    stringr::str_remove_all(pattern = "\\n")

}


# extract specific infos from card:

extract_full_gene_name <- function(text){

  prel_out <-
    confuns::str_extract_after(
      string = text,
      pattern = "Full Name|Full name|Gene Description|Gene description"
    )

  out <-
    confuns::str_extract_before(
      string = prel_out,
      pattern = "provided by HGNC"
    )

  if(base::is.na(out)){

    out <-
      confuns::str_extract_before(
        string = prel_out,
        pattern = "Gene type"
      )

  }

  return(out)

}

extract_gene_id <- function(text){

  str_extract_after(string = text, pattern = "Gene ID:", match = " *\\d*") %>%
    base::as.numeric()

}

extract_gene_symbol <- function(text){

  confuns::str_extract_after(string = text, pattern = "Official {1,10}Symbol|Gene symbol|Symbol") %>%
    stringr::str_extract(string = ., pattern = )

}


extract_gene_type <- function(text){

  confuns::str_extract_after(string = text, pattern = "Gene type") %>%
    confuns::str_extract_before(string = ., pattern = "RefSeq status")

}

extract_organism <- function(text){

  test <- stringr::str_detect(string = text, pattern = "Organism.{1,20}Homo sapiens")

  if(base::isTRUE(test)){

    out <- "Homo sapiens"

  } else {

    out <- "Other"

  }

  return(out)

}

extract_summary <- function(text){

  prel_out <- confuns::str_extract_after(string = text, pattern = "Summary")

  if(!base::is.na(prel_out)){

    # matches ending of a sentence with a dot
    if(stringr::str_detect(string = prel_out, pattern = "[A-Z|a-z|0-9]\\.")){

      out <- prel_out

    } else {

      out <-
        confuns::str_extract_before(
          string = prel_out,
          pattern = "\\[provided by .*, .{1,10}\\d\\d\\d\\d\\]"
        )

    }

    if(base::is.na(out)){

      out <-
        confuns::str_extract_before(string = prel_out, pattern = "Broad expression in") %>%
        confuns::str_extract_before(string = ., pattern = "Expression")

    }

  } else {

    out <- NA

  }

  return(out)

}

extract_synonyms <- function(text){

  confuns::str_extract_after(string = text, pattern = "Also known as") %>%
    confuns::str_extract_before(string = ., pattern = "Summary")

}


# wrapper that creates data.frame

assemble_gene_info <- function(gene, pb){

  pb$tick()

  text <- get_html_text(gene)

  organism <- extract_organism(text)

  cropped_text <- crop_gene_info(text)

  if(base::is.character(gene)){

    symbol <- gene
    id <- extract_gene_id(text)

  } else if(base::is.numeric(gene)){

    symbol <- extract_gene_symbol(text)
    id <- gene

  }

  out <-
    base::data.frame(
      id = id,
      symbol = symbol,
      full_name = extract_full_gene_name(text),
      synonyms = extract_synonyms(cropped_text),
      summary = extract_summary(cropped_text),
      type = extract_gene_type(text),
      organism = organism
    ) %>%
    tibble::as_tibble()


  return(out)

}

make_gene_info_list <- function(genes){

  glist <- base::vector(mode = "list", length = base::length(genes))

  pb <- create_progress_bar(total = base::length(genes))

  for(i in seq_along(genes)){

    gene <- genes[i]

    gene_df <- base::tryCatch({

      assemble_gene_info(gene = gene, pb = pb)

    }, error = function(error){

      message(glue::glue("Failed for gene {gene}."))

      NA

    })

    glist[[i]] <- gene_df

  }

  return(glist)

}



# functions to work with gene info ----------------------------------------



#' @title Obtain gene names
#'
#' @description Returns a vector gene names (HGNC symbols). Defaults to all gene names
#' in the gene info data.frame \code{gene_info_df}.
#'
#' @param ... Use tidyselect grammer to select specific genes.
#'
#' @return Character vector of HGNC symbols in case of \code{get_gene_names()}
#' and of full gene names in case of \code{get_full_gene_names()}.
#'
#' @export
#'
#' @examples
#'
#' # all gene names
#' all_gene_names <- get_gene_names()
#'
#' # gene names that start with CD
#' cd_genes <- get_gene_names(starts_with("CD))
#'
#'
get_gene_names <- function(...){

  confuns::vselect(input = gene_info_df[["symbol"]], ...)

}

#' @rdname get_gene_names
#' @export
get_full_gene_names <- function(...){

  confuns::vselect(input = gene_info_df[["full_name"]], ...)

}



#' @title Obtain gene synonyms
#'
#' @description Extracts synonyms with which to refer to a gene.
#'
#' @param gene Character value. HGNC symbol of the gene of interest.
#'
#' @return Character vector. NA if there are no synonyms.
#' @export
#'
get_gene_synonyms <- function(gene){

  confuns::is_value(x = gene, mode = "character")

  confuns::check_one_of(
    input = gene,
    against = gene_info_df[["symbol"]]
  )

  dplyr::filter(gene_info_df, symbol == {{gene}}) %>%
    dplyr::pull(synonyms) %>%
    stringr::str_split(pattern = ";") %>%
    purrr::flatten_chr() %>%
    stringr::str_remove_all(string = ., pattern = " ")

}

#' @export
is_gene_symbol <- function(input){

  input %in% gene_info_df[["symbol"]]

}

#' @export
is_gene_synonym <- function(input){

  synonyms <-
    dplyr::filter(gene_info_df, !base::is.na(synonyms)) %>%
    dplyr::pull(synonyms) %>%
    stringr::str_c(collapse = "; ") %>%
    stringi::stri_split(str = ., regex = "; ")

  synonym_vec <- synonyms[[1]]

  input %in% synonym_vec

}


# assembles
make_gene_card <- function(gene){

  sgdf <- dplyr::filter(gene_info_df, symbol == {{gene}})

  if(base::nrow(sgdf) == 1){

    header <- stringr::str_c("Name: ", sgdf$full_name, " (", sgdf$symbol, ")")

    sub <- stringr::str_c("Type: ", sgdf$type)

    synonyms <- stringr::str_c("Synonyms: ", sgdf$synonyms)

    if(base::is.na(synonyms)){ synonyms <- "Synonyms: None"}

    summary <- stringr::str_c("\n", sgdf$summary)

    if(base::is.na(summary)){ summary <- "No summary exists."}

    out <- c(header, sub, synonyms, summary)

  } else if(base::nrow(sgdf) == 0) {

    out <-
      glue::glue("Did not find gene '{gene}'.") %>%
      base::as.character()

  }

  return(out)

}



#' @title Print gene information
#'
#' @description Prints human readable summary texts of genes in the console.
#'
#' @param genes Character vector of gene names.
#' @param ignore_case Logical value. If `TRUE`, ignores case when matching gene
#' names. (Gene symbols are stored with capitalized letters. Providing *'Gfap'*
#' as input for `genes` will fail if `ignore_case` is `FALSE` as the symbol
#' under which the gene is stored is written as *'GFAP'*).
#' @param use_synonyms Logical value. If `TRUE`, genes that were not matched via
#' their official gene symbol are looked for via their known synonyms.
#' @param check Logical value. If `TRUE`, input is checked for availability. If
#' FALSE, unknown elements are silently dropped.
#' @param ... Additional arguments given to [filter_genes()]. Is only applied if ...
#' is not empty.
#'
#' @inherit synonyms_to_hgnc params
#'
#' @return Invisible `TRUE`. Texts are immediately printed using \code{base::writeLines()}.
#' @export
#'
#' @details See documentation for `?gene_info_df` to read about how the source
#' data.frame has been generated.
#'
#' @examples
#'
#'  # genes are checked for validity by default
#'  print_gene_info(genes = c("GFAP", "MAG", "OLIG1", "XYZ"))
#'
#'  # set check = FALSE to drop unknown genes silently
#'  print_gene_info(genes = c("GFAP", "MAG", "OLIG1", "XYZ"), check = FALSE)
#'
#'  # use arguments of filter_genes()
#'  print_gene_info(c("KCNH2", "NRROS"))
#'  print_gene_info(c("KCNH2", "NRROS"), catchphrase = "development")
#'
#'
print_gene_info <- function(genes,
                            use_synonyms = TRUE,
                            ignore_case = TRUE,
                            check = TRUE,
                            warn = TRUE,
                            ...){

  genes <- base::unique(genes)
  all_symbols <- base::unique(gene_info_df[["symbol"]])

  if(base::isTRUE(ignore_case)){

    genes <- base::toupper(genes)

  }

  if(base::isTRUE(use_synonyms)){

    found <- genes[genes %in% all_symbols]
    not_found <- genes[!genes %in% all_symbols]

    found_by_syn <- synonyms_to_hgnc(input = not_found, verbose = FALSE, warn = warn)

    genes <- base::unique(c(found, found_by_syn))

    check <- FALSE

  }

  if(base::isTRUE(check) && base::length(not_found) >= 1){

    confuns::check_one_of(input = genes, against = all_symbols)

  }

  if(!purrr::is_empty(x = list(...))){

    genes <- filter_genes(..., genes_subset = genes)

  }

  purrr::walk(
    .x = genes,
    .f = function(gene){

      base::writeLines(stringr::str_c(base::rep("-", 50), collapse = ""))

      make_gene_card(gene = gene) %>%
        base::writeLines()

    }
  )

  base::invisible(TRUE)

}




#' @title Search for genes
#'
#' @description Goes through the summary text of genes and returns
#' gene names that match the requirements.
#'
#' @param catchphrases Character vector of catchphrases to look for.
#' @param test Character value. Determines how the input of \code{catchphrases}
#' is used to skim the genes.
#'
#'  \itemize{
#'   \item{`test = 'any'`:}{ Genes are kept if at least one of the catchphrases
#'    appears in the summary.}
#'   \item{`test = 'all'`:}{ Genes are kept if all of the catchphrases appear
#'    in the summary.}
#'   \item{`test = 'none'`:}{ Genes are kept if none of the catchphrases appear
#'    in the summary}
#'   }
#'
#' @param case_sensitive Logical value. If `TRUE`, the function pays attention
#' to capital- and non-capital letters. E.g. if `catchphrases = 'astrocyte'`
#' but the text of a gene only features *Astrocyte* the gene is not included
#' in the output as the text features astrocyte in upper case. Setting `case_sensitive`
#' to `FALSE`, the default, makes the function ignore upper and lower cases.
#'
#' @param standalone Logical value. If `TRUE`, catchphrases must appear as
#' standalone words. If `FALSE`, they can be part of other words.
#'
#' @param genes_subset Character vector or `NULL`. If character, subsets
#' gene info data.frame accordingly such that the test is only applied to
#' the provided genes.
#' @param genes_rm Character vector or `NULL`. If character, removes specified
#' genes from gene info data.frame such that the catchphrase test is only
#' applied to genes that are **not** specified.
#' @param return_var The variable of the \code{gene_info_df} that is returned
#' as a vector. Defaults to \emph{symbol}.
#' @param ... Logical tests given to \code{dplyr::filter()} that can be used to
#' prefilter the data.frame before going through the summary texts.
#'
#' @return Character vector of gene names.
#'
#' @details See documentation for `?gene_info_df` to read about how the source
#' data.frame has been generated.
#'
#' @export
#'
#' @examples
#'
#'  # create a small subset of genes for demonstration purposes
#'
#'  example_genes <- c("GFAP" , "MAG", "OLIG1", "EGFR")
#'
#'  print_gene_info(genes = example_genes)
#'
#'  # different tests
#'
#'  filter_genes(
#'   catchphrases = c("astrocytes", "surface"),
#'   test = "any",
#'   genes_subset = example_genes
#'   )
#'
#'  filter_genes(
#'   catchphrases = c("astrocytes", "surface"),
#'   test = "all",
#'   genes_subset = example_genes
#'   )
#'
#'
#'  filter_genes(
#'   catchphrases = c("astrocytes", "surface"),
#'   test = "none",
#'   genes_subset = example_genes
#'   )
#'
#'  # standalone TRUE or FALSE
#'
#'  filter_genes(
#'   catchphrases = c("glyco"),
#'   standalone = FALSE,
#'   genes_subset = example_genes
#'   )
#'
#'  filter_genes(
#'   catchphrases = c("glyco"),
#'   standalone = TRUE,
#'   genes_subset = example_genes
#'   )
#'
#'
#'
filter_genes <- function(catchphrases,
                         test = "any",
                         standalone = TRUE,
                         case_sensitive = FALSE,
                         return_var = "symbol",
                         genes_subset = NULL,
                         genes_rm = NULL,
                         ...){

  # prepare gene info data.frame
  gidf <- dplyr::filter(gene_info_df, ...)

  if(base::is.character(genes_subset)){

    gidf <- dplyr::filter(gidf, symbol %in% {{genes_subset}})

  }

  if(base::is.character(genes_rm)){

    gidf <- dplyr::filter(gidf, !symbol %in% {{genes_subset}})

  }


  # process catchphrase input
  if(base::isFALSE(case_sensitive)){

    cp_low <- base::tolower(x = catchphrases)
    cp_up <- base::toupper(x = catchphrases)

    cp_cap <-
      purrr::map_chr(
        .x = catchphrases,
        .f = ~ make_capital_letters(.x, collapse.with = " ")
      )

    catchphrases <-
      purrr::map(
        .x = list(a = cp_low, b = cp_up, c = cp_cap),
        .f = ~ wrap_standalone(.x, do = standalone)
      ) %>%
      purrr::pmap(.l = ., .f = stringr::str_c, sep = "|") %>%
      purrr::flatten_chr()

  } else {

    catchphrases <- wrap_standalone(catchphrases, do = standalone)

  }


  if(test == "any"){

    pattern <- stringr::str_c(catchphrases, collapse = "|")

    df <- dplyr::filter(gidf, stringr::str_detect(summary, pattern = {{pattern}}))

  } else if(test == "all") {

    df <- gidf

    # iterate over all catchphrases and apply each as a test
    for(i in base::seq_along(catchphrases)){

      pattern <- stringr::str_c(catchphrases[i], collapse = "|")

      df <- dplyr::filter(df, stringr::str_detect(summary, pattern = {{pattern}}))

    }

  } else if(test == "none"){

    pattern <- stringr::str_c(catchphrases, collapse = "|")

    df <-
      dplyr::filter(
        .data = gidf,
        (!stringr::str_detect(summary, pattern = {{pattern}}))
        )

  }

  out <- df[[return_var]]

  return(out)

}


#' @title Map gene synonyms to HGNC symbols
#'
#' @description Maps gene synonyms to their official HGNC symbol.
#'
#' @param input Character vector of possible gene synonyms.
#' @param verbose Logical. If `TRUE`, informative message are printed in
#' the console about which elements of \code{input} could be matched
#' to which genes and which could not be mapped.
#' @param warn Logical. If `TRUE`, a warning is printed if elements
#' of argument `input` are neither identified as a gene symbol nor
#' as matching synonym.
#'
#' @details Elements of \code{input} that are, in fact, synonyms are
#' replaced by their official HGNC symbol. Elements that are already
#' HGNC symbols stay as they are. Elements that are neither are silently
#' dropped.
#'
#' @return Character vector.
#' @export
#'
#' @examples
#'
#' # give feedback and warn
#' synonyms_to_hgnc(input = c("GFAP", "M33", "XXX"), verbose = T, warn = T)
#'
#' # give only feedback and drop silently
#' synonyms_to_hgnc(input = c("GFAP", "M33", "XXX"), verbose = T, warn = F)
#'
#'
synonyms_to_hgnc <- function(input, verbose = FALSE, warn = TRUE, ...){

  confuns::is_vec(x = input, mode = "character")

  genes <- input[is_gene_symbol(input)]

  synonyms_inp <- input[is_gene_synonym(input)]

  neither <-  input[!input %in% c(genes, synonyms_inp)]

  if(base::length(synonyms_inp) >= 1){

    syn_pattern <-
      stringr::str_c("( |)", synonyms_inp, "(;| )") %>%
      stringr::str_c(collapse = "|")

    genes_by_syn <-
      dplyr::filter(
        .data = gene_info_df,
        stringr::str_detect(synonyms, pattern = {{syn_pattern}})
      )

    if(base::nrow(genes_by_syn) >= 1){

      for(i in 1:base::nrow(genes_by_syn)){

        gene <- base::as.character(genes_by_syn[i, "symbol"])

        gene_synonyms <- get_gene_synonyms(gene)

        mapped_syn <- base::unique(synonyms_inp[synonyms_inp %in% gene_synonyms])

        synonyms_inp[synonyms_inp %in% mapped_syn] <- gene

        if(base::length(mapped_syn) == 1){

          msg <- glue::glue("Mapping synonym '{mapped_syn}' to gene '{gene}'.")

          confuns::give_feedback(
            msg = msg,
            verbose = verbose,
            with.time = FALSE,
            ...
          )

        }

      }

    }

  }

  # merge genes and synonyms_inp (exchanged during for loop)
  out <- c(genes, synonyms_inp)

  if(base::length(neither) >= 1){

    neither <- confuns::scollapse(neither)

    msg <- glue::glue("Neither gene nor synonym: '{neither}'.")

     if(base::isTRUE(warn)){

       rlang::warn(msg)

     }

  }

  return(out)

}



