





# functions to assemble gene_info_df (not exported) -----------------------

pattern_end_1 <- "\\[provided by .*, .{1,10}\\d\\d\\d\\d\\]"
pattern_end_2 <- "and.{1,10}other tissues"
pattern_end_3 <- stringr::str_c(pattern_end_1, pattern_end_2, sep = "|")

pattern_hgnc_symbol <- "[A-Z|0-9]*"

# extract html webpage and return as character vector
get_html_text <- function(id){

  rvest::read_html(x = stringr::str_c("https://www.ncbi.nlm.nih.gov/gene/",id)) %>%
    rvest::html_element(css = "div") %>%
    rvest::html_text() %>%
    base::as.character() %>%
    stringr::str_remove_all(pattern = "\\n")

}

# crop down html to gene card
crop_gene_info <- function(text){

  confuns::str_extract_after(string = text, pattern = "Symbol") %>%
    confuns::str_extract_before(pattern = pattern_end_3)

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

extract_gene_symbol <- function(text){

  confuns::str_extract_after(string = text, pattern = "Official {1,10}Symbol|Gene symbol|Symbol") %>%
    stringr::str_extract(string = ., pattern = pattern_hgnc_symbol)

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

assemble_gene_info <- function(id, pb){

  pb$tick()

  text <- get_html_text(id)

  cropped_text <- crop_gene_info(text)

  base::data.frame(
    id = id,
    symbol = extract_gene_symbol(text),
    full_name = extract_full_gene_name(text),
    synonyms = extract_synonyms(cropped_text),
    summary = extract_summary(cropped_text),
    type = extract_gene_type(text),
    organism = extract_organism(text)
  ) %>%
    tibble::as_tibble()

}

make_gene_info_list <- function(ids){

  glist <- base::vector(mode = "list", length = base::length(ids))

  pb <- create_progress_bar(total = base::length(ids))

  for(i in seq_along(ids)){

    id <- ids[i]

    gene_df <- base::tryCatch({

      assemble_gene_info(id = id, pb = pb)

    }, error = function(error){

      message(glue::glue("Failed for gene id {id}."))

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
#'
#' @return Invisible TRUE. Texts are immediately printed using \code{base::writeLines()}.
#' @export
#'
print_gene_info <- function(genes, check = TRUE){

  genes <- base::unique(genes)

  if(base::isTRUE(check)){

    confuns::check_one_of(
      input = genes,
      against = base::unique(gene_info_df[["symbol"]])
    )

  } else {

    genes <- genes[genes %in% base::unique(gene_info_df[["symbol"]])]

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
#' gene names that fit the requirements.
#'
#' @param catchphrases Character vector of catchphrases to look for.
#' @param test Character value. Determines how the input of \code{catchphrases}
#' is used to skim the genes. If \emph{'any'} genes are kept if at least
#' one of the catchphrases appears in the summary. If \emph{'all'}, genes are kept if all
#' of the catchphrases appear in the summary. If \emph{'none'}, genes are kept
#' if none of the catchphrases appear in the summary.
#' @param return_var The variable of the \code{gene_info_df} that is returned
#' as a vector. Defaults to \emph{symbol}.
#' @param ... Logical tests given to \code{dplyr::filter()} that can be used to
#' prefilter the data.frame before going through the summary texts.
#'
#' @return Character vector of gene names.
#' @export
#'
search_gene_names <- function(catchphrases, test = "any", return_var = "symbol", ...){

  gidf <- dplyr::filter(gene_info_df, ...)

  if(test == "any"){

    pattern <- stringr::str_c(catchphrases, collapse = "|")

    df <- dplyr::filter(gidf, stringr::str_detect(summary, pattern = {{pattern}}))

  } else if(test == "all") {

    df <- gidf

    for(phrase in chatchphrases){

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
#' @param verbose Logical. If TRUE, informative message are printed in
#' the console about which elements of \code{input} could be matched
#' to which genes and which could not be mapped.
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
#' synonyms_to_hgnc(input = c("GFAP", "M33", "XXX"), verbose = T)
#'
#'
synonyms_to_hgnc <- function(input, verbose = TRUE, ...){

  confuns::is_vec(x = input, mode = "character")

  genes <- input[input %in% gene_info_df[["symbol"]]]

  gene_pattern <- stringr::str_c(genes, collapse = "|")

  possible_synonyms <- input[!input %in% genes]

  if(base::length(possible_synonyms) >= 1){

    syn_pattern <- stringr::str_c(string = possible_synonyms, collapse = "|")

    genes_by_syn <-
      dplyr::filter(
        .data = gene_info_df,
        stringr::str_detect(synonyms, pattern = {{syn_pattern}})
      )

    if(base::nrow(genes_by_syn) >= 1){

      for(i in 1:base::nrow(genes_by_syn)){

        gene <- base::as.character(genes_by_syn[i, "symbol"])

        gene_synonyms <- get_gene_synonyms(gene)

        mapped_syn <- base::unique(possible_synonyms[possible_synonyms %in% gene_synonyms])

        possible_synonyms[possible_synonyms %in% mapped_syn] <- gene

        input[input == mapped_syn] <- gene

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

  # remove not matched synonyms
  out <- input[input %in% gene_info_df[["symbol"]]]

  dropped <- input[!input %in% gene_info_df[["symbol"]]]

  if(base::length(dropped) >= 1){

    dropped <- confuns::scollapse(dropped)

    confuns::give_feedback(
      msg = glue::glue("Did not find gene match for '{dropped}'"),
      verbose = verbose,
      with.time = FALSE,
      ...
    )

  }

  return(out)

}



