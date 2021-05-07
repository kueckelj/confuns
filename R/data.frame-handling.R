#' @title Arrange rows
#'
#' @description Arranges the rows of a data.frame according to
#' the positions of their maxima or their minima. The earlier
#' a rows maximum/minimum appears the earlier the row will appear in the
#' returned data.frame.
#'
#' @param df A data.frame with at least one numeric variable.
#' @param according.to Character. Either \emph{'maxima} or \emph{'minima'}.
#' @param across Deprecated in favor of \code{according.to}.
#'
#' @inherit verbose params
#'
#' @return The arranged data.frame.
#' @export

arrange_rows <- function(df, according.to, verbose, across = NULL){

  base::stopifnot(base::is.data.frame(df))
  base::stopifnot(base::any(base::sapply(df, base::is.numeric)))

  if(!base::is.null(across)){ according.to <- across }

  give_feedback(
    msg = glue::glue("Arranging rows according to their {according.to}."),
    verbose = verbose
    )

  if(according.to == "maxima"){

    df$order <- sapply(1:nrow(df), function(i){base::which.max(df[i, base::sapply(df, base::is.numeric)])})
    df <- dplyr::arrange(df, order) %>% dplyr::select(-order) %>% base::as.data.frame()

  } else if(according.to == "minima") {

    df$order <- base::sapply(1:nrow(df), function(i){base::which.max(df[i, base::sapply(df, base::is.numeric)])})
    df <- dplyr::arrange(df, desc(order)) %>% dplyr::select(-order) %>% base::as.data.frame()

  }

  base::return(df)

}



#' @title Across input
#'
#' @description Returns input options for the \code{across} argument.
#'
#' @param df A data.frame
#' @param n.across.subset Numeric value. Denotes the maximal number of unique
#' groups the across options to be returned may contain.
#'

across_options <- function(df, n.across.subset = 25){

  base::stopifnot(base::is.data.frame(df))
  is_value(x = n.across.subset, mode = "numeric")

  numeric_names <-
    dplyr::select_if(df, .predicate = base::is.numeric) %>%
    base::colnames()

  all_across_options <-
    dplyr::select(df, -dplyr::all_of(x = numeric_names)) %>%
    base::colnames()

  if(base::length(all_across_options) == 0){ base::stop("No discrete variables in input for 'df'.") }

  valid_across_options <-
    purrr::map(.x = dplyr::select(df, dplyr::all_of(all_across_options)), .f = dplyr::n_distinct) %>%
    purrr::keep(.p = ~ .x <= n.across.subset) %>%
    base::names()

  if(base::length(valid_across_options) == 0){

    base::stop(glue::glue("No discrete variable in input for 'df' has a lower number of unique values than the defined minimum: {n.across.subset}."))

    }


  base::return(valid_across_options)

}

across_subset_options <- function(df, across){

  base::stopifnot(base::is.data.frame(df))
  is_value(x = across, mode = "character")

  across_var <- dplyr::pull(df, var = across)

  if(base::is.factor(across_var)){

    res <- base::levels(across_var)

  } else if(base::is.character(across_var)){

    res <- base::unique(across_var)

  } else {

    base::stop(glue::glue("Variable '{across}' must be of type 'character' or 'factor'."))

  }

  base::return(res)

}



#' @title Wrapper around dplyr::select()
#'
#' @param df A data.frame.
#' @param keep Columns that have to be kept.
#' @param contains,matches,starts.with,ends.with Character or NULL. If character
#' given to the respective tidyselect function as input for argument \code{match}.
#' @param negate Logical value. If set to TRUE negates the regex input of the tidyselect
#' input.
#'
#' @return Selected data.frame.
#'
#' @export
#'
select_columns <- function(df,
                           keep = NULL,
                           contains = NULL,
                           matches = NULL,
                           starts.with = NULL,
                           ends.with = NULL,
                           negate = FALSE,
                           return = "tibble",
                           fdb.fn = "stop"){

  if(base::is.character(keep)){

    df_keep <- dplyr::select(df, dplyr::all_of(x = keep))

    df <- dplyr::select(df, -dplyr::all_of(x = keep))

  }


  if(base::is.character(contains)){

    if(base::isTRUE(negate)){

      df <- dplyr::select(df, -dplyr::contains(match = {{contains}}))

    } else {

      df <- dplyr::select(df, dplyr::contains(match = {{contains}}))

    }

  }

  if(base::is.character(matches)){

    if(base::isTRUE(negate)){

      df <- dplyr::select(df, -dplyr::matches(match = {{matches}}))

    } else {

      df <- dplyr::select(df, dplyr::matches(match = {{matches}}))

    }

  }

  if(base::is.character(starts.with)){

    if(base::isTRUE(negate)){

      df <- dplyr::select(df, -dplyr::starts_with(match = {{starts.with}}))

    } else {

      df <- dplyr::select(df, dplyr::starts_with(match = {{starts.with}}))

    }

  }

  if(base::is.character(ends.with)){

    if(base::isTRUE(negate)){

      df <- dplyr::select(df, -dplyr::ends_with(match = {{ends.with}}))

    } else {

      df <- dplyr::select(df, dplyr::ends_with(match = {{ends.with}}))

    }

  }


  if(base::ncol(df) == 0){

    selection_input <-
      list(contains = contains, matches = matches, starts_with = starts.with, ends_with = ends.with) %>%
      purrr::keep(.p = base::is.character) %>%
      base::names()

    msg <- glue::glue("Variable selection via specification of {ref_argument} '{ref_input}' resulted in zero variables.",
                      ref_input = glue::glue_collapse(selection_input, sep = "', '", last = "' and '"),
                      ref_argument = adapt_reference(input = selection_input, "argument", "arguments")
                      )

    give_feedback(msg = msg, fdb.fn = fdb.fn, with.time = FALSE)


  }

  if(base::is.character(keep)){

    df <- base::cbind(df_keep, df)

  }

  if(return == "tibble"){

    df <- tibble::as_tibble(df)

  }

  base::return(df)

}
