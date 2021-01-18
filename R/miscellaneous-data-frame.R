#' Subset the across-variables
#'
#' @description Checks across and across.subset input and if at least one
#' of the \code{across.subset} values exists if the letter is not NULL. It
#' then filters the input data.frame accordingly.
#'
#' @param df A data.frame that contains the grouping variable specified in \code{across}.
#'
#' @return A filtered data.frame, informative messages or an error.
#' @export

check_across_subset <- function(df, across, across.subset, relevel = TRUE, fdb.fn = "warning"){

  across_subset_input <- base::substitute(across.subset)

  is_value(x = across, mode = "character", skip.allow = TRUE, skip.val = NULL)

  if(base::is.null(across) | base::is.null(across.subset)){

    base::return(df)

  } else {

    # get valid groups
    if(base::is.factor(df[[across]])){

      all_groups <- base::levels(df[[across]])

    } else if(base::is.character(df[[across]])) {

      all_groups <- base::unique(df[[across]])

    } else {

      class_across <- base::class(df[[across]])

      msg <-
        glue::glue("Input for argument 'across' must refers to a variable of class {class_across}. Must be of type 'character' or 'factor'.")

      confuns::give_feedback(
        msg = msg,
        fdb.fn = "stop",
        verbose = TRUE
      )

    }

    # distinguish between groups to keep and groups to discard
    discard_groups <-
      stringr::str_subset(across.subset, pattern = "^-") %>%
      stringr::str_remove_all(pattern = "^-")

    keep_groups <-
      stringr::str_subset(across.subset, pattern = "^[^-]")

    # check for ambiguous input
    duplicated_groups <-
      base::intersect(keep_groups, discard_groups)

    if(base::length(duplicated_groups) >= 1){

      duplicated_groups <- stringr::str_c("(-)", duplicated_groups)

      msg <-
        glue::glue("Ambiguous values ('{duplicated_input}') in input for argument '{across_subset_input}'.",
                   duplicated_input = glue::glue_collapse(x = duplicated_groups, sep = "', ", last = "' and '"))

      give_feedback(
        fdb.fn = "stop",
        msg = msg
      )

    }

    across.subset <- c(keep_groups, discard_groups)

    # keep valid groups
      check_one_of(
        input = across.subset,
        against = all_groups,
        ref.input = base::as.character(glue::glue("input to subset '{across}'-groups"))
      )

    #if no error all are valid
    across.subset_valid <- across.subset

    # keep valid distinguished groups
    discard_groups <- discard_groups[discard_groups %in% across.subset_valid]

    # in case only -across.subset has been provided "refill" 'keep_groups'
    if(base::length(keep_groups) == 0){

      keep_groups <- all_groups

    }

    # discard what has been denoted with -
    keep_groups <- keep_groups[!keep_groups %in% discard_groups]

    # filter input data.frame
    df <- dplyr::filter(.data = df, !!rlang::sym(across) %in% {{keep_groups}})

    # relevel 'across' if desired
    if(base::isTRUE(relevel)){

      df[[across]] <-
        base::factor(x = df[[across]], levels = keep_groups)

    }

    base::return(df)

  }

}


#' @title Process a data.frame
#'
#' A wrapper around \code{check_df_variables}, \code{check_across_subset()}
#' and \code{dplyr::pivot_longer()}.
#'
#' @inherit argument_dummy params
#'
#' @return
#' @export

process_and_shift_df <- function(df,
                                 variables = NULL,
                                 valid.classes = NULL,
                                 across = NULL,
                                 across.subset = NULL,
                                 relevel = TRUE,
                                 keep = NULL,
                                 verbose = TRUE){

  df_checked <-
    check_df_variables(
      df = df,
      variables = variables,
      valid.classes = valid.classes,
      keep = c(across, keep),
      verbose = verbose
    ) %>%
    check_across_subset(
      df = .,
      across = across,
      across.subset = across.subset,
      relevel = relevel,
      fdb.fn = "warning"
    )

  unique_names <-
    base::names(df_checked) %>%
    base::unique()

  shift_cols <-
    purrr::keep(df_checked[, unique_names], .p = ~ is_any_of(.x, valid.classes = valid.classes)) %>%
    base::colnames()

  if(base::is.character(across)){

    shift_cols <- shift_cols[shift_cols != across]

  }

  df_shifted <-
    tidyr::pivot_longer(
      data = df_checked,
      cols = dplyr::all_of(shift_cols),
      names_to = "variables",
      values_to = "values"
    )

  base::return(df_shifted)

}
