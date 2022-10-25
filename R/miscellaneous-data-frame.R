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
  is_vec(x = across.subset, mode = "character", skip.allow = TRUE, skip.val = NULL)

  # 1. Return option (across is NULL - original df) -------------------------

  if(base::is.null(across)){

    return(df)

  } else {

    # get valid groups and make sure that across is of type character or factor
    if(base::is.factor(df[[across]])){

      all_groups <- base::levels(df[[across]])

    } else if(base::is.character(df[[across]])) {

      all_groups <- base::unique(df[[across]])

    } else {

      class_across <- base::class(df[[across]])

      msg <-
        glue::glue("Input '{across}' for argument 'across' refers to a variable of class {class_across}. Must be of class character or factor.")

      confuns::give_feedback(
        msg = msg,
        fdb.fn = "stop",
        verbose = TRUE,
        with.time = FALSE
      )

    }


    # 2. Return option (across.subset is NULL - original df) ------------------

    if(base::is.null(across.subset)){

      return(df)

    } else if(base::is.character(across.subset)){

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


      # 3. Return option (across.subset is character - filtered df) ---------

      return(df)

    }

  }



}

#' @rdname check_across_subset
#' @export
check_across_subset2 <- function(df, across, across.subset, relevel){

  if(base::length(across) == 2){

    if(base::length(relevel == 1)){

      relevel <- base::rep(relevel, 2)

    }

    if(!base::is.null(across.subset) & !is_list(input = across.subset)){

      msg <- "If input for argument 'across' is of length two the input for argument 'across.subset' must be a named list or NULL."

      give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)

    }

    df <- check_across_subset(
      df = df, across = across[1],
      across.subset = across.subset[[across[1]]],
      relevel = relevel[1]
    )

    df <-
      check_across_subset(
        df = df,
        across = across[2],
        across.subset = across.subset[[across[2]]],
        relevel = relevel[2]
      )

  } else {

    df <-
      check_across_subset(
        df = df,
        across = across,
        across.subset = across.subset,
        relevel = relevel[1]
      )

  }

  return(df)

}

#' @title Process a data.frame
#'
#' A wrapper around \code{check_df_variables}, \code{check_across_subset()}
#' and \code{dplyr::pivot_longer()}.
#'
#' @inherit argument_dummy params
#' @inherit check_df_variables params
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
                                 ref_df = NULL,
                                 verbose = TRUE){


  # select and filter
  keep <- base::unique(c(across, keep))

  variables <- base::unique(variables)

  df_checked <-
    check_df_variables(
      df = df,
      variables = variables,
      valid.classes = valid.classes,
      keep = keep,
      ref_df = ref_df,
      verbose = verbose
    ) %>%
    check_across_subset(
      across = across,
      across.subset = across.subset,
      relevel = relevel,
      fdb.fn = "warning"
    )


  # extract the variables that are to be shifted
  valid_variables <- base::colnames(df_checked)

  if(base::is.character(keep) && base::length(keep) >= 1){

    shift_cols <- valid_variables[!valid_variables %in% keep]

  } else {

    shift_cols <- valid_variables

  }

  # shift the data.frame (unorder factor to prevent errors)
  df_shifted <-
    df_checked %>%
    dplyr::mutate_if(.predicate = base::is.ordered,
                     .funs = base::factor,
                     ordered = FALSE) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(shift_cols),
      names_to = "variables",
      values_to = "values"
    )

  # relevel variables if set to true
  if(base::isTRUE(relevel) && base::is.character(variables)){

    variables <- variables[!stringr::str_detect(variables, pattern = "^-")]

    if(base::length(variables) >= 2){

      variable_levels <- variables[variables %in% shift_cols]

      df_shifted$variables <- base::factor(df_shifted$variables, levels = variable_levels)

    }

  }

  df_shifted[["variables"]] <-
    base::factor(
      x = df_shifted[["variables"]],
      levels = variables
      )

  return(df_shifted)

}
