#' Subset the across-variables
#'
#' @description Checks across and across.subset input and if at least one
#' of the \code{across.subset} values exists if the letter is not NULL. It
#' then filters the input data.frame accordingly.
#'
#' @param df A data.frame that contains the grouping variable specified in \code{across}.
#' @param across Character value. Denotes the discrete variable in the data.frame
#' across which the variables of interest are to be analyzed or displayed.
#' @param across.subset Character vector. The groups of interest that the grouping variable
#' denoted in \code{across} contains. Groups prefixed with an \emph{'-'} are discarded.
#' @param relevel Logical value. If set to TRUE the input of \code{across.subset}
#' determines the new order in which the results are displayed.
#'
#' @return A filtered data.frame, informative messages or an error.
#' @export

check_across_subset <- function(df, across, across.subset, relevel = TRUE, fdb.fn = "warning"){

  across_subset_input <- base::substitute(across.subset)

  if(base::is.null(across.subset)){

    base::return(df)

  } else {

    # get valid groups
    if(base::is.factor(df[[across]])){

      all_groups <- base::levels(df[[across]])

    } else {

      all_groups <- base::unique(df[[across]])

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
    across.subset_valid <-
      check_vector(input = across.subset,
                   against = all_groups,
                   fdb.fn = fdb.fn,
                   ref.input = glue::glue("input to subset '{across}'-groups"),
                   ref.against = glue::glue("valid options")
                   )

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
