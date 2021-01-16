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
#' denoted in \code{across} contains.
#' @param relevel Logical value. If set to TRUE the input of \code{across.subset}
#' determines the new order in which the results are displayed.
#'
#' @return A filtered data.frame, informative messages or an error.
#' @export
#'

check_across_subset <- function(df, across, across.subset, relevel = TRUE, fdb.fn = "warning"){

  if(base::is.null(across.subset)){

    base::return(df)

  } else {

    #df[[across]] <- confuns::unfactor(df[[across]])

    if(base::is.factor(df[[across]])){

      against_input <- base::levels(df[[across]])

    } else {

      against_input <- base::unique(df[[across]])

    }

    across.subset <- check_vector(input = across.subset,
                                  against = against_input,
                                  fdb.fn = fdb.fn,
                                  ref.input = glue::glue("input to subset '{across}'-groups"),
                                  ref.against = glue::glue("valid options"))

    df <- dplyr::filter(.data = df, !!rlang::sym(across) %in% {{across.subset}})

    if(base::is.factor(df[[across]]) & base::isTRUE(relevel)){

      df[[across]] <-
        base::factor(x = df[[across]], levels = across.subset)

    }

    base::return(df)

  }

}
