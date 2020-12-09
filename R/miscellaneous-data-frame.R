#' Subset the across-variables
#'
#' @description Checks across and across.subset input and if at least one
#' of the across.subset values exists filters the data.frame accordingly.
#'
#' @param df A data.frame that contains the variable specified in \code{across}.
#' @param across Character value. Denotes the discrete variable in the data.frame
#' across which the variables of interest are to be analyzed or displayed. Valid input
#' options can be obtained via \code{getAcrossOptions()}.
#' @param across.subset Character vector. The groups of interest that the \code{across}-
#' variable contains. }.
#'
#' @return A filtered data.frame, informative messages or an error.
#' @export
#'

check_across_subset <- function(df, across, across.subset){

  if(base::is.null(across.subset)){

    base::return(df)

  } else {

    #df[[across]] <- confuns::unfactor(df[[across]])

    if(base::is.factor(df[[across]])){

      against_input <- base::levels(df[[across]])

    } else {

      against_input <- base::unique(df[[across]])

    }

    df <- dplyr::filter(.data = df, !!rlang::sym(across) %in% {{across.subset}})

    if(base::is.factor(df[[across]])){

      df[[across]] <-
        base::factor(x = df[[across]], levels = across.subset)

    }

    base::return(df)

  }

}
