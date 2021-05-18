

#' @title Select vector with tidyselect functions
#'
#' @description A wrapper around the tidyselect functions that allows to use them
#' not only on data.frames but on vectors as well.
#'
#' @param input A character vector or a factor.
#' @param ... Additional selection helpers from the \code{tidyselect} package that match
#' variable names according to a given pattern.
#'
#' @return A subsetted version of the input.
#'
#' @seealso \code{starts_with()}, \code{ends_with()}, \code{contains()}, \code{matches()}
#'
#' @export
#'

vector_select <- function(input, ...){

  original_input <- input

  original_class <- base::class(input)

  if(!base::is.data.frame(input)){

    if(base::is.character(input)){

      input <- base::unique(input)

      n_cols <- base::length(input)

    } else if(base::is.factor(input)){

      input <- base::levels(input)

      n_cols <- base::length(input)

    }

    input_df <-
      base::matrix(data = 1, ncol = n_cols) %>%
      base::as.data.frame() %>%
      magrittr::set_colnames(value = input)

  }

  selected_df <- dplyr::select(input_df, ...)

  if(base::ncol(selected_df) == 0){

    # if TRUE then ncol == 0 because selection resulted in no vars
    selection_helpers_provided <-
      base::tryCatch({

        # leads to error if tidyselection specified
        list(...)

      }, error = function(error){

        TRUE

      })

    if(base::isTRUE(selection_helpers_provided)){

      base::stop("Tidyselect input resulted in no output.")

      # if FALSE then ncol == 0 because no tidyselection specified: return all variable names
    } else {

      selected_df <- input_df

    }

  }

  output <- base::colnames(selected_df)

  if("factor" %in% original_class){

    output <- base::as.factor(output)

  }


  base::return(output)

}
