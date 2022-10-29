

#' @title Select vector with tidyselect functions
#'
#' @description A wrapper around the tidyselect functions that allows to use them
#' not only on data.frames but on vectors as well.
#'
#' @param input A character vector or a factor.
#' @param lst A named list. (Unnamed elements are discarded.)
#' @param ... Additional selection helpers from the \code{tidyselect} package that match
#' names according to a given pattern.
#' @param out.fail Output if no content remains after tidyselection has been tried.
#'
#' @note \code{vselect()} selects by element. \code{lselect()} selects by element name.
#'
#' @return A subsetted version of the input.
#'
#' @seealso \code{starts_with()}, \code{ends_with()}, \code{contains()}, \code{matches()}
#'
#' @export
#'
#' @examples
#'
#'  library(datasets)
#'  library(confuns)
#'
#'  vec <- c("apple", "banana", "pineapple", "tomato")
#'
#'  vselect(vec, contains("apple") | ends_with("o"))
#'
#'  lst <- list(a = mtcars, ab = iris, ac = cars)
#'
#'  lselect(lst, starts_with("a") & !ends_with("c"))
#'
#'
#'
vselect <- function(input, ..., out.fail = base::character(0)){

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

      output <- out.fail

      # if FALSE then ncol == 0 because no tidyselection specified: return all variable names
    } else {

      output <- base::colnames(input_df)

    }

  } else {

    output <- base::colnames(selected_df)

  }


  if("factor" %in% original_class){

    output <- base::as.factor(output)

  }


  return(output)

}

#' @rdname vselect
#' @export
lselect <- function(lst, ..., out.fail = base::list()){

  lst <- keep_named(input = lst)

  input <- base::names(lst)

  input <- base::unique(input)

  n_cols <- base::length(input)

  input_df <-
    base::matrix(data = 1, ncol = n_cols) %>%
    base::as.data.frame() %>%
    magrittr::set_colnames(value = input)

  selected_df <- dplyr::select(input_df, ...)

  if(base::ncol(selected_df) == 0){

    # if TRUE then ncol == 0 because selection resulted in no vars
    selection_helpers_provided <-
      base::tryCatch({

        # leads to error if tidy selection specified
        list(...)

      }, error = function(error){

        TRUE

      })

    if(base::isTRUE(selection_helpers_provided)){

      lst_output <- out.fail

      # if FALSE then ncol == 0 because no tidy selection specified: return all variable names
    } else {

      output <- base::colnames(input_df)

      lst_output <- lst[output]

    }

  } else {

    output <- base::colnames(selected_df)

    lst_output <- lst[output]

  }





  return(lst_output)

}
