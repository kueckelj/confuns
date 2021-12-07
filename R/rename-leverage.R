

#' @export
vredefine <- function(input, ..., safely = TRUE){

  if(base::isTRUE(safely)){

    redefined_input <-
      base::tryCatch({

        base::matrix(nrow = 1, ncol = base::length(input)) %>%
          base::as.data.frame() %>%
          magrittr::set_colnames(value = input) %>%
          dplyr::rename(...) %>%
          base::colnames()

      }, error = function(error){

        NA

      })

    if(!base::is.character(redefined_input)){

      redefined_input <- input

    }

  } else {

    redefined_input <-
      base::matrix(nrow = 1, ncol = base::length(input)) %>%
      base::as.data.frame() %>%
      magrittr::set_colnames(value = input) %>%
      dplyr::rename(...) %>%
      base::colnames()
  }

  return(redefined_input)

}

#' @export
vredefine_with <- function(input, ...){

  base::matrix(nrow = 1, ncol = base::length(input)) %>%
    base::as.data.frame() %>%
    magrittr::set_colnames(value = input) %>%
    dplyr::rename_with(...) %>%
    base::colnames()

}

#' @export
vrename <- function(input, ..., safely = TRUE){

  warning("Change vrename() to vredefine()")

  if(base::isTRUE(safely)){

    renamed_input <-
      base::tryCatch({

        base::matrix(nrow = 1, ncol = base::length(input)) %>%
          base::as.data.frame() %>%
          magrittr::set_colnames(value = input) %>%
          dplyr::rename(...) %>%
          base::colnames()

      }, error = function(error){

        NA

      })

    if(!base::is.character(renamed_input)){

      renamed_input <- input

    }

  } else {

    renamed_input <-
      base::matrix(nrow = 1, ncol = base::length(input)) %>%
      base::as.data.frame() %>%
      magrittr::set_colnames(value = input) %>%
      dplyr::rename(...) %>%
      base::colnames()
  }

  return(renamed_input)


}

#' @export
vrename_with <- function(input, ...){

  warning("Change vrename_with() to vredefine_with().")

  base::matrix(nrow = 1, ncol = base::length(input)) %>%
    base::as.data.frame() %>%
    magrittr::set_colnames(value = input) %>%
    dplyr::rename_with(...) %>%
    base::colnames()

}


#' @export
#'
lrename <- function(lst, ..., safely = TRUE){

  input <- base::names(lst)

  if(base::isTRUE(safely)){

    renamed_input <-
      base::tryCatch({

        base::matrix(nrow = 1, ncol = base::length(input)) %>%
          base::as.data.frame() %>%
          magrittr::set_colnames(value = input) %>%
          dplyr::rename(...) %>%
          base::colnames()

      }, error = function(error){

        NA

      })

    if(!base::is.character(renamed_input)){

      renamed_input <- input

    }

  } else {

    renamed_input <-
      base::matrix(nrow = 1, ncol = base::length(input)) %>%
      base::as.data.frame() %>%
      magrittr::set_colnames(value = input) %>%
      dplyr::rename(...) %>%
      base::colnames()
  }

  renamed_lst <- purrr::set_names(lst, renamed_input)

  return(renamed_lst)

}

#' @export
lrename_with <- function(lst, ...){

  input <- base::names(lst)

  input_renamed <-
    base::matrix(nrow = 1, ncol = base::length(input)) %>%
    base::as.data.frame() %>%
    magrittr::set_colnames(value = input) %>%
    dplyr::rename_with(...) %>%
    base::colnames()

  lst_renamed <- purrr::set_names(lst, nm = input_renamed)

  return(lst_renamed)

}


#' @title Rename matrix in tidyverse style
#' @export
#'
mrename <- function(mtr, dims = c(1,2), ...){

  if(1 %in% dims){

    base::rownames(mtr) <-
      vredefine(input = base::rownames(mtr), ...)

  }

  if(2 %in% dims){

    base::colnames(mtr) <-
      vredefine(input = base::colnames(mtr), ...)

  }

  return(mtr)


}

#' @export
mrename_with <- function(mtr, dims = c(1,2), ...){

  if(1 %in% dims){

    base::rownames(mtr) <-
      vredefine_with(input = base::rownames(mtr), ...)

  }

  if(2 %in% dims){

    base::colnames(mtr) <-
      vredefine_with(input = base::colnames(mtr), ...)

  }

  return(mtr)

}


#' @title Save wrapper around dplyr::rename()
#'
#' @export
#'
rename_safely <- function(df, ...){

  df_renamed <-
    base::tryCatch({

      dplyr::rename(df, ...)

    },error = function(error){

      NA

    })

  if(!base::is.data.frame(df_renamed)){

    return(df)

  } else {

    return(df_renamed)

  }

}

