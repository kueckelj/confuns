

#' @title Rename vector in tidyverse style
#'
#' @param input
#' @param ...
#'
#' @return
#' @export
#'
vrename <- function(input, ..., safely = TRUE){

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

  base::return(renamed_input)


}

#' @rdname vrename
#' @export
vrename_with <- function(input, ...){

  base::matrix(nrow = 1, ncol = base::length(input)) %>%
    base::as.data.frame() %>%
    magrittr::set_colnames(value = input) %>%
    dplyr::rename_with(...) %>%
    base::colnames()

}


#' @title Rename matrix in tidyverse style
#'
#' @param mtr
#' @param dims
#' @param ...
#'
#' @return
#' @export
#'
mrename <- function(mtr, dims = c(1,2), ...){

  if(1 %in% dims){

    base::rownames(mtr) <-
      vrename(input = base::rownames(mtr), ...)

  }

  if(2 %in% dims){

    base::colnames(mtr) <-
      vrename(input = base::colnames(mtr), ...)

  }

  base::return(mtr)


}

#' @rdname mrename
#' @export
mrename_with <- function(mtr, dims = c(1,2), ...){

  if(1 %in% dims){

    base::rownames(mtr) <-
      vrename_with(input = base::rownames(mtr), ...)

  }

  if(2 %in% dims){

    base::colnames(mtr) <-
      vrename_with(input = base::colnames(mtr), ...)

  }

  base::return(mtr)

}





#' @title Rename numeric variables of corr_conv objects
#'
#' @param corr.obj
#' @param ...
#'
#' @return
#' @export
#'
rename_numeric_vars <- function(corr.obj, ..., rename.data = TRUE){

  # rename @variables_num
  corr.obj@variables_num <-
    vrename(input = corr.obj@variables_num, ...)

  # rename @data, option to skip in case of integration
  # in other S4 objects (data is added via extracting functions)
  if(base::isTRUE(rename.data)){

    corr.obj@data <-
      base::as.data.frame(corr.obj@data) %>%
      tibble::rownames_to_column(var = "key") %>%
      rename_safely(df = ., ...) %>%
      tibble::column_to_rownames(var = "key") %>%
      base::as.matrix()

  }


  # rename @results_all
  corr.obj@results_all <-
    purrr::map(.x = corr.obj@results_all, # iterate over methods (pearson, spearman)
               .f = function(input_list){ # list of three slots: r, n, P

                 output_list <-
                   purrr::map_at(
                     .x = input_list,
                     .at = c("r", "P"),
                     .f = mrename,
                     ...
                   )

                 base::return(output_list)

               })

  # rename @results_across
  corr.obj@results_across <-
    purrr::map(.x = corr.obj@results_across, # iterate over methods, names => all corr methods
               .f = function(method_list){

                 method_list_out <-
                   purrr::map(.x = method_list, # iterate over grouping variables, names => all grouping variables
                              .f = function(grouping_list){

                                grouping_list_out <-
                                  purrr::map(.x = grouping_list, # iterate over all groups, names => all groups of the grouping variable
                                             .f = function(group_list){

                                               group_list_out <-
                                                 purrr::map_at(.x = group_list, # iterate over the three corr slots r, n, P
                                                               .at = c("r", "P"), # only matric slots
                                                               .f = mrename,
                                                               ...)

                                               base::return(group_list_out)

                                             })

                                base::return(grouping_list_out)

                              })

                 base::return(method_list_out)

               })

  return(corr.obj)

}

#' @rdname rename_numeric_vars
#' @export
rename_numeric_vars_with <- function(corr.obj, ..., rename.data = TRUE){

  # rename @variables_num
  corr.obj@variables_num <-
    vrename_with(input = corr.obj@variables_num, ...)

  if(base::isTRUE(rename.data)){

    # rename @data
    corr.obj@data <-
      mrename_with(mtr = corr.obj@data, dims = 2, ...)

  }


  # rename @results_all
  corr.obj@results_all <-
    purrr::map(.x = corr.obj@results_all, # iterate over methods (pearson, spearman)
               .f = function(input_list){ # list of three slots: r, n, P

                 output_list <-
                   purrr::map_at(
                     .x = input_list,
                     .at = c("r", "P"),
                     .f = mrename_with,
                     ...
                   )

                 base::return(output_list)

               })

  # rename @results_across
  corr.obj@results_across <-
    purrr::map(.x = corr.obj@results_across, # iterate over methods
               .f = function(method_list){ # names(method_list) => all corr methods

                 method_list_out <-
                   purrr::map(.x = method_list, # iterate over grouping variables
                              .f = function(grouping_list){ # names(grouping_list) => all grouping variables

                                grouping_list_out <-
                                  purrr::map(.x = grouping_list, # iterate over all groups
                                             .f = function(group_list){ # names(group_list) => all groups of the grouping variable

                                               group_list_out <-
                                                 purrr::map_at(.x = group_list, # iterate over the three corr slots r, n, P
                                                               .at = c("r", "P"), # only matric slots
                                                               .f = mrename_with,
                                                               ...)

                                               base::return(group_list_out)

                                             })

                                base::return(grouping_list_out)

                              })

                 base::return(method_list_out)

               })

  return(corr.obj)

}



#' @title Save wrapper around dplyr::rename()
#'
#' @param df
#' @param ...
#'
#' @return
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

    base::return(df)

  } else {

    base::return(df_renamed)

  }

}

