


# s4 ----------------------------------------------------------------------

corr_conv <- methods::setClass(Class = "corr_conv",
                               slots = c(
                                 data = "matrix",
                                 default = "list",
                                 meta = "data.frame",
                                 results_all = "list",
                                 results_across = "list",
                                 variables_num = "character",
                                 variables_discrete = "character"
                               ))




# r-objects ---------------------------------------------------------------

valid_methods_corr <- c("pearson", "spearman")



# input check -------------------------------------------------------------

#' Title
#'
#' @param method.corr
#'
#' @return
#' @export
#'
check_method_corr <- function(method.corr){

  check_one_of(input = method.corr, against = valid_methods_corr)

}



# initiation  -------------------------------------------------------------

#' Title
#'
#' @param corr.data
#' @param default.method
#' @param default.dir
#'
#' @return
#' @export
initiate_corr_obj <- function(corr.data,
                              default.method = c("pearson", "spearman"),
                              default.dir = "conv-corr-obj.RDS"){


  corr.obj <- methods::new(Class = "corr_conv")

  corr.data <- base::as.data.frame(corr.data)

  # create key
  key_var <- stringr::str_c("ID", 1:base::nrow(corr.data), sep = "_")

  # set variables
  corr.obj@variables_num <-
    dplyr::select_if(corr.data, .predicate = base::is.numeric) %>%
    base::colnames()

  corr.obj@variables_discrete <-
    purrr::discard(.x = corr.data, .p = base::is.numeric) %>%
    base::names()

  # set data
  corr.obj@data <-
    base::as.matrix(corr.data[, corr.obj@variables_num]) %>%
    magrittr::set_rownames(value = key_var)

  # set meta
  corr.obj@meta <-
    dplyr::select(corr.data, dplyr::all_of(x = corr.obj@variables_discrete)) %>%
    purrr::map_df(.f = function(discr_var){

      if(!base::is.factor(discr_var)){

        discr_var <- base::as.factor(discr_var)

      }

      base::return(discr_var)

    }) %>%
    dplyr::mutate(key = {{key_var}} ) %>%
    dplyr::select(key, dplyr::everything())

  # set default
  corr.obj <-
    set_corr_default(corr.obj = corr.obj,
                     method.corr = default.method,
                     directory = default.dir)


  base::return(corr.obj)

}



# set ---------------------------------------------------------------------

#' Title
#'
#' @param corr.obj
#' @param method.corr
#' @param directory
#'
#' @return
#' @export
#'
set_corr_default <- function(corr.obj, method.corr = NA, directory = NA){

  if(!base::all(is.na(method.corr))){

    check_method_corr(method.corr)

    corr.obj@default[["method.corr"]] <- method.corr[1]
    corr.obj@default[["methods.corr"]] <- method.corr

  }


  if(!base::is.na(directory)){

    corr.obj@default$directory <- directory

  }

  base::return(corr.obj)

}


# computation -------------------------------------------------------------

#' Title
#'
#' @param corr.obj
#' @param methods.corr
#'
#' @return
#' @export
#'
correlate_all <- function(corr.obj, methods.corr = NULL){

  assign_corr_default(corr.obj)

  corr.obj@results_all <-
    purrr::map(.x = methods.corr, .f = ~ Hmisc::rcorr(x = corr.obj@data, type = .x)) %>%
    purrr::set_names(nm = methods.corr)

  base::return(corr.obj)

}


#' Title
#'
#' @param corr.obj
#' @param across
#' @param methods.corr
#' @param verbose
#'
#' @return
#' @export
#'
correlate_across <- function(corr.obj, across = NULL, methods.corr = NULL, verbose = TRUE){

  assign_corr_default(corr.obj)

  if(base::is.null(across)){

    across <- corr.obj@variables_discrete

  } else {

    check_one_of(input = across, against = across)

  }

  across_present <- base::names(corr.obj@results_across)

  df <- get_corr_data(corr.obj, keep.key = TRUE)

  output_list <-
    purrr::map(.x = methods.corr, .f = function(method){

      msg <- glue::glue("Using correlation method '{method}'.")

      give_feedback(msg = msg, verbose = verbose)

      if(base::is.character(across_present)){

        across_new <- discard_if(input = across, one_of = across_present, verbose = verbose)

      } else if(base::is.null(across_present)){

        across_new <- across

      }

      if(base::is.character(across_new)){

        results <-
          purrr::map(.x = across_new, .f = function(.across){

            across_levels <- base::levels(dplyr::pull(df, var = {{.across}}))

            .results <-
              purrr::map(
                .x = across_levels,
                .f = purrr::safely(function(level){

                mtr_filtered <-
                  dplyr::filter(df, !!rlang::sym(.across) == {{level}}) %>%
                  dplyr::select_if(.predicate = base::is.numeric) %>%
                  base::as.matrix()

                res <- Hmisc::rcorr(x = mtr_filtered, type = method)

                res$n <- base::nrow(mtr_filtered)

                base::return(res)

              })) %>%
              purrr::set_names(nm = across_levels)

            .results_filtered <-
              purrr::keep(.x = .results, .p = ~ base::is.null(.x[["error"]])) %>%
              purrr::map(.x = ., .f = ~ .x[["result"]])

            errors <-
              purrr::discard(.x = .results, .p = ~ base::is.null(.x[["error"]])) %>%
              purrr::map(.x = ., .f = ~ .x[["error"]][["message"]]) %>%
              purrr::flatten()

            base::return(
              c(.results_filtered, errors)
              )

          }) %>%
          purrr::set_names(nm = across_new)

      } else if(base::is.null(across_new)){

        results <- NULL

      }

      base::return(results)

    }) %>%
    purrr::set_names(nm = methods.corr) %>%
    purrr::discard(.p = base::is.null)


  if(base::isTRUE(verbose)){

    errors <-
      purrr::map(
        .x = output_list,
        .f = ~ purrr::map(.x = .x, .f = ~ purrr::keep(.x = .x, .p = base::is.character)) %>%
          purrr::discard(.x = ., .p = ~ base::length(.x) == 0)
      )

    print(errors)

  }


  for(method in methods.corr){

    for(across_val in across){

      corr.obj@results_across[[method]][[across_val]] <-
        c(output_list[[method]][[across_val]],
          corr.obj@results_across[[method]][[across_val]])

    }

  }

  base::return(corr.obj)

}


# extraction --------------------------------------------------------------


#' Title
#'
#' @param corr.obj
#' @param keep.key
#' @param return
#'
#' @return
#' @export
#'
get_corr_data <- function(corr.obj, keep.key = FALSE, return = "tibble"){

  df <-
    dplyr::left_join(
      x = base::as.data.frame(corr.obj@data) %>% tibble::rownames_to_column(var = "key"),
      y = corr.obj@meta,
      by = "key"
    )

  if(base::isFALSE(keep.key)){

    df <- dplyr::select(df, -key)

  }

  if(return == "tibble"){

    df <- tibble::as_tibble(df)

  }



  base::return(df)

}


# plotting ----------------------------------------------------------------






# miscellaneous -----------------------------------------------------------

#' Title
#'
#' @param corr.obj
#'
#' @return
#' @export
#'
assign_corr_default <- function(corr.obj){

  ce <- rlang::caller_env()

  default_args <- base::names(corr.obj@default)

  cfn <- rlang::caller_fn()

  # get arguments froms calling function
  cargs <- rlang::fn_fmls_names(fn = cfn)

  # keep arguments from calling function
  default_args <- cargs[cargs %in% default_args]

  # assign default argument values if input was set to NULL
  for(arg in default_args){

    arg_value <-
      base::parse(text = arg) %>%
      base::eval(envir = ce)

    if(base::is.null(arg_value)){

      arg_value <- corr.obj@default[[arg]]

      if(!base::is.null(arg_value)){

        base::assign(
          x = arg,
          value = arg_value,
          envir = ce
        )

      }

    }

  }

}


