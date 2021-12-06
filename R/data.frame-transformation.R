continuous_ggplot2_aes <- c("x", "y", "alpha", "color", "fill", "shape", "size")

transformation_fns <-
  list(
    "exp" = base::exp,
    "log" = base::log,
    "log2" = base::log2,
    "log10" = base::log10,
    "sqrt" = base::sqrt
  )


#' @export
transform_df <- function(df, transform.with, sep = "_"){

  aes_pattern <- stringr::str_c(sep, "by")

  calling_env <- rlang::caller_env(n = 1)

  calling_fn <- rlang::caller_fn(n = 1)

  aes_args <-
    rlang::fn_fmls_names(fn = calling_fn) %>%
    vselect(
      contains(continuous_ggplot2_aes) & ends_with(aes_pattern),
      any_of(x = c("x", "y"))
    )

  spec_aes_args <-
    purrr::map(
      .x = aes_args,
      .f = function(arg){

        base::parse(text = arg) %>%
          base::eval(envir = calling_env)

      }) %>%
    purrr::set_names(nm = aes_args) %>%
    purrr::keep(.p = base::is.character)

  spec_aes <-
    stringr::str_remove(
      string = base::names(spec_aes_args),
      pattern = aes_pattern
    )

  aes_vars <- purrr::flatten_chr(.x = spec_aes_args)

  if(base::is.character(transform.with)){

    check_one_of(
      input = transform.with,
      against = base::names(transformation_fns)
    )

    for(tf in transform.with){

      df <-
        dplyr::mutate(
          .data = df,
          dplyr::across(
            .cols = dplyr::all_of(x = aes_vars),
            .fns = transformation_fns[[tf]],
            .names = NULL
          )
        )

    }

  } else if(is_list(transform.with) & !purrr::is_empty(transform.with)){

    names_tw <- base::names(transform.with)

    check_one_of(
      input = names_tw,
      against = spec_aes,
      ref.input = "aesthetics to transform",
      fdb.opt = 2,
      ref.opt.2 = "aesthetics that are specified"
    )

    for(name_tw in names_tw){

      if(!name_tw %in% c("x", "y")){

        name_pattern <- stringr::str_c(name_tw, "by", sep = sep)

      } else {

        name_pattern <- name_tw

      }

      var_to_transform <- spec_aes_args[[name_pattern]]

      transform_info <- transform.with[[name_tw]]

      if(base::is.character(transform_info)){

        for(tf in transform_info){

          df <-
            dplyr::mutate(
              .data = df,
              dplyr::across(
                .cols = {{var_to_transform}},
                .fns = transformation_fns[[tf]],
                .names = NULL
              )
            )

        }

      } else if(is_list(transform_info)){

        for(tf in transform_info){

          if(base::is.function(tf)){

            df <-
              dplyr::mutate(
                .data = df,
                dplyr::across(
                  .cols = {{var_to_transform}},
                  .fns = tf,
                  .names = NULL
                )
              )

          } else if(base::is.character(tf)){

            df <-
              dplyr::mutate(
                .data = df,
                dplyr::across(
                  .cols = {{var_to_transform}},
                  .fns = transformation_fns[[tf]],
                  .names = NULL
                )
              )

          }

        }

      }

    }

  }

  return(df)

}
