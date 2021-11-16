


# s4 ----------------------------------------------------------------------

corr_conv <- methods::setClass(Class = "corr_conv",
                               slots = c(
                                 data = "matrix",
                                 default = "list",
                                 meta = "data.frame",
                                 results_all = "list",
                                 results_across = "list",
                                 variables_discrete = "character",
                                 variables_num = "character"
                               ))




# r-objects ---------------------------------------------------------------

valid_methods_corr <- c("pearson", "spearman")

valid_plot_types_corr <- c("lower", "upper", "complete")

valid_shapes_corr <- c("circle","rect", "tile")

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



#' Title
#'
#' @param input
#' @param method.corr
#' @param across
#' @param fdb.fn
#'
#' @return
#' @export
#'
check_corr_availability <- function(input, method.corr, across = NULL, fdb.fn = "stop"){

  if(!shiny::isTruthy(input)){

    if(!base::is.null(across)){

      across_ref <- glue::glue(" across variable '{across}'")

    } else {

      across_ref <- ""
    }

    msg <- glue::glue("Could not find correlation results for method '{method.corr}'{across_ref}.")

    give_feedback(msg = msg, fdb.fn = fdb.fn)

    input <- NULL

  }

  base::invisible(input)

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
initiate_corr_object <- function(corr.data,
                                 key.name = NULL,
                                 default.method = c("pearson", "spearman"),
                                 default.dir = "conv-corr-obj.RDS"){


  corr.obj <- methods::new(Class = "corr_conv")

  corr.data <- base::as.data.frame(corr.data)

  # create key
  if(base::is.null(key.name)){

    key_var <- stringr::str_c("ID", 1:base::nrow(corr.data), sep = "_")

  } else {

    key_var <- base::as.character(corr.data[,key.name])

    corr.data[,key.name] <- NULL

    n_distinct_vals <- dplyr::n_distinct(key_var)

    base::stopifnot(n_distinct_vals == base::nrow(corr.data))

  }


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
    dplyr::mutate_if(corr.data, .predicate = base::is.character, .funs = base::as.factor)%>%
    dplyr::mutate(key = {{key_var}} ) %>%
    dplyr::select(key, where(base::is.factor))

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
correlate_across <- function(corr.obj, across = NULL, methods.corr = NULL, print.errors = FALSE, verbose = TRUE){

  assign_corr_default(corr.obj)

  if(base::is.null(across)){

    across <- corr.obj@variables_discrete

  } else {

    check_one_of(input = across, against = across)

  }

  df <- get_corr_data(corr.obj, keep.key = TRUE)

  output_list <-
    purrr::map(.x = methods.corr, .f = function(method){

      msg <- glue::glue("Using correlation method '{method}'.")

      give_feedback(msg = msg, verbose = verbose)

      across_present <- base::names(corr.obj@results_across[[method]])

      if(base::is.character(across_present)){

        across_new <- discard_if(input = across, one_of = across_present, verbose = verbose)

      } else if(base::is.null(across_present)){

        across_new <- across

      }

      if(base::is.character(across_new)){

        pb <- create_progress_bar(total = base::length(across_new))

        results <-
          purrr::map(.x = across_new, .f = function(.across){

            if(base::isTRUE(verbose)){ pb$tick() }

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


  if(base::isTRUE(print.errors)){

    errors <-
      purrr::map(
        .x = output_list,
        .f = ~ purrr::map(.x = .x, .f = ~ purrr::keep(.x = .x, .p = base::is.character)) %>%
          purrr::discard(.x = ., .p = ~ base::length(.x) == 0)
      )

    base::print(errors)

  }


  for(method in methods.corr){

    for(across_val in across){

      output_res <-
        purrr::keep(.x = output_list[[method]][[across_val]],
                    .p = ~ base::is.list(.x) & is_named(input = .x))

      corr.obj@results_across[[method]][[across_val]] <-
        c(output_res, corr.obj@results_across[[method]][[across_val]])

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


#' Title
#'
#' @param corr.obj
#' @param method.corr
#' @param across
#' @param fdb.fn
#'
#' @return
#' @export
#'
get_corr_results <- function(corr.obj, method.corr = NULL, across = NULL, across.subset = NULL, fdb.fn = "stop"){

  assign_corr_default(corr.obj)

  if(base::is.null(across)){

    corr_mtr <- corr.obj@results_all[[method.corr]]

  } else {

    is_value(across, mode = "character")

    corr_mtr <- corr.obj@results_across[[method.corr]][[across]]

  }

  corr_mtr <-
    check_corr_availability(
      input = corr_mtr,
      across = across,
      method.corr = method.corr,
      fdb.fn = fdb.fn
    )

  base::return(corr_mtr)

}


# plotting ----------------------------------------------------------------

#' Title
#'
#' @param corr.input
#' @param method.corr
#' @param plot.type
#' @param display.diagonal
#' @param p.mtr
#' @param signif.level
#' @param clr.low
#' @param clr.high
#' @param shape
#' @param display.with.size
#' @param size.max
#' @param display.values
#' @param values.alpha
#' @param values.clr
#' @param values.digits
#' @param values.size
#' @param draw.grid
#' @param grid.clr
#' @param grid.size
#'
#' @return
#' @export
#'
plot_corrplot <- function(corr.input,
                          method.corr = NULL,
                          variables.subset = NULL,
                          plot.type = "lower",
                          display.diagonal = TRUE,
                          p.mtr = NULL,
                          signif.level = NULL,
                          clr.low = "darkred",
                          clr.high = "steelblue",
                          shape = "tile",
                          display.with.size = TRUE,
                          size.max = 15,
                          display.values = TRUE,
                          values.alpha = 0.9,
                          values.clr = "black",
                          values.digits = 2,
                          values.size = 4,
                          draw.grid = TRUE,
                          grid.clr = "black",
                          grid.size = 0.5){

  if(base::class(corr.input) == "corr_conv"){

    corr.res<- get_corr_results(corr.input, across = NULL, method.corr = method.corr)

    corr.mtr <- corr.res$r
    p.mtr <- corr.res$P

    if(plot.type == "lower"){

      corr.mtr[base::upper.tri(corr.mtr, diag = !display.diagonal)] <- NA
      p.mtr[base::upper.tri(p.mtr, diag = !display.diagonal)] <- NA

    } else if(plot.type == "upper"){

      corr.mtr[base::lower.tri(corr.mtr, diag = !display.diagonal)] <- NA
      p.mtr[base::lower.tri(p.mtr, diag = !display.diagonal)] <- NA

    }

    if(!base::is.numeric(signif.level)){ p.mtr <- NULL}


  } else if("matrix" %in% base::class(corr.input)){

    corr.mtr <- corr.input

  }

  corr.mtr <- subset_mtr(mtr = corr.mtr, dims = c(1,2), variables.subset = variables.subset)

  p.mtr <- subset_mtr(mtr = p.mtr, dims = c(1,2), variables.subset = variables.subset)

  # reshape correlation input
  df_corr <-
    reshape2::melt(data = base::as.matrix(corr.mtr),
                   varnames = c("var1", "var2"),
                   value.name = "cor") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(cor_abs = base::abs(x = cor))

  # make sure that the grid is displayed completely
  # only discards rows that are na due to upper.tri()/lower.tri()
  df_grid <- dplyr::filter(df_corr, !base::is.na(cor))
  df_values <- dplyr::filter(df_corr, !base::is.na(cor))

  # add significance information
  if(base::is.matrix(p.mtr)){

    is_value(x = signif.level, mode = "numeric")

    df_pval <-
      reshape2::melt(data = base::as.matrix(p.mtr),
                     varnames = c("var1", "var2"),
                     value.name = "pval") %>%
      tibble::as_tibble()

    df_corr <-
      dplyr::left_join(x = df_corr, y = df_pval, by = c("var1", "var2")) %>%
      dplyr::mutate(
        significance = dplyr::case_when(
          base::is.na(pval) ~ "na",
          pval < signif.level ~ "Significant",
          pval >= signif.level ~ "Insignificant"
        )
      )

    df_corr$cor[df_corr$significance == "Insignificant"] <- NA

    df_corr_insignificant <-
      dplyr::filter(df_corr, significance == "Insignificant")

    insignificance_add_on <-
      ggplot2::geom_point(shape = 4, size = size.max, data = df_corr_insignificant, color = "black")

  } else {

    insignificance_add_on <- NULL

  }

  df_corr <- dplyr::filter(df_corr, !base::is.na(cor))

  # start plotting

  p <-
    ggplot2::ggplot(data = df_corr, mapping = ggplot2::aes(x = var1, y = var2)) +
    ggplot2::scale_color_gradient2(midpoint = 0, low = clr.low, high = clr.high, na.value = "white") +
    ggplot2::scale_size_area(max_size = size.max) +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(),
      axis.text.x = ggplot2::element_text(angle = 90)
    ) +
    ggplot2::guides(size = FALSE) +
    ggplot2::coord_fixed()



  # add grid
  if(base::isTRUE(draw.grid)){

    grid_add_on <-
      ggplot2::geom_tile(
        fill = NA, color = grid.clr, size = grid.size,
        data = df_grid
      )

  } else {

    grid_add_on <- NULL

  }

  # create aes mapping
  if(base::isTRUE(display.with.size)){

    mapping <- ggplot2::aes(x = var1, y = var2, color = cor, size = cor_abs)

  } else {

    mapping <- ggplot2::aes(x = var1, y = var2, color = cor)

  }

  # add point geometry
  if(shape == "circle"){

    shape_num <- 19

    if(base::isTRUE(display.with.size)){

      geom_add_on <- ggplot2::geom_point(shape = shape_num, mapping = mapping)

    } else {

      geom_add_on <- ggplot2::geom_point(shape = shape_num, mapping = mapping, size = size.max)

    }

  } else if(shape == "rect"){

    shape_num <- 15

    if(base::isTRUE(display.with.size)){

      geom_add_on <- ggplot2::geom_point(shape = shape_num, mapping = mapping)

    } else {

      geom_add_on <- ggplot2::geom_point(shape = shape_num, mapping = mapping, size = size.max)

    }

  } else if(shape == "tile"){

    geom_add_on <-
      list(
        ggplot2::geom_tile(mapping = ggplot2::aes(x = var1, y = var2, fill = cor)),
        ggplot2::scale_fill_gradient2(midpoint = 0, low = clr.low, high = clr.high, na.value = "white")
      )

  }

  # add text geometry
  if(base::isTRUE(display.values)){

    values_add_on <-
      list(
        ggplot2::geom_text(mapping = ggplot2::aes(label = base::round(cor, digits = values.digits)),
                           alpha = values.alpha,
                           color = values.clr,
                           size = values.size,
                           data = df_corr),
        ggplot2::guides(fill = FALSE)
      )

  } else {

    values_add_on <- NULL

  }

  p +
    geom_add_on +
    grid_add_on +
    values_add_on +
    insignificance_add_on

}


#' Title
#'
#' @param corr.obj
#' @param across
#' @param across.subset
#' @param relevel
#' @param method.corr
#' @param signif.level
#' @param clr.low
#' @param clr.high
#' @param shape
#' @param display.with.size
#' @param size.max
#' @param display.values
#' @param values.alpha
#' @param values.clr
#' @param values.digits
#' @param values.size
#' @param draw.grid
#' @param grid.clr
#' @param grid.size
#' @param nrow
#' @param ncol
#' @param variables.subset
#' @param plot.type
#' @param display.diagonal
#'
#' @return
#' @export
#'
plot_corrplots <- function(corr.obj,
                           across,
                           across.subset = NULL,
                           relevel = TRUE,
                           variables.subset = NULL,
                           method.corr = NULL,
                           plot.type = "lower",
                           display.diagonal = TRUE,
                           signif.level = NULL,
                           clr.low = "darkred",
                           clr.high = "steelblue",
                           shape = "tile",
                           display.with.size = TRUE,
                           size.max = 15,
                           display.values = TRUE,
                           values.alpha = 0.9,
                           values.clr = "black",
                           values.digits = 2,
                           values.size = 4,
                           draw.grid = TRUE,
                           grid.clr = "black",
                           grid.size = 0.5,
                           nrow = NULL,
                           ncol = NULL){

  assign_corr_default(corr.obj)


  # control -----------------------------------------------------------------

  is_value(x = across, mode = "character")

  check_one_of(input = across, against = corr.obj@variables_discrete)

  check_one_of(input = plot.type, against = c("lower", "upper", "complete"))

  if(base::is.character(variables.subset)){

    store <- variables.subset

    variables.subset <-
      stringr::str_remove_all(variables.subset, pattern = "^-")

    check_one_of(
      input = variables.subset,
      against = corr.obj@variables_num
    )

    variables.subset <- store

  }

  corr_results <-
    get_corr_results(corr.obj = corr.obj,
                     method.corr = method.corr,
                     across = across,
                     across.subset = across.subset
    )


  df_corr <-
    purrr::map2_dfr(
      .x = corr_results,
      .y = base::names(corr_results),
      .f = function(corr_result, group){

        corr_mtr <- corr_result$r
        p_mtr <- corr_result$P

        if(plot.type == "lower"){

          corr_mtr[base::upper.tri(x = corr_mtr, diag = !display.diagonal)] <- NA
          p_mtr[base::upper.tri(x = p_mtr, diag = !display.diagonal)] <- NA

        } else if(plot.type == "upper"){

          corr_mtr[base::lower.tri(x = corr_mtr, diag = !display.diagonal)] <- NA
          p_mtr[base::lower.tri(x = p_mtr, diag = !display.diagonal)] <- NA

        }

        df_corr <-
          reshape2::melt(data = base::as.matrix(corr_mtr),
                         varnames = c("var1", "var2"),
                         value.name = "cor") %>%
          tibble::as_tibble() %>%
          dplyr::mutate(
            cor_abs = base::abs(x = cor),
            !!across := {{group}}
          )

        df_corr$grid <- !base::is.na(x = df_corr$cor)

        if(base::is.numeric(signif.level)){

          is_value(x = signif.level, mode = "numeric")

          df_pval <-
            reshape2::melt(data = base::as.matrix(p_mtr),
                           varnames = c("var1", "var2"),
                           value.name = "pval") %>%
            tibble::as_tibble()

          df_corr <-
            dplyr::left_join(x = df_corr, y = df_pval, by = c("var1", "var2")) %>%
            dplyr::mutate(
              significance = dplyr::case_when(
                base::is.na(pval) ~ "na",
                pval < signif.level ~ "Significant",
                pval >= signif.level ~ "Insignificant"
              )
            )

          df_corr$cor[df_corr$significance == "Insignificant"] <- NA

        }

        base::return(df_corr)

      }
    ) %>%
    dplyr::mutate(
      !!across := base::as.factor(x = !!rlang::sym(across))
    ) %>%
    confuns::check_across_subset(
      df = .,
      across = across,
      across.subset = across.subset,
      relevel = relevel
    )

  # filter variables of interest
  df_corr <-
    check_across_subset(df_corr,
                        across = "var1",
                        across.subset = variables.subset,
                        relevel = FALSE
    )

  df_corr <-
    check_across_subset(df_corr,
                        across = "var2",
                        across.subset = variables.subset,
                        relevel = FALSE)

  # make sure that the grid is displayed completely
  df_grid <- dplyr::filter(df_corr, grid)

  # only discards rows that are na due to upper.tri()/lower.tri()
  df_corr <- dplyr::filter(df_corr, !base::is.na(cor))

  p <-
    ggplot2::ggplot(data = df_corr, mapping = ggplot2::aes(x = var1, y = var2)) +
    ggplot2::scale_color_gradient2(midpoint = 0, low = clr.low, high = clr.high, na.value = "white") +
    ggplot2::scale_size_area(max_size = size.max) +
    ggplot2::facet_wrap(
      facets = stats::as.formula(stringr::str_c(". ~", across, sep = " ")),
      nrow = nrow,
      ncol = ncol
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(),
      axis.text.x = ggplot2::element_text(angle = 90)
    ) +
    ggplot2::guides(size = FALSE) +
    ggplot2::coord_fixed()

  # add grid
  if(base::isTRUE(draw.grid)){

    grid_add_on <-
      ggplot2::geom_tile(
        fill = NA, color = grid.clr, size = grid.size,
        data = df_grid
      )

  } else {

    grid_add_on <- NULL

  }

  # create aes mapping
  if(base::isTRUE(display.with.size)){

    mapping <- ggplot2::aes(x = var1, y = var2, color = cor, size = cor_abs)

  } else {

    mapping <- ggplot2::aes(x = var1, y = var2, color = cor)

  }

  # add point geometry
  if(shape == "circle"){

    shape_num <- 19

    if(base::isTRUE(display.with.size)){

      geom_add_on <- ggplot2::geom_point(shape = shape_num, mapping = mapping)

    } else {

      geom_add_on <- ggplot2::geom_point(shape = shape_num, mapping = mapping, size = size.max)

    }

  } else if(shape == "rect"){

    shape_num <- 15

    if(base::isTRUE(display.with.size)){

      geom_add_on <- ggplot2::geom_point(shape = shape_num, mapping = mapping)

    } else {

      geom_add_on <- ggplot2::geom_point(shape = shape_num, mapping = mapping, size = size.max)

    }

  } else if(shape == "tile"){

    geom_add_on <-
      list(
        ggplot2::geom_tile(mapping = ggplot2::aes(x = var1, y = var2, fill = cor)),
        ggplot2::scale_fill_gradient2(midpoint = 0, low = clr.low, high = clr.high, na.value = "white")
      )

  }

  # add text geometry
  if(base::isTRUE(display.values)){

    values_add_on <-
      list(
        ggplot2::geom_text(mapping = ggplot2::aes(label = base::round(cor, digits = values.digits)),
                           alpha = values.alpha,
                           color = values.clr,
                           size = values.size,
                           data = df_corr),
        ggplot2::guides(fill = FALSE, color = FALSE)
      )

  } else {

    values_add_on <- NULL

  }

  # add insignificant add on
  if(base::is.numeric(signif.level)){

    df_corr_insignificant <-
      dplyr::filter(df_grid, significance == "Insignificant")

    if(base::nrow(df_corr_insignificant) >= 1){

      insignificance_add_on <-
        ggplot2::geom_point(shape = 4, size = size.max, data = df_corr_insignificant, color = "black")

    } else {

      insignificance_add_on <- NULL

    }

  } else {

    insignificance_add_on <- NULL

  }

  # return plot -------------------------------------------------------------

  p +
    geom_add_on +
    grid_add_on +
    values_add_on +
    insignificance_add_on


}


#' Title
#'
#' @param corr.obj
#' @param method.corr
#' @param across
#' @param aes.fill
#'
#' @return
#' @export
#'
plot_correlation_sd <- function(corr.obj, method.corr = NULL,  across = NULL, aes.fill = "sd", signif.level = NULL){

  assign_corr_default(corr.obj)

  corr_list <- corr.obj@results_across[[method.corr]]

  df_corr <-
    purrr::map2_dfr(
      .x = corr_list,
      .y = base::names(corr_list),
      .f = function(corr_results, across){

        plot.type = "complete"
        display.diagonal = FALSE

        df_across <-
          purrr::map2_dfr(
            .x = corr_results,
            .y = base::names(corr_results),
            .f = function(corr_result, group){

              corr_mtr <- corr_result$r
              p_mtr <- corr_result$P

              if(plot.type == "lower"){

                corr_mtr[base::upper.tri(x = corr_mtr, diag = !display.diagonal)] <- NA
                p_mtr[base::upper.tri(x = p_mtr, diag = !display.diagonal)] <- NA

              } else if(plot.type == "upper"){

                corr_mtr[base::lower.tri(x = corr_mtr, diag = !display.diagonal)] <- NA
                p_mtr[base::lower.tri(x = p_mtr, diag = !display.diagonal)] <- NA

              }

              df_corr <-
                reshape2::melt(data = base::as.matrix(corr_mtr),
                               varnames = c("var1", "var2"),
                               value.name = "cor") %>%
                tibble::as_tibble() %>%
                dplyr::mutate(
                  cor_abs = base::abs(x = cor)
                )

              df_corr$grid <- !base::is.na(x = df_corr$cor)

              if(base::is.numeric(signif.level)){

                is_value(x = signif.level, mode = "numeric")

                df_pval <-
                  reshape2::melt(data = base::as.matrix(p_mtr),
                                 varnames = c("var1", "var2"),
                                 value.name = "pval") %>%
                  tibble::as_tibble()

                df_corr <-
                  dplyr::left_join(x = df_corr, y = df_pval, by = c("var1", "var2")) %>%
                  dplyr::mutate(
                    significance = dplyr::case_when(
                      base::is.na(pval) ~ "na",
                      pval < signif.level ~ "Significant",
                      pval >= signif.level ~ "Insignificant"
                    )
                  )

                df_corr$cor[df_corr$significance == "Insignificant"] <- NA

              }

              base::return(df_corr)

            }
          ) %>%
          dplyr::mutate(across = {{across}})

        base::return(df_across)

      }
    ) %>%
    confuns::check_across_subset(
      across = "across",
      across.subset = across,
      relevel = TRUE
    )

  df_corr_var <-
    dplyr::group_by(df_corr, across, var1, var2) %>%
    dplyr::summarise(
      var = stats::var(x = cor, na.rm = TRUE),
      sd = stats::sd(x = cor, na.rm = TRUE)
    )


  ggplot2::ggplot(data = df_corr_var, mapping = ggplot2::aes(x = var1, y = var2)) +
    ggplot2::geom_tile(mapping = ggplot2::aes(fill = .data[[aes.fill]])) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(),
      axis.text.x = ggplot2::element_text(angle = 90)
    ) +
    ggplot2::facet_wrap(facets = . ~ across)


}

#' @rdname plot_correlation_sd
#' @export
plot_correlation_variance <- plot_correlation_sd





# object manipulation -----------------------------------------------------

#' @title Discard numeric variables of corr_conv objects
#'
#' @param corr.obj
#' @param vars
#' @param discard.data
#' @param ...
#'
#' @return
#' @export
#'
discard_numeric_vars <- function(corr.obj, vars, discard.data = TRUE, ...){

  check_one_of(
    input = vars,
    against = corr.obj@variables_num,
    ...
  )

  corr.obj@variables_num <-
    corr.obj@variables_num[!corr.obj@variables_num %in% vars]

  if(base::isTRUE(discard.data)){

    corr.obj@data <-
      base::as.data.frame(corr.obj@data) %>%
      dplyr::select(-dplyr::all_of(vars)) %>%
      base::as.matrix()

  }

  vars_discard <- stringr::str_c("-", vars)

  # discard @results_all
  corr.obj@results_all <-
    purrr::map(.x = corr.obj@results_all,
               .f = function(method_list){ # iterate over methods, names(): all corr methods

                 method_list_out <-
                   purrr::map_at(
                     .x = method_list,
                     .at = c("r", "P"),
                     .f = subset_mtr,
                     dims = c(1,2),
                     variables.subset = vars_discard
                   )

               })

  # discard @results_across
  corr.obj@results_across <-
    purrr::map(.x = corr.obj@results_across, # iterate over methods, names(): all corr methods
               .f = function(method_list){

                 method_list_out <-
                   purrr::map(.x = method_list, # iterate over grouping variables, names(): all grouping variables
                              .f = function(grouping_list){

                                grouping_list_out <-
                                  purrr::map(.x = grouping_list, # iterate over all groups, names(): all groups of the grouping variable
                                             .f = function(group_list){

                                               group_list_out <-
                                                 purrr::map_at(.x = group_list, # iterate over the three corr slots r, n, P
                                                               .at = c("r", "P"), # only matrix slots
                                                               .f = subset_mtr,
                                                               dims = c(1,2),
                                                               variables.subset = vars_discard)

                                               base::return(group_list_out)

                                             })

                                base::return(grouping_list_out)

                              })

                 base::return(method_list_out)

               })

  return(corr.obj)

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
    vredefine(input = corr.obj@variables_num, ...)

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
    vredefine_with(input = corr.obj@variables_num, ...)

  if(base::isTRUE(rename.data)){

    # rename @data
    corr.obj@data <-
      mrename_with(mtr = corr.obj@data, dims = 2, ...)

  }


  # rename @results_all
  corr.obj@results_all <-
    purrr::map(.x = corr.obj@results_all, # iterate over methods (pearson, spearman)
               .f = function(method_list){ # list of three slots: r, n, P

                 method_list_out <-
                   purrr::map_at(
                     .x = method_list,
                     .at = c("r", "P"),
                     .f = mrename_with,
                     ...
                   )

                 base::return(method_list_out)

               })

  # rename @results_across
  corr.obj@results_across <-
    purrr::map(.x = corr.obj@results_across, # iterate over methods, names(): all corr methods
               .f = function(method_list){

                 method_list_out <-
                   purrr::map(.x = method_list, # iterate over grouping variables, names(): all grouping variables
                              .f = function(grouping_list){

                                grouping_list_out <-
                                  purrr::map(.x = grouping_list, # iterate over all groups, names(): all groups of the grouping variable
                                             .f = function(group_list){

                                               group_list_out <-
                                                 purrr::map_at(.x = group_list, # iterate over the three corr slots r, n, P
                                                               .at = c("r", "P"), # only matrix slots
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


