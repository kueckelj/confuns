
#' Title
#'
#' @param df
#' @param x
#' @param y
#' @param across
#' @param across.subset
#' @param relevel
#' @param ncol
#' @param nrow
#' @param scales
#' @param space
#' @param pt.alpha
#' @param pt.clr
#' @param pt.clrp
#' @param pt.fill
#' @param pt.shape
#' @param pt.size
#' @param clr.aes
#' @param clr.by
#' @param clrp.adjust
#' @param display.smooth
#' @param smooth.alpha
#' @param smooth.clr
#' @param smooth.method
#' @param smooth.se
#' @param smooth.size
#' @param display.corr
#' @param corr.method
#' @param corr.p.min
#' @param corr.pos.x
#' @param corr.pos.y
#' @param corr.text.sep
#' @param corr.text.size
#' @param ...
#'
#' @return
#' @export
#'
plot_scatterplot <- function(df,
                             x,
                             y,
                             across = NULL,
                             across.subset = NULL,
                             relevel = TRUE,
                             ncol = NULL,
                             nrow = NULL,
                             scales = "fixed",
                             space = "fixed",
                             pt.alpha = 0.9,
                             pt.clr = "black",
                             pt.clrp = "milo",
                             pt.fill = "black",
                             pt.shape = 19,
                             pt.size = 1.5,
                             clr.aes = "color",
                             clr.by = NULL,
                             clrp.adjust = NULL,
                             display.smooth = FALSE,
                             smooth.alpha = 0.9,
                             smooth.clr = "blue",
                             smooth.method = "lm",
                             smooth.se = FALSE,
                             smooth.size = 1,
                             display.corr = FALSE,
                             corr.method = "pearson",
                             corr.p.min = 0.00005,
                             corr.pos.x = NULL,
                             corr.pos.y = NULL,
                             corr.text.sep = "\n",
                             corr.text.size = 1,
                             ...
                             ){


  check_data_frame(
    df = df,
    var.class = purrr::map(.x = c(x,y), .f = function(i){ return("numeric") }) %>% purrr::set_names(nm = c(x,y)),
    verbose = TRUE,
    with.time = FALSE
  )

  # subsetting according to across input ------------------------------------

  if(base::length(across) == 2){

    if(base::length(relevel == 1)){

      relevel <- base::rep(relevel, 2)

    }

    if(!base::is.null(across.subset) & !is_list(input = across.subset)){

      msg <- "If input for argument 'across' is of length two the input for argument 'across.subset' must be a named list or NULL."

      give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)

    }

    df <- check_across_subset(
      df = df, across = across[1],
      across.subset = across.subset[[across[1]]],
      relevel = relevel[1]
      )

    df <-
      check_across_subset(
        df = df,
        across = across[2],
        across.subset = across.subset[[across[2]]],
        relevel = relevel[2]
        )

  } else {

    df <-
      check_across_subset(
        df = df,
        across = across,
        across.subset = across.subset,
        relevel = relevel[1]
        )

  }

  p <-
    ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank()
    )


  if(base::is.character(clr.by)){

    check_one_of(
      input = clr.by,
      against = dplyr::select(df, where(is.factor), where(is.character)) %>% base::colnames()
    )

      if(pt.shape %in% color_shapes){

        p_mapping <- ggplot2::aes(color = .data[[clr.by]])

      } else if(pt.shape %in% fill_shapes){

        p_mapping <- ggplot2::aes(fill = .data[[clr.by]])

      } else {

        base::stop("Input for argument pt.shape/pt_shape must be an integer between 1 and 25.")

      }

    p <-
      p +
      ggplot2::geom_point(
        mapping = p_mapping,
        alpha = pt.alpha,
        shape = pt.shape,
        size = pt.size) +
      scale_color_add_on(
        aes = clr.aes,
        variable = df[[clr.by]],
        clrp = pt.clrp,
        clrp.adjust = clrp.adjust,
        ...
      )

  } else {

    p <-
      p +
      ggplot2::geom_point(
        alpha = pt.alpha,
        color = pt.clr,
        fill = pt.fill,
        shape = pt.shape,
        size = pt.size)

  }


  if(!base::is.null(across)){

    if(base::length(across) == 1){

      p <-
        p +
        ggplot2::facet_wrap(facets = stats::as.formula(stringr::str_c(". ~ ", across)),
                            scales = scales,
                            nrow = nrow,
                            ncol = ncol)

    } else {

      across1 <- across[1]
      across2 <- across[2]

      p <-
        p +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!rlang::sym(across1)),
          cols = ggplot2::vars(!!rlang::sym(across2)),
          scales = scales,
          space = space)

    }

  }


  # add model ---------------------------------------------------------------

  if(base::isTRUE(display.smooth)){

    p <- p +
      ggplot2::geom_smooth(
        formula = y ~ x,
        alpha = smooth.alpha,
        color = smooth.clr,
        method = smooth.method,
        se = smooth.se,
        size = smooth.size
        )

  }



  # add correlation results -------------------------------------------------

  if(base::isTRUE(display.corr)){

    if(base::is.null(across)){

      df_corr <-
        scatter_correlation_df(
          x.var = dplyr::pull(df, var = {{x}}),
          y.var = dplyr::pull(df, var = {{y}}),
          corr.pos.x = corr.pos.x,
          corr.pos.y = corr.pos.y,
          corr.method = corr.method,
          corr.p.min = corr.p.min,
          corr.text.sep = corr.text.sep
        )

      p <-
        p + ggplot2::geom_text(mapping = ggplot2::aes(x = x, y = y, label = label),
                               data = df_corr, size = corr.text.size)

    } else if(base::length(across) == 1){

      across_var <- dplyr::pull(df, var = {{across}})

      if(base::is.factor(across_var)){

        across_values <- base::levels(across_var)

      } else {

        across_values <- base::unique(across_var)

      }

      df_corr <-
        purrr::map_df(.x = across_values,
                      x = x,
                      y = y,
                      .f = function(across_value, x, y){

                        df_filtered <-
                          dplyr::filter(df, !!rlang::sym(across) == {{across_value}})

                        df_corr <-
                          scatter_correlation_df(
                            x.var = dplyr::pull(df_filtered, var = {{x}}),
                            y.var = dplyr::pull(df_filtered, var = {{y}}),
                            corr.pos.x = corr.pos.x,
                            corr.pos.y = corr.pos.y,
                            corr.p.min = corr.p.min,
                            corr.method = corr.method,
                            corr.text.sep = corr.text.sep
                          )

                        df_corr[[across]] <- across_value

                        base::return(df_corr)

                      })

      p <-
        p + ggplot2::geom_text(mapping = ggplot2::aes(x = x, y = y, label = label),
                               data = df_corr, size = corr.text.size)

    } else if(base::length(across) == 2){

      across1 <- across[1]

      across_var1 <- dplyr::pull(df, var = {{across1}})

      if(base::is.factor(across_var1)){

        across_values1 <- base::levels(across_var1)

      } else if(base::is.character(across_var1)){

        across_values1 <- base::unique(across_var1)

      }


      across2 <- across[2]

      across_var2 <- dplyr::pull(df, var = {{across2}})

      if(base::is.factor(across_var2)){

        across_values2 <- base::levels(across_var2)

      } else if(base::is.character(across_var2)){

        across_values2 <- base::unique(across_var2)

      }

      across_combinations <-
        tidyr::expand_grid(x = across_values1, y = across_values2) %>%
        magrittr::set_colnames(value = across)

      df_corr <-
        base::apply(X = across_combinations, MARGIN = 1,
                    x = x, y = y, across = across,
                    FUN = function(combination, x, y, across){

                      combination <- base::as.character(combination)

                      across1 <- across[1]
                      across2 <- across[2]

                      across_value1 <- combination[1]
                      across_value2 <- combination[2]

                      df_filtered <-
                        dplyr::filter(df,
                                      !!rlang::sym(across1) == {{across_value1}} &
                                      !!rlang::sym(across2) == {{across_value2}}
                                      )

                      df_corr <-
                        scatter_correlation_df(
                          x.var = dplyr::pull(df_filtered, var = {{x}}),
                          y.var = dplyr::pull(df_filtered, var = {{y}}),
                          corr.pos.x = corr.pos.x,
                          corr.pos.y = corr.pos.y,
                          corr.p.min = corr.p.min,
                          corr.method = corr.method,
                          corr.text.sep = corr.text.sep
                        )

                      df_corr[[across1]] <- across_value1
                      df_corr[[across2]] <- across_value2

                      base::return(df_corr)

                    }) %>%
        purrr::map_df(.f = ~ .x)

      p <-
        p +
        ggplot2::geom_text(
          mapping = ggplot2::aes(x = x, y = y, label = label),
          data = df_corr, size = corr.text.size
          ) +
        ggplot2::theme(
          strip.background = ggplot2::element_rect()
        )

    }

  }


  # return plot -------------------------------------------------------------

  base::return(p)

}



# helper ------------------------------------------------------------------

scatter_correlation_df <- function(x.var,
                                   y.var,
                                   corr.method,
                                   corr.p.min,
                                   corr.pos.x,
                                   corr.pos.y,
                                   corr.text.sep){

  corr_res <- stats::cor.test(x = x.var, y = y.var, method = corr.method)

  if(base::is.null(corr.pos.x)){

    corr.pos.x <- base::max(x.var) * 0.1

  }

  if(base::is.null(corr.pos.y)){

    corr.pos.y <- base::max(y.var) * 0.9

  }

  p_rounded <- base::round(corr_res$p.value, digits = 5)

  if(p_rounded < corr.p.min){

    p_rounded <- corr.p.min

  }

  r_rounded <- base::round(corr_res$estimate, digits = 2)

  corr_info <- stringr::str_c("p.value < ", p_rounded, corr.text.sep, "r = ", r_rounded)

  res_df <-
    data.frame(
      x = corr.pos.x,
      y = corr.pos.y,
      label = corr_info
    )

}









