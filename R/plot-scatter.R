
#' Title
#'
#' @inherit argument_dummy params
#'
#' @return A ggplot.
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
                             pt.clr = NA,
                             pt.color = "black",
                             pt.fill = "black",
                             pt.shape = 19,
                             pt.size = 1.5,
                             alpha.by = NULL,
                             color.aes = "color",
                             color.by = NULL,
                             color.trans = "identity",
                             order.by = NULL,
                             order.desc = FALSE,
                             shape.by = NULL,
                             size.by = NULL,
                             clrp = "milo",
                             clrp.adjust = NULL,
                             clrsp = "inferno",
                             display.smooth = FALSE,
                             smooth.alpha = 0.9,
                             smooth.color = "blue",
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
                             transform.with = NULL,
                             clr.aes = NA,
                             clr.by = NA,
                             pt.clrp = NA,
                             ...
                             ){

  if(!base::is.na(pt.clr)){

    warning("pt.clr is deprecated")
    pt.color <- pt.clr

  }

  if(!base::is.na(clr.aes)){

    warning("clr.aes is deprecated")
    color.aes <- clr.aes

  }

  if(!base::is.na(clr.by)){

    warning("clr.by is deprecated")
    color.by <- clr.by

  }

  check_data_frame(
    df = df,
    var.class = purrr::map(.x = c(x,y), .f = function(i){ return("numeric") }) %>% purrr::set_names(nm = c(x,y)),
    verbose = TRUE,
    with.time = FALSE
  )

  df <- transform_df(df = df, transform.with = transform.with, sep = ".")

  if(base::is.character(order.by)){

    check_one_of(
      input = order.by,
      against = get_numeric_names(df),
      fdb.opt = 2,
      ref.opt.2 = "numeric variables"
    )

    if(base::isTRUE(order.desc)){

      df <- dplyr::arrange(df, dplyr::desc(x = !!rlang::sym(order.by)), .by_group = TRUE)

    } else {

      df <- dplyr::arrange(df, !!rlang::sym(order.by), .by_group = TRUE)

    }

  }

  # subsetting according to across input ------------------------------------

  df <- check_across_subset2(df = df, across = across, across.subset = across.subset, relevel = relevel)

  p <-
    ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank()
    )

  if(base::is.character(color.by)){

    check_one_of(
      input = color.by,
      against = base::colnames(df)
    )

  }


  # add points --------------------------------------------------------------

  if(color.aes == "color" & base::is.character(color.by)){

    p_mapping <-
      ggplot2::aes_string(
        alpha = alpha.by,
        color = color.by,
        shape = shape.by,
        size = size.by
      )

    var <- df[[color.by]]
    fill.by <- NULL

  } else if(color.aes == "fill" & base::is.character(color.by)){

    p_mapping <-
      ggplot2::aes_string_(
        alpha = alpha.by,
        fill = color.by,
        shape = shape.by,
        size = size.by
      )

    var <- df[[color.by]]
    fill.by <- color.by
    color.by <- NULL

  } else {

    p_mapping <- ggplot2::aes_string(alpha = alpha.by, shape = shape.by, size = size.by)
    var <- "numeric"

  }

  params <-
    adjust_ggplot_params(
      params = list(
        alpha = pt.alpha,
        color = pt.color,
        fill = pt.fill,
        shape = pt.shape,
        size = pt.size
      ),
      sep = "."
    )

  p <-
    p +
    ggplot2::layer(
      geom = "point",
      stat = "identity",
      position = "identity",
      mapping = p_mapping,
      params = params,
      data = df
      ) +
    scale_color_add_on(
      aes = color.aes,
      variable = var,
      clrsp = clrsp,
      clrp = clrp,
      clrp.adjust = clrp.adjust,
      color.trans = color.trans,
      ...
    )

  # add facets --------------------------------------------------------------

  facet_add_on <-
    make_facet_add_on(
      across = across,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      space = space
    )

  p <- p + facet_add_on

  # add model ---------------------------------------------------------------

  if(base::isTRUE(display.smooth)){

    p <- p +
      ggplot2::geom_smooth(
        formula = y ~ x,
        alpha = smooth.alpha,
        color = smooth.color,
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
        p +
        ggplot2::geom_text(
          mapping = ggplot2::aes(x = x, y = y, label = label),
          data = df_corr, size = corr.text.size
        )

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
        p +
        ggplot2::geom_text(
          mapping = ggplot2::aes(x = x, y = y, label = label),
          data = df_corr, size = corr.text.size
        )

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

  return(p)

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









