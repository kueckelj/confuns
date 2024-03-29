





# s4 ----------------------------------------------------------------------


dim_red_conv <- methods::setClass(Class = "dim_red_conv",
                                  slots = c(
                                    additional_arguments = "list",
                                    data = "matrix",
                                    dims = "numeric",
                                    embedding = "data.frame",
                                    key_name = "character",
                                    meta = "data.frame",
                                    method = "character",
                                    results = "list",
                                    scale = "logical",
                                    variables_discrete = "character",
                                    variables_num = "character"
                                  )
)


#' @export

compute_dim_red <- function(data,
                            key.name,
                            method.dim.red,
                            scale = TRUE,
                            verbose = TRUE,
                            seed = NULL,
                            ...){

  is_value(method.dim.red, mode = "character")

  check_one_of(
    input = method.dim.red,
    against = c("pca", "tsne", "umap")
    )

  data <- base::as.data.frame(data)

  if(!key.name %in% base::colnames(data)){

    key_var <- base::rownames(data)

    if(!base::is.character(key_var)){

      msg <- "If argument 'key.name' is not a column in input for argument data the input must have rownames."

      give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)

    }

  } else {

    n_obs <- base::nrow(data)

    check_one_of(
      input = key.name,
      against = base::colnames(purrr::keep(data, .p = ~ base::is.character(.x) & dplyr::n_distinct(.x) == n_obs))
      )

    key_var <- dplyr::pull(data, var = {{key.name}})

  }

  numeric_df <-
    dplyr::select_if(data, .predicate = base::is.numeric)

  unscaled_mtr <- base::as.matrix(numeric_df)
  base::rownames(unscaled_mtr) <- key_var

  if(base::isTRUE(scale)){

    numeric_df <- rescale_df(df = numeric_df, verbose = verbose)

  }

  mtr <- base::as.matrix(numeric_df)

  give_feedback(msg = glue::glue("Computing dimensional reduction. Method: {method.dim.red}"), verbose = verbose)

  if(base::is.numeric(seed)){

    give_feedback(msg = glue::glue("Setting seed: {seed}"), verbose = verbose)

    base::set.seed(seed = seed)

  }

  if(method.dim.red == "pca"){

    dim_red_res <- irlba::prcomp_irlba(x = mtr, ...)

    pca_names <- stringr::str_c("pc", 1:base::ncol(dim_red_res$x), sep = "_")

    embedding_df <-
      base::as.data.frame(dim_red_res$x) %>%
      magrittr::set_colnames(value = pca_names)

    dim_red_res <- dim_red_res[base::names(dim_red_res) != "x"]

    dim_red_res <-
      purrr::map(.x = base::as.list(dim_red_res), .f = ~ .x) %>%
      purrr::set_names(nm = base::names(dim_red_res))

  } else if(method.dim.red == "tsne"){

    dim_red_res <- tsne::tsne(X = mtr, ...)

    tsne_names <- stringr::str_c("tsne", 1:base::ncol(dim_red_res), sep = "_")

    embedding_df <-
      base::as.data.frame(dim_red_res) %>%
      magrittr::set_colnames(value = tsne_names)

    dim_red_res <- list()

  } else if(method.dim.red == "umap"){

    dim_red_res <- umap::umap(d = mtr, ...)

    umap_names <- stringr::str_c("umap", 1:base::ncol(dim_red_res$layout), sep = "_")

    embedding_df <-
      base::as.data.frame(dim_red_res$layout) %>%
      magrittr::set_colnames(value = umap_names)

    dim_red_res <- dim_red_res[base::names(dim_red_res) %in% c("layout", "data")]

    dim_red_res <-
      purrr::map(.x = base::as.list(dim_red_res), .f = ~ .x) %>%
      purrr::set_names(nm = base::names(dim_red_res))

  }

  give_feedback(msg = "Done.", verbose = verbose)

  meta_df <-
    purrr::discard(.x = data, .p = base::is.numeric)

  if(base::ncol(meta_df) >= 1){

    meta_df <-
      dplyr::mutate(meta_df, !!key.name := {{key_var}}) %>%
      dplyr::select(dplyr::all_of(key.name), dplyr::everything())

  } else {

    meta_df <- base::data.frame()

  }

  dim_red_obj <- methods::new(
    Class = "dim_red_conv",
    additional_arguments = keep_named(list(...)),
    data = unscaled_mtr,
    dims = 1:base::ncol(embedding_df),
    embedding =
      dplyr::mutate(embedding_df, !!key.name := {{key_var}}) %>%
      dplyr::select(dplyr::all_of(key.name), dplyr::everything()),
    key_name = key.name,
    meta = meta_df,
    method = method.dim.red,
    results = dim_red_res,
    scale = scale,
    variables_discrete = base::colnames(meta_df),
    variables_num = base::colnames(mtr)
    )

  base::return(dim_red_obj)

}



#' @export
#'
get_dim_red_df <- function(dimred.obj, with.data = TRUE, with.meta = TRUE){

  key_name <- dimred.obj@key_name

  embedding_df <-
    base::as.data.frame(dimred.obj@embedding)

  if(base::isTRUE(with.data)){

    embedding_df <-
      dplyr::left_join(
        x = embedding_df,
        y = base::as.data.frame(dimred.obj@data) %>% tibble::rownames_to_column(var = key_name),
        by = key_name
      )

  }

  if(base::isTRUE(with.meta) & !base::identical(dimred.obj@meta, base::data.frame())){

    embedding_df <-
      dplyr::left_join(
        x = embedding_df,
        y = base::as.data.frame(dimred.obj@meta),
        by = key_name
      )

  }

  base::return(embedding_df)

}



#' @export
#'
plot_dim_red <- function(dimred.obj,
                         clr.aes = "fill",
                         clr.by = NULL,
                         clrp.adjust = NULL,
                         pt.alpha = 0.9,
                         pt.clr = "black",
                         pt.clrp = "milo",
                         pt.clrsp = "inferno",
                         pt.fill = "black",
                         pt.shape = 21,
                         pt.size = 3,
                         ncol = NULL,
                         nrow = NULL,
                         add.df = NA,
                         add.ons = list(),
                         ...
                         ){

  key <- dimred.obj@key_name

  # create plot df
  plot_df <- get_dim_red_df(dimred.obj)

  if(base::is.data.frame(add.df)){

    check_data_frame(
      df = add.df,
      var.class = purrr::set_names(x = list("character"), nm = key)
    )

    plot_df <-
      dplyr::left_join(
        x = plot_df,
        y = add.df,
        by = key
      )

  }

  x_axis <- base::colnames(plot_df)[2]
  y_axis <- base::colnames(plot_df)[3]

  # create geom_point_add_on
  if(base::is.character(clr.by)){

    # select additional plot variables
    plot_dfs <-
      dplyr::select(plot_df, -dplyr::all_of(c(key, x_axis, y_axis))) %>%
      dplyr::select(-dplyr::starts_with(match = "pc_"))

    check_one_of(
      input = clr.by,
      against = base::colnames(plot_dfs),
      ref.input = glue::glue("input for argument clr.by/clr_by")
    )

  } else {

    geom_point_add_on <-
      ggplot2::geom_point(alpha = pt.alpha, fill = pt.fill, shape = pt.shape, size = pt.size)

  }

  if(base::is.character(clr.by)){

    if(base::length(clr.by) > 1){

      plot_output <-
        purrr::map(.x = clr.by,
                   .f = function(.clr.by){

                     if(clr.aes == "color"){

                       geom_point_add_on <-
                         ggplot2::geom_point(
                           mapping = ggplot2::aes(color = .data[[.clr.by]]),
                           alpha = pt.alpha, fill = pt.fill, shape = pt.shape, size = pt.size
                         )

                     } else if(clr.aes == "fill"){

                       geom_point_add_on <-
                         ggplot2::geom_point(
                           mapping = ggplot2::aes(fill = .data[[.clr.by]]),
                           alpha = pt.alpha, color = pt.clr, shape = pt.shape, size = pt.size
                         )

                     }

                     p <-
                       ggplot2::ggplot(data = plot_df, mapping = ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]])) +
                       geom_point_add_on +
                       ggplot2::theme_classic() +
                       ggplot2::theme(
                         axis.text = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(),
                         axis.title = ggplot2::element_blank(),
                         strip.background = ggplot2::element_blank(),
                         plot.subtitle = ggplot2::element_text(hjust = 0.5)
                       ) +
                       ggplot2::labs(subtitle = .clr.by) +
                       scale_color_add_on(
                         aes = clr.aes,
                         variable = plot_df[[.clr.by]],
                         clrp = pt.clrp,
                         clrsp = pt.clrsp,
                         clrp.adjust = clrp.adjust,
                         ...
                       ) +
                       add.ons

                     base::return(p)

                   })

      resulting_grid <- gridExtra::grid.arrange(grobs = plot_output, nrow = nrow, ncol = ncol)

      base::plot(resulting_grid)

    } else {

      if(clr.aes == "color"){

        geom_point_add_on <-
          ggplot2::geom_point(
            mapping = ggplot2::aes(color = .data[[clr.by]]),
            alpha = pt.alpha, fill = pt.fill, shape = pt.shape, size = pt.size
          )

      } else if(clr.aes == "fill"){

        geom_point_add_on <-
          ggplot2::geom_point(
            mapping = ggplot2::aes(fill = .data[[clr.by]]),
            alpha = pt.alpha, color = pt.clr, shape = pt.shape, size = pt.size
          )

      }

      p <-
        ggplot2::ggplot(data = plot_df, mapping = ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]])) +
        geom_point_add_on +
        ggplot2::theme_classic() +
        ggplot2::theme(
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          strip.background = ggplot2::element_blank(),
          plot.subtitle = ggplot2::element_text(hjust = 0.5)
        ) +
        ggplot2::labs(subtitle = clr.by) +
        scale_color_add_on(
          aes = clr.aes,
          variable = plot_df[[clr.by]],
          clrp = pt.clrp,
          clrsp = pt.clrsp,
          clrp.adjust = clrp.adjust,
          ...
        ) +
        add.ons

      base::return(p)

    }



  } else {

    plot_output <-
      ggplot2::ggplot(data = plot_df, mapping = ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]])) +
      geom_point_add_on +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        plot.title.position = "plot"
      )

    base::return(plot_output)
  }



}





