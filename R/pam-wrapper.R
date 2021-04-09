



# s4 ----------------------------------------------------------------------

pam_conv <- methods::setClass(Class = "pam_conv",
                              slots = c(
                                data = "matrix",
                                default = "list",
                                results = "list",
                                variables = "character",
                                observations = "character",
                                key_name = "character"
                              ))




# r-objects ---------------------------------------------------------------


valid_metrics_pam <- c("euclidean", "manhattan")




# input check -------------------------------------------------------------


#' Title
#'
#' @param input
#' @param method.dist
#' @param method.aggl
#' @param fdb.fn
#'
#' @return
#' @export
#'
check_pam_availability <- function(input, metric.pam, k, fdb.fn = "stop"){

  if(!shiny::isTruthy(input)){

    msg <- glue::glue("Could not find pam results for metric '{metric.pam}' and k = {k}.")

    give_feedback(msg = msg, fdb.fn = fdb.fn)

    input <- NULL

  }

  base::invisible(input)

}

#' Title
#'
#' @param k
#' @param metric.pam
#' @param ...
#'
#' @return
#' @export
#'

check_pam_input <- function(k, metric.pam, k.length = NULL, m.length = NULL){

  is_vec(x = k, mode = "numeric", of.length = k.length)
  is_vec(x = metric.pam, mode = "character", of.length = m.length)

  if(base::any(k <= 1)){

    msg <- "Input for argument 'k' must not contain values 0 or 1."

    give_feedback(msg = msg, fdb.fn = "stop")

  }

  check_one_of(input = metric.pam, against = valid_metrics_pam)

}



# initiation --------------------------------------------------------------


initiate_pam_object <- function(pam.data,
                                key.name,
                                default.as.dist = FALSE,
                                default.metric.pam = "euclidean",
                                default.k = 2,
                                default.dir = "conv-pam-obj.RDS"){

  pam.obj <- methods::new(Class = "pam_conv")

  # set observations
  obs <- base::rownames(pam.data)

  # set observations
  obs <- base::rownames(pam.data)

  if(dplyr::n_distinct(obs) == base::nrow(pam.data)){

    pam.obj@observations <- obs

  } else {

    msg <- "Observations of input for argument 'pam.data' must be named and the number of unique rownames must be equal to the number of rows."

    confuns::give_feedback(msg = msg, fdb.fn = "stop")

  }

  # set variables
  vars <- base::colnames(pam.data)

  if(dplyr::n_distinct(vars) == base::ncol(pam.data)){

    pam.obj@variables <- vars

  }

  # set data
  numeric_check <- base::apply(X = pam.data, MARGIN = 2, FUN = base::is.numeric)

  if(base::all(numeric_check)){

    pam.obj@data <- base::as.matrix(pam.data)

  } else {

    msg <- "All variables of input for argument 'pam.data' must be numeric."

    give_feedback(msg = msg, fdb.fn = "stop")

  }

  # set key
  if(is_value(x = key.name, mode = "character")){

    pam.obj@key_name <- key.name

  }


  # set default
  pam.obj <-
    set_pam_default(
      pam.obj = pam.obj,
      metric.pam = default.metric.pam,
      k = default.k,
      as.dist = default.as.dist,
      directory = default.dir
    )

  base::return(pam.obj)

}




# set  --------------------------------------------------------------------

#' Title
#'
#' @param pam.obj
#' @param metric.pam
#' @param k
#' @param as.dist
#' @param verbose
#'
#' @return
#' @export
#'
set_pam_default <- function(pam.obj,
                            as.dist = NULL,
                            directory = NULL,
                            k = NULL,
                            metric.pam = NULL,
                            verbose = TRUE){

  default_list <- pam.obj@default

  if(base::isTRUE(as.dist) | base::isFALSE(as.dist)){

    default_list$as.dist <- as.dist

    give_feedback(msg = glue::glue("Setting default for 'as.dist' to {base::as.character(as.dist)}."),
                  verbose = verbose)

  }


  if(base::is.character(directory)){

    is_value(directory, "character")

    default_list$directory <- directory

    give_feedback(msg = glue::glue("Setting default directory to '{directory}'."),
                  verbose = verbose)

  }


  if(base::is.numeric(k)){

    is_value(k, "numeric")

    default_list$k <- k

    give_feedback(msg = glue::glue("Setting default input for argument 'k' to {k}."),
                  verbose = verbose)

  }


  if(base::is.character(metric.pam)){

    is_value(metric.pam, "character")

    check_one_of(input = metric.pam, against = valid_metrics_pam)

    default_list$metric.pam <- metric.pam

    give_feedback(msg = glue::glue("Setting default pam metric to '{metric.pam}'."),
                  verbose = verbose)

  }


  pam.obj@default <- default_list


  base::return(pam.obj)

}




# computation -------------------------------------------------------------

#' Title
#'
#' @param pam.obj
#' @param k
#' @param metric.pam
#' @param verbose
#' @param verbose.pb
#' @param ...
#'
#' @return
#' @export
#'
perform_pam_clustering <- function(pam.obj,
                                   k = NULL,
                                   metric.pam = NULL,
                                   verbose = TRUE,
                                   verbose.pb = TRUE,
                                   ...){

  assign_pam_default(pam.obj)

  # check k
  is_vec(x = k, mode = "numeric")

  if(base::any(k <= 1)){

    msg <- "Input for argument k must not contain values 0 or 1."

    give_feedback(msg = msg, fdb.fn = "stop")

  }

  # check metric
  check_one_of(input = metric.pam, against = valid_metrics_pam)

  n_metrics <- base::length(metric.pam)
  n_k <- base::length(k)

  # feedback

  n_total <- n_metrics * n_k

  give_feedback(msg = glue::glue("Iterating over {n_total} combinations of 'k' and 'metric.pam'."),
                verbose = verbose)

  if(base::isTRUE(verbose.pb)){

    pb <- create_progress_bar(total = n_total)

  }

  pam_data <- get_pam_data(pam.obj)

  for(metric in metric.pam){

    for(k_val in k){

      if(base::isTRUE(verbose.pb)){

        pb$tick()

      } else if(base::isTRUE(verbose)){

        msg <- glue::glue("Performing pam-algorithm with metric '{metric}' and k = {k_val}.")

        give_feedback(msg = msg, verbose = verbose)

      }

      res <-
        cluster::pam(
          x = pam_data,
          k = k_val,
          metric = metric,
          ...
        )

      pam_string <- stringr::str_c("k", k_val, sep = "_")

      pam.obj@results[[metric]][[pam_string]] <- res

    }


  }

  base::return(pam.obj)

}







# extraction --------------------------------------------------------------


#' Title
#'
#' @param pam.obj
#' @param k
#' @param metric.pam
#' @param verbose
#' @param sep
#' @param fdb.fn
#'
#' @return
#' @export
#'
get_medoids_df <- function(pam.obj,
                           k = NULL,
                           metric.pam = NULL,
                           verbose = TRUE,
                           cluster.prefix = "",
                           sep = " = ",
                           fdb.fn = "message"){

  assign_pam_default(pam.obj)

  check_pam_input(k = k, metric.pam = metric.pam, m.length = 1)

  medoids_df <-
    purrr::map_df(
      .x = k,
      .f = function(k){

        pam_res <- get_pam_obj(pam.obj, k = k, metric.pam = metric.pam, fdb.fn = fdb.fn)

        df <-
          base::as.data.frame(pam_res$medoids) %>%
          tibble::rownames_to_column(var = pam.obj@key_name) %>%
          dplyr::mutate(
            cluster_name = stringr::str_c("k", k, sep = sep) %>% base::as.factor(),
            cluster = dplyr::row_number() %>% stringr::str_c(cluster.prefix, .) %>% base::as.factor()
            ) %>%
          dplyr::select(cluster_name, cluster, dplyr::everything())

        base::return(df)

      }

    )

  if(base::length(k) == 1){

    medoids_df$cluster_name <- NULL

  }

  base::return(medoids_df)

}

#' Title
#'
#' @param pam.obj
#' @param return.tibble
#' @param as.dist
#'
#' @return
#' @export
#'
get_pam_data <- function(pam.obj, return.tibble = FALSE, as.dist = NULL){

  assign_pam_default(pam.obj)

  pam_data <- pam.obj@data

  if(base::isTRUE(as.dist)){

    pam_data <- stats::as.dist(pam_data)

  } else if(base::isTRUE(return.tibble)){

    pam_data <-
      tibble::as_tibble(pam_data) %>%
      dplyr::mutate(!!rlang::sym(pam.obj@key_name) := pam.obj@observations) %>%
      dplyr::select(!!rlang::sym(pam.obj@key_name), dplyr::everything())

  }

  base::return(pam_data)

}




#' Title
#'
#' @param pam.obj
#' @param metric.pam
#' @param k
#' @param with.data
#'
#' @return
#' @export
#'
get_pam_df <- function(pam.obj,
                       metric.pam = NULL,
                       k = NULL,
                       cluster.prefix = "",
                       full.name = TRUE,
                       sep = "_",
                       with.data = FALSE,
                       fdb.fn = "message"){

  assign_pam_default(pam.obj)

  if(base::isTRUE(with.data)){

    if(base::isTRUE(pam.obj@default$as.dist)){

      msg <- "Can not join cluster variables to a distance object."

      give_feedback(msg = msg, fdb.fn = "stop")

    } else {

      cluster_df <- get_pam_data(pam.obj, return.tibble = TRUE)

    }

  } else {

    cluster_df <-
      magrittr::set_colnames(
        x = data.frame(pam.obj@observations),
        value = pam.obj@key_name
      )

  }


  for(metric in metric.pam){

    for(k_val in k){

      pam_res <- get_pam_obj(pam.obj = pam.obj, k = k_val, metric.pam = metric.pam, fdb.fn = fdb.fn)

      if(!base::is.null(pam_res)){


        if(base::isTRUE(full.name)){

          cluster_name <- stringr::str_c("pam", metric, "k", k_val, sep = sep)

        } else {

          cluster_name <- stringr::str_c("k", k_val, sep = sep)

        }

        df_to_add <-
          base::as.data.frame(x = pam_res$clustering) %>%
          tibble::rownames_to_column(var = pam.obj@key_name) %>%
          magrittr::set_colnames(value = c(pam.obj@key_name, "cluster"))

        cluster_df <-
          dplyr::left_join(x = cluster_df, y = df_to_add, by = pam.obj@key_name) %>%
          dplyr::mutate(cluster = stringr::str_c(cluster.prefix, cluster, sep = "") %>% base::as.factor()) %>%
          dplyr::rename(!!cluster_name := cluster) %>%
          tibble::as_tibble()

      }

    }

  }

  base::return(cluster_df)

}


#' Title
#'
#' @param pam.obj
#' @param k
#' @param metric.pam
#' @param fdb.fn
#'
#' @return
#' @export
#'
get_pam_obj <- function(pam.obj, k = NULL, metric.pam = NULL, fdb.fn = "stop"){

  assign_pam_default(pam.obj)

  check_pam_input(k = k, metric.pam = metric.pam, k.length = 1, m.length = 1)

  pam_string <- stringr::str_c("k", k, sep = "_")

  res <- pam.obj@results[[metric.pam]][[pam_string]]

  res <- check_pam_availability(input = res, metric.pam = metric.pam, k = k, fdb.fn = fdb.fn)

  base::return(res)

}


#' Title
#'
#' @param pam.obj
#' @param k
#' @param metric.pam
#' @param m.length
#'
#' @return
#' @export
#'
get_pam_sil_df <- function(pam.obj, k = NULL, metric.pam = NULL, m.length = 1){

  assign_pam_default(pam.obj)

  check_pam_input(k = k, metric.pam = metric.pam, m.length = m.length)

  sil_df <-
    purrr::map_df(
      .x = k,
      function(k){

        pam_res <- get_pam_obj(pam.obj, k = k, metric.pam = metric.pam, fdb.fn = "message")

        if(!base::is.null(pam_res)){

          sil_df2 <-
            base::as.data.frame(pam_res$silinfo$widths) %>%
            dplyr::mutate(
              cluster = forcats::as_factor(x = cluster),
              cluster_name = stringr::str_c("k", k, sep = " = ") %>% base::as.factor(),
              x_axis = dplyr::row_number()
            ) %>%
            tibble::rownames_to_column(var = pam.obj@key_name) %>%
            dplyr::group_by(cluster)

        } else {

          sil_df2 <- NULL

        }

        base::return(sil_df2)

      }

    )

}







# plotting ----------------------------------------------------------------




#' Title
#'
#' @param pam.obj
#' @param k
#' @param metric.pam
#' @param cluster.prefix
#' @param cluster.subset
#' @param cluster.relevel
#' @param variables.subset
#' @param variables.relevel
#' @param display.medoid.name
#' @param sep
#' @param clr
#' @param clrp
#' @param verbose
#' @param ...
#'
#' @return
#' @export
#'
plot_medoid_barchart <- function(pam.obj,
                                 k = NULL,
                                 metric.pam = NULL,
                                 cluster.prefix = "",
                                 cluster.subset = NULL,
                                 cluster.relevel = TRUE,
                                 variables.subset = NULL,
                                 variables.relevel = TRUE,
                                 display.medoid.name = FALSE,
                                 sep = ": ",
                                 clr = "black",
                                 clrp = "milo",
                                 verbose = TRUE,
                                 ...){

  assign_pam_default(pam.obj)

  check_pam_input(k = k, k.length = 1, metric.pam = metric.pam, m.length = 1)

  medoids_df <-
    get_medoids_df(pam.obj, k = k, metric.pam  = metric.pam, cluster.prefix = cluster.prefix) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(pam.obj@variables),
      names_to = "variables",
      values_to = "values"
    ) %>%
    dplyr::mutate(
      variables = base::as.factor(variables)
    ) %>%
    check_across_subset(
      across = "variables",
      across.subset = variables.subset,
      relevel = variables.relevel,
    ) %>%
    check_across_subset(
      across = "cluster",
      across.subset = cluster.subset,
      relevel = cluster.relevel
    )

  if(base::isTRUE(display.medoid.name)){

    medoids_df <-
      dplyr::mutate(medoids_df,
                    cluster = stringr::str_c(cluster, !!rlang::sym(pam.obj@key_name), sep = sep)
      )

  }

  ggplot2::ggplot(data = medoids_df, mapping = ggplot2::aes(x = cluster, y = values)) +
    ggplot2::geom_col(mapping = ggplot2::aes(fill = cluster), color = clr) +
    ggplot2::facet_wrap(facets = . ~ variables, scales = "free_x") +
    theme_statistics() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = NULL, fill = "Cluster") +
    scale_color_add_on(aes = "fill", variable = medoids_df$cluster, clrp = clrp, ...)

}


#' Title
#'
#' @param pam.obj
#' @param k
#' @param metric.pam
#' @param cluster.subset
#' @param cluster.relevel
#' @param cluster.prefix
#' @param clr
#' @param clrp
#' @param ...
#'
#' @return
#' @export
#'
plot_pam_cluster_count <- function(pam.obj,
                                   k = NULL,
                                   metric.pam = NULL,
                                   cluster.subset = NULL,
                                   cluster.relevel = TRUE,
                                   cluster.prefix = "",
                                   clr = "black",
                                   clrp = "milo",
                                   ...){

  assign_pam_default(pam.obj)

  check_pam_input(k = k, metric.pam = metric.pam)

  cluster_df <-
    get_pam_df(pam.obj,
               metric.pam = metric.pam,
               k = k,
               cluster.prefix = cluster.prefix,
               full.name = FALSE,
               sep = " = ")

  clusters <-
    dplyr::select(cluster_df, -!!rlang::sym(pam.obj@key_name)) %>%
    base::colnames()

  shifted_df <-
    tidyr::pivot_longer(
      data = cluster_df,
      cols = dplyr::all_of(clusters),
      names_to = "clusters",
      values_to = "values"
    )


  ggplot2::ggplot(data = shifted_df, mapping = ggplot2::aes(x = values)) +
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = values), color = clr) +
    ggplot2::facet_wrap(facets = . ~ clusters, scales = "free_x") +
    scale_color_add_on(aes = "fill", variable = shifted_df$values, clrp = clrp, ...) +
    theme_statistics() +
    ggplot2::labs(x = NULL, y = "Count", fill = "Cluster")

}

#' Title
#'
#' @param pam.obj
#' @param metric.pam
#' @param k
#' @param clrp
#' @param ncol
#' @param nrow
#' @param verbose
#'
#' @return
#' @export
#'
plot_silhouette_widths <- function(pam.obj,
                                   metric.pam = NULL,
                                   k = NULL,
                                   clrp = "milo",
                                   ncol = NULL,
                                   nrow = NULL,
                                   verbose = TRUE){

  assign_pam_default(pam.obj)

  sil_df <- get_pam_sil_df(pam.obj, k = k, metric.pam = metric.pam)

  ggplot2::ggplot(data = sil_df, mapping = ggplot2::aes(x = x_axis, y = sil_width)) +
    ggplot2::geom_col(mapping = ggplot2::aes(color = cluster, fill = cluster)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_wrap(facets = ~ cluster_name, ncol = ncol, nrow = nrow) +
    scale_color_add_on(aes = "fill",  variable = "discrete", clrp = clrp) +
    scale_color_add_on(aes = "color", variable = "discrete", clrp = clrp, guide = FALSE) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "lightgrey"),
      axis.title.y = ggplot2::element_text(face = "bold", size = 12.5),
      legend.title = ggplot2::element_text(face = "bold", size = 12.5),
      legend.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = 16.5),
      plot.subtitle = ggplot2::element_text(size = 10)
    ) +
    ggplot2::labs(x = NULL, y = NULL, color = NULL, fill = "Cluster")

}


# miscellaneous -----------------------------------------------------------

#' Title
#'
#' @param pam.obj
#'
#' @return
#' @export
#'
assign_pam_default <- function(pam.obj){

  ce <- rlang::caller_env()

  default_args <- base::names(pam.obj@default)

  cfn <- rlang::caller_fn()

  # get arguments from calling function
  cargs <- rlang::fn_fmls_names(fn = cfn)

  cargs <- cargs[!cargs == "..."]

  # keep arguments from calling function
  default_args <- cargs[cargs %in% default_args]

  # assign default argument values if input was set to NULL
  for(arg in default_args){

    arg_value <-
      base::parse(text = arg) %>%
      base::eval(envir = ce)

    if(base::is.null(arg_value)){

      arg_value <- pam.obj@default[[arg]]

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

