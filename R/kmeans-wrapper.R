


# s4 ----------------------------------------------------------------------

kmeans_conv <- methods::setClass(Class = "kmeans_conv",
                                 slots = c(
                                   data = "matrix",
                                   default = "list",
                                   results = "list",
                                   variables = "character",
                                   observations = "character",
                                   key_name = "character"
                                 ))




# r-objects ---------------------------------------------------------------

#' @export
valid_methods_kmeans <- c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")





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
check_kmeans_availability <- function(input, method.kmeans, centers, fdb.fn = "stop"){

  if(!shiny::isTruthy(input)){

    msg <- glue::glue("Could not find kmeans results for kmeans method '{method.kmeans}' and {centers} centers.")

    give_feedback(msg = msg, fdb.fn = fdb.fn)

    input <- NULL

  }

  base::invisible(input)

}


# initation ---------------------------------------------------------------

#' Title
#'
#' @param kmeans.data
#' @param key.name
#' @param default.dir
#' @param default.method.kmeans
#' @param default.centers
#'
#' @return
#' @export
#'
initiate_kmeans_object <- function(kmeans.data,
                                   key.name,
                                   default.method.kmeans = "Hartigan-Wong",
                                   default.centers = 2,
                                   default.dir = "conv-kmeans-obj.RDS",
                                   verbose = TRUE){

  kmeans.obj <- methods::new(Class = "kmeans_conv")

  # set observations
  obs <- base::rownames(kmeans.data)

  if(dplyr::n_distinct(obs) == base::nrow(kmeans.data)){

    kmeans.obj@observations <- obs

  } else {

    msg <- "Observations of input for argument 'kmeans.data' must be named and the number of unique rownames must be equal to the number of rows."

    confuns::give_feedback(msg = msg, fdb.fn = "stop")

  }


  # set variables
  vars <- base::colnames(kmeans.data)

  if(dplyr::n_distinct(vars) == base::ncol(kmeans.data)){

    kmeans.obj@variables <- vars

  }

  # set data
  numeric_check <- base::apply(X = kmeans.data, MARGIN = 2, FUN = base::is.numeric)

  if(base::all(numeric_check)){

    kmeans.obj@data <- base::as.matrix(kmeans.data)

  } else {

    msg <- "All variables of input for argument 'kmeans.data' must be numeric."

    give_feedback(msg = msg, fdb.fn = "stop")

  }

  # set key
  if(is_value(x = key.name, mode = "character")){

    kmeans.obj@key_name <- key.name

  }

  # set default
  kmeans.obj <-
    set_kmeans_default(
      kmeans.obj = kmeans.obj,
      method.kmeans = default.method.kmeans,
      centers = default.centers,
      directory = default.dir,
      verbose = verbose
    )

  # return obj
  base::return(kmeans.obj)

}


# set ---------------------------------------------------------------------


#' Title
#'
#' @param kmeans.obj
#' @param method.kmeans
#' @param centers
#' @param directory
#'
#' @return
#' @export
#'
set_kmeans_default <- function(kmeans.obj, method.kmeans = NA, centers = NA, directory = NA, verbose = FALSE){

  default_list <- kmeans.obj@default

  if(!base::is.na(method.kmeans)){

    check_one_of(input = method.kmeans, against = valid_methods_kmeans)

    default_list$method.kmeans <- method.kmeans
    default_list$methods.kmeans <- method.kmeans

    give_feedback(msg = glue::glue("Setting default kmeans algorithm to '{method.kmeans}'."), verbose = verbose)

  }

  if(!base::is.na(centers)){

    if(is_value(x = centers, mode = "numeric") & centers > 1){

      default_list$centers <- centers

      give_feedback(msg = glue::glue("Setting default number of centers to {centers}."), verbose = verbose)

    } else {

      msg <- "Input for argument 'centers' must be a numeric value bigger than 1."

      give_feedback(msg = msg, fdb.fn = "stop", verbose = verbose)

    }

  }

  if(!base::is.na(directory)){

    default_list$directory <- directory

    give_feedback(msg = glue::glue("Setting default directory to '{directory}'."), verbose = verbose)


  }

  kmeans.obj@default <- default_list

  base::return(kmeans.obj)

}








# computation  ------------------------------------------------------------

#' Title
#'
#' @param kmeans.obj
#' @param centers
#' @param methods.kmeans
#' @param verbose
#' @param ...
#'
#' @return
#' @export
#'
perform_kmeans_clustering <- function(kmeans.obj,
                                      centers = NULL,
                                      methods.kmeans = NULL,
                                      verbose = TRUE,
                                      verbose.pb = TRUE,
                                      ...){

  assign_kmeans_default(kmeans.obj)

  is_vec(x = centers, mode = "numeric")

  check_one_of(input = methods.kmeans, against = valid_methods_kmeans)

  n_methods <- base::length(methods.kmeans)
  n_centers <- base::length(centers)


  if(n_centers > 1){

    msg <- glue::glue("Iterating over {n_methods} {ref_method} and {n_centers} input {ref_centers} for argument 'center'.",
                      ref_method = adapt_reference(methods.kmeans, sg = "method", pl = "methods"),
                      ref_centers = adapt_reference(centers, sg = "option", pl = "options"))

  }

  give_feedback(msg = msg, verbose = verbose)

  verbose_input <- verbose

  if(base::isTRUE(verbose.pb)){

    pb <- create_progress_bar(total = n_methods)

    verbose <- FALSE

  }

  for(method in methods.kmeans){

    if(base::isTRUE(verbose.pb)){ pb$tick() }

    msg <-
      glue::glue("Iterating over {n_centers} different input options for argument 'centers' with method '{method}'")

    give_feedback(msg = msg, verbose = verbose)

    for(k in centers){

      msg = glue::glue("Input for argument 'centers' = {k}.")

      give_feedback(msg = msg, verbose = verbose)

      res <- stats::kmeans(x = kmeans.obj@data, centers = k, ...)

      if(shiny::isTruthy(res)){

        slot_name <- stringr::str_c("k", k, sep = "_")

        kmeans.obj@results[[method]][[slot_name]] <- res

        give_feedback(msg = "Successful.", verbose = verbose)

      } else {

        give_feedback(msg = "Failed.", verbose = verbose)

      }

    }

  }

  give_feedback(msg = "Done.", verbose = verbose_input)

  base::return(kmeans.obj)

}





# extraction --------------------------------------------------------------

#' Title
#'
#' @param kmeans.obj
#' @param return.tibble
#'
#' @return
#' @export
#'
get_kmeans_data <- function(kmeans.obj, return.tibble = TRUE){

  if(base::isTRUE(return.tibble)){

    kmeans_data <-
      tibble::as_tibble(kmeans.obj@data) %>%
      dplyr::mutate(!!rlang::sym(kmeans.obj@key_name) := kmeans.obj@observations) %>%
      dplyr::select(!!rlang::sym(kmeans.obj@key_name), dplyr::everything())

  } else {

    kmeans_data <- kmeans.obj@data

  }

  base::return(kmeans_data)

}



#' Title
#'
#' @param kmeans.obj
#' @param method.kmeans
#' @param centers
#' @param fdb.fn
#'
#' @return
#' @export
#'
get_kmeans_obj <- function(kmeans.obj,
                           method.kmeans = NULL,
                           centers = NULL,
                           fdb.fn = "stop"){

  assign_kmeans_default(kmeans.obj)

  slot_string <- stringr::str_c("k", centers, sep = "_")

  res <- kmeans.obj@results[[method.kmeans]][[slot_string]]

  res <- check_kmeans_availability(input = res, method.kmeans = method.kmeans, centers = centers)

  base::return(res)

}


#' Title
#'
#' @param kmeans.obj
#' @param methods.kmeans
#' @param centers
#' @param fdb.fn
#'
#' @return
#' @export
#'
get_kmeans_df <- function(kmeans.obj,
                          methods.kmeans = NULL,
                          centers = NULL,
                          centers.string = "centers",
                          cluster.prefix = "",
                          with.data = FALSE,
                          fdb.fn = "message"){

  assign_kmeans_default(kmeans.obj)

  check_one_of(input = methods.kmeans, against = valid_methods_kmeans)

  is_vec(x = centers, mode = "numeric")

  if(base::isTRUE(with.data)){

    cluster_df <- get_kmeans_data(kmeans.obj = kmeans.obj, return.tibble = TRUE)

  } else {

    cluster_df <-
      magrittr::set_colnames(
        x = data.frame(kmeans.obj@observations),
        value = kmeans.obj@key_name
      ) %>%
      tibble::as_tibble()

  }

  all_cluster_names <- base::vector(mode = "character", length = 0)

  for(method in methods.kmeans){

    for(k in centers){

      kmeans_res <- get_kmeans_obj(kmeans.obj = kmeans.obj,
                                   method.kmeans = method,
                                   centers = k,
                                   fdb.fn = fdb.fn)

      if(base::is.null(kmeans_res)){

        # skip

      } else {

        cluster_name <- stringr::str_c("kmeans", method, centers.string, k, sep = "_")

        all_cluster_names <- c(cluster_name, all_cluster_names)

        df_to_add <-
          base::as.data.frame(kmeans_res$cluster) %>%
          tibble::rownames_to_column(var = kmeans.obj@key_name) %>%
          magrittr::set_colnames(value = c(kmeans.obj@key_name, "cluster"))

        cluster_df <-
          dplyr::left_join(x = cluster_df, y = df_to_add, by = kmeans.obj@key_name) %>%
          dplyr::mutate(cluster = stringr::str_c(cluster.prefix, cluster, sep = "") %>% base::as.factor()) %>%
          dplyr::rename(!!cluster_name := cluster)

      }

    }

  }

  cluster_df <- dplyr::rename_with(cluster_df,
                                   .fn = ~ stringr::str_replace_all(string = .x, pattern = "-", replacement = "."),
                                   .cols = dplyr::all_of(all_cluster_names))


  base::return(cluster_df)

}




# plotting ----------------------------------------------------------------

#' Title
#'
#' @param kmeans.obj
#' @param methods.kmeans
#' @param clr
#' @param display.cols
#' @param display.line
#' @param display.points
#'
#' @return
#' @export
#'
plot_screeplot <- function(kmeans.obj,
                           methods.kmeans = NULL,
                           clr = "steelblue",
                           display.cols = TRUE,
                           display.line = TRUE,
                           display.points = TRUE){

  assign_kmeans_default(kmeans.obj)

  check_one_of(input = methods.kmeans, against = valid_methods_kmeans)

  calculated_methods <- base::names(kmeans.obj@results)

  methods.kmeans <- methods.kmeans[methods.kmeans %in% calculated_methods]

  res_list <- kmeans.obj@results[methods.kmeans]

  res_df <-
    purrr::imap_dfr(
      .x = res_list,
      .f = function(method_list, method){

        res_df2 <-
          purrr::imap_dfr(.x = method_list,
                          method_kmeans = method,
                          .f = function(res, k_string, method_kmeans){

                            res_df3 <-
                              data.frame(
                                method = method_kmeans,
                                k = stringr::str_remove(k_string, pattern = "^k_") %>% base::as.numeric(),
                                tot_withinss = res$tot.withinss
                              )

                            base::return(res_df3)

                          })

      }
    ) %>%
    dplyr::group_by(method) %>%
    dplyr::mutate(k = base::as.factor(k))


  # create basic plot
  p <-
    ggplot2::ggplot(data = res_df, mapping = ggplot2::aes(x = k, y = tot_withinss)) +
    ggplot2::facet_wrap(facets = ~ method) +
    ggplot2::labs(y = NULL, x = "Centers (k)") +
    theme_statistics()


  # add layer
  if(base::isTRUE(display.cols)){

    p <- p + ggplot2::geom_col(color = "black", fill = clr)

  }

  if(base::isTRUE(display.points)){

    p <- p + ggplot2::geom_point(color = "black")

  }

  if(base::isTRUE(display.line)){

    p <- p + ggplot2::geom_line(color = "black", mapping = ggplot2::aes(group = 1))

  }

  # return plot
  base::return(p)

}





# miscellaneous -----------------------------------------------------------

#' Title
#'
#' @param kmeans.obj
#'
#' @return
#' @export
#'
assign_kmeans_default <- function(kmeans.obj){

  ce <- rlang::caller_env()

  default_args <- base::names(kmeans.obj@default)

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

      arg_value <- kmeans.obj@default[[arg]]

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

