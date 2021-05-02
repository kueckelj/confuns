
# s4 ----------------------------------------------------------------------


hclust_conv <- methods::setClass(Class = "hclust_conv",
                                 slots = c(
                                   data = "matrix",
                                   default = "list",
                                   dist_matrices = "list",
                                   hclust_results = "list",
                                   variables = "character",
                                   observations = "character",
                                   key_name = "character"
                                 ))



# r-objects ---------------------------------------------------------------

#' @export
valid_methods_dist <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

#' @export
valid_methods_aggl <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")



# input check -------------------------------------------------------------

#' Title
#'
#' @param h
#' @param k
#' @param only.one
#'
#' @return
#' @export
#'

check_h_k <- function(h = NULL, k = NULL, only.one = FALSE, skip.allow = TRUE){

  are_vectors(c("k", "h"), mode = "numeric", skip.allow = TRUE, skip.val = NULL)

  if(base::all(base::is.null(k), base::is.null(h)) & base::isFALSE(skip.allow)){

    msg <- "Please specify either argument 'k' or argument 'h'."

    give_feedback(msg = msg, fdb.fn = "stop")

  }

  if(base::isTRUE(only.one)){

    if(base::all(base::is.numeric(k), base::is.numeric(h))){

      msg <- "Please specify only one of argument 'k' or argument 'h'. Not both."

      give_feedback(msg = msg, fdb.fn = "stop")

    }

  }

}

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

check_hclust_availability <- function(input, method.dist, method.aggl, fdb.fn = "stop"){

  if(!shiny::isTruthy(input)){

    msg <- glue::glue("Could not find hclust results for distance method '{method.dist}' and agglomerative method '{method.aggl}'.")

    give_feedback(msg = msg, fdb.fn = fdb.fn)

  }

  base::invisible(TRUE)

}


#' Title
#'
#' @param method.dist
#' @param method.aggl
#' @param methods.dist
#' @param methods.aggl
#'
#' @return
#' @export
#'

check_hclust_methods <- function(method.dist = NULL, method.aggl = NULL, methods.dist = NULL, methods.aggl = NULL){

  if(is_value(x = method.dist, mode = "character", skip.allow = TRUE, skip.val = NULL)){

    check_one_of(
      input = method.dist,
      against = valid_methods_dist
    )

  }

  if(is_vec(x = methods.dist, mode = "character", skip.allow = TRUE, skip.val = NULL)){

    check_one_of(
      input = methods.dist,
      against = valid_methods_dist
    )

  }

  if(is_value(x = method.aggl, mode = "character", skip.allow = TRUE, skip.val = NULL)){

    check_one_of(
      input = method.aggl,
      against = valid_methods_aggl
    )

  }

  if(is_vec(x = methods.aggl, mode = "character", skip.allow = TRUE, skip.val = NULL)){

    check_one_of(
      input = methods.aggl,
      against = valid_methods_aggl
    )

  }


}





# initiation  -------------------------------------------------------------


#' Title
#'
#' @param hclust_data
#'
#' @return
#' @export
#'

initiate_hclust_object <- function(hclust_data = NULL,
                                   key_name = NULL,
                                   hclust.data = NULL,
                                   key.name = NULL,
                                   default.dist = "euclidean",
                                   default.aggl = "complete",
                                   default.dir = "conv-hcl-obj.RDS",
                                   verbose = TRUE){

  hcl.obj <- methods::new(Class = "hclust_conv")

  if(!base::is.null(hclust_data)){ warning("hclust_data is deprecatd. Use argument hclust.data ")}

  if(!base::is.null(key_name)){ warning("key_name is deprecatd. Use argument key.name ")}

  if(!base::is.null(hclust.data)){ hclust_data <- hclust.data }

  if(!base::is.null(key.name)){ key_name <- key.name }

  # set observations
  obs <- base::rownames(hclust_data)

  if(dplyr::n_distinct(obs) == base::nrow(hclust_data)){

    hcl.obj@observations <- obs

  } else {

    msg <- "Observations of input for argument 'hclust.data' must be named and the number of unique rownames must be equal to the number of rows."

    confuns::give_feedback(msg = msg, fdb.fn = "stop")

  }

  # set variables
  vars <- base::colnames(hclust_data)

  if(dplyr::n_distinct(vars) == base::ncol(hclust_data)){

    hcl.obj@variables <- vars

  }

  # set data
  numeric_check <- base::apply(X = hclust_data, MARGIN = 2, FUN = base::is.numeric)

  if(base::all(numeric_check)){

    hcl.obj@data <- base::as.matrix(hclust_data)

  } else {

    msg <- "All variables of input for argument 'hclust_data' must be numeric."

    give_feedback(msg = msg, fdb.fn = "stop")

  }

  # set key
  if(is_value(x = key_name, mode = "character")){

    hcl.obj@key_name <- key_name

  }

  # set defaults
  hcl.obj <-
    set_hclust_default(
      hcl.obj = hcl.obj,
      method.dist = default.dist,
      method.aggl = default.aggl,
      directory = default.dir,
      verbose = verbose
      )


  # return obj
  base::return(hcl.obj)


}





# set ---------------------------------------------------------------------

#' Title
#'
#' @param hcl.obj
#' @param method.aggl
#' @param method.dist
#'
#' @return
#' @export
#'

set_hclust_default <- function(hcl.obj, method.aggl = NA, method.dist = NA, directory = NA, verbose = FALSE){

  if(!base::is.na(method.dist)){

    is_value(x = method.dist, mode = "character")

    check_hclust_methods(method.dist = method.dist)

    hcl.obj@default[["method.dist"]] <- method.dist
    hcl.obj@default[["methods.dist"]] <- method.dist

  }

  if(!base::is.na(method.aggl)){

    is_value(x = method.aggl, mode = "character")

    check_hclust_methods(method.aggl = method.aggl)

    hcl.obj@default[["method.aggl"]] <- method.aggl
    hcl.obj@default[["methods.aggl"]] <- method.aggl

  }

  if(!base::is.na(directory)){

    is_value(x = directory, mode = "character")

    hcl.obj@default[["directory"]] <- directory

  }

  base::return(hcl.obj)


}

# computation  ------------------------------------------------------------

#' Title
#'
#' @param hcl.obj
#' @param methods.dist
#' @param p
#' @param verbose
#'
#' @return
#' @export
#'

compute_distance_matrices <- function(hcl.obj, methods.dist, p = 2, verbose = TRUE, force = FALSE){

  check_one_of(
    input = methods.dist,
    against = valid_methods_dist
  )


  # make sure not to calculate needlessly
  available_methods <- base::names(hcl.obj@dist_matrices)

  redundant_methods <- methods.dist[methods.dist %in% available_methods]

  if(base::length(redundant_methods) >= 1 & !base::isTRUE(force)){

    msg <- glue::glue("Skipping computation of distance {ref1} according to {ref2} '{ref_methods}' as {ref3} {ref4} already present. Set argument 'force' to TRUE in order to force the computation.",
                      ref1 = adapt_reference(redundant_methods, "matrix", "matrices"),
                      ref2 = adapt_reference(redundant_methods, "method", "methods"),
                      ref3 = adapt_reference(redundant_methods, "it", "they"),
                      ref4 = adapt_reference(redundant_methods, "is", "are"),
                      ref_methods = glue::glue_collapse(redundant_methods, sep = "', '", last = "' and '")
                      )

    give_feedback(msg = msg, verbose = verbose)

    methods.dist <- methods.dist[!methods.dist %in% redundant_methods]

  }

  if(base::length(methods.dist) == 0){

    msg <- "No distance methods remaining. Please specify distance methods according to which no distance matrix has been calculated yet or set argument 'force' to TRUE."

    give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)

  }


  # compute matrices in for loop
  n_methods <- base::length(methods.dist)

  dist_list <- base::vector(mode = "list", length = n_methods)

  pb <- create_progress_bar(total = n_methods)

  msg <- glue::glue("Computing distance {ref_matrix} according to {n_methods} {ref_method}.",
                    ref_matrix = adapt_reference(methods.dist, "matrx", "matrices"),
                    ref_method = adapt_reference(methods.dist, "method", "methods")
                    )

  give_feedback(msg = msg, verbose = verbose)

  data_mtr <- hcl.obj@data

  for(method in methods.dist){

    pb$tick()

    dist_list[[method]] <- stats::dist(x = data_mtr, method = method, p = p)

  }

  dist_list <- purrr::discard(dist_list, .p = base::is.null)

  # add to hcl-object
  current_dist_list <- hcl.obj@dist_matrices

  if(base::identical(x = current_dist_list, y = list())){

    hcl.obj@dist_matrices <- dist_list

  } else {

    final_dist_list <- join_lists(lst.1 = current_dist_list, lst.2 = dist_list)

    hcl.obj@dist_matrices <- final_dist_list

  }

  # return
  give_feedback(msg = "Done.", verbose = verbose)

  base::return(hcl.obj)

}


#' Title
#'
#' @param hcl.obj
#' @param methods.aggl
#' @param methods.dist
#'
#' @return
#' @export
#'

compute_hierarchical_cluster <- function(hcl.obj, methods.aggl = NULL, methods.dist = NULL, verbose = TRUE){


  assign_hclust_default(hcl.obj)

  # check input vality
  check_hclust_methods(methods.aggl = methods.aggl)

  if(base::is.character(methods.dist)){

    check_one_of(
      input = methods.dist,
      against = base::names(hcl.obj@dist_matrices)
    )

  } else {

    # calculate for all distance methods if set to NULL
    methods.dist <- base::names(hcl.obj@dist_matrices)

  }

  # compute hcluster
  n_methods_dist <- base::length(methods.dist)
  n_methods_aggl <- base::length(methods.aggl)

  list_methods_dist <- base::vector(mode = "list", length = n_methods_dist)

  for(method_dist in methods.dist){

    list_methods_aggl <- base::vector(mode = "list", length = n_methods_aggl)

    msg <-
      glue::glue("Iterating over {n_methods_aggl} agglomerative {ref_method} using distance matrix from slot '{method_dist}'.",
                 ref_method = adapt_reference(methods.dist, "method", "methods"))

    give_feedback(msg = msg, verbose = verbose)

    for(method_aggl in methods.aggl){

      give_feedback(msg = glue::glue("Agglomerative method: '{method_aggl}'."), verbose = verbose)

      # extract distance matrix
      dist_mtr <- hcl.obj@dist_matrices[[method_dist]]

      # do clustering
      results <- stats::hclust(d = dist_mtr, method = method_aggl)

      if(!shiny::isTruthy(x = results)){

        give_feedback(msg = "Failed.", verbose = verbose)

        list_methods_aggl[[method_aggl]] <- NULL

      } else {

        give_feedback(msg = "Successfull.", verbose = verbose)

        list_methods_aggl[[method_aggl]] <- results

      }

    }

    list_methods_dist[[method_dist]] <- purrr::discard(list_methods_aggl, .p = base::is.null)

  }

  list_methods_dist <- purrr::discard(list_methods_dist, .p = base::is.null)

  # add to hcl-object
  current_hclust_list <- hcl.obj@hclust_results

  if(base::identical(x = current_hclust_list, y = list())){

    hcl.obj@hclust_results <- list_methods_dist

  } else {

    calculated_methods <- base::names(list_methods_dist)

    initiated_methods <- base::names(current_hclust_list)

    # join lists method by method
    for(calculated_method in calculated_methods){

      # if slot already exists: join new results
      if(calculated_method %in% initiated_methods){

        current_hclust_list[[calculated_method]] <-
          join_lists(
            lst.1 = current_hclust_list[[calculated_method]],
            lst.2 = list_methods_dist[[calculated_method]]
          )

        # if slot does not exist yet: add
      } else {

        current_hclust_list[[calculated_method]] <- list_methods_dist[[calculated_method]]

      }

    }

    hcl.obj@hclust_results <- current_hclust_list

  }

  # return
  give_feedback(msg = "Done.", verbose = verbose)

  base::return(hcl.obj)

}


# extraction --------------------------------------------------------------


#' Title
#'
#' @param hcl.obj
#'
#' @return
#' @export
#'

get_hclust_data <- function(hcl.obj, return.tibble = TRUE){

  if(base::isTRUE(return.tibble)){

    hclust_data <-
      tibble::as_tibble(hcl.obj@data) %>%
      dplyr::mutate(!!rlang::sym(hcl.obj@key_name) := hcl.obj@observations) %>%
      dplyr::select(!!rlang::sym(hcl.obj@key_name), dplyr::everything())

  } else {

    hclust_data <- hcl.obj@data

  }

  base::return(hclust_data)

}


#' Title
#'
#' @param hcl.obj
#' @param method.dist
#' @param method.aggl
#'
#' @return
#' @export
#'

get_hclust_obj <- function(hcl.obj,
                           method.dist = NULL,
                           method.aggl = NULL,
                           fdb.fn = "stop"){

  assign_hclust_default(hcl.obj)

  check_hclust_methods(method.dist = method.dist, method.aggl = method.aggl)

  hclust_res <- hcl.obj@hclust_results[[method.dist]][[method.aggl]]

  check_hclust_availability(
    input = hclust_res,
    method.dist = method.dist,
    method.aggl = method.aggl,
    fdb.fn = fdb.fn
  )

  base::return(hclust_res)

}


#' Title
#'
#' @param hcl.obj
#' @param methods.dist
#' @param methods.aggl
#' @param k
#' @param h
#'
#' @return
#' @export
#'

get_hclust_df <- function(hcl.obj,
                          methods.dist = NULL,
                          methods.aggl = NULL,
                          k = NULL,
                          h = NULL,
                          cluster.prefix = "",
                          with.data = FALSE,
                          verbose = TRUE){

  assign_hclust_default(hcl.obj)

  check_hclust_methods(methods.dist = methods.dist, methods.aggl = methods.aggl)

  check_h_k(k = k, h = h, only.one = FALSE, skip.allow = FALSE)

  if(base::isTRUE(with.data)){

    cluster_df <- get_hclust_data(hcl.obj = hcl.obj)

  } else {

    obs <- hcl.obj@observations

    cluster_df <-
      tibble::tibble(!!rlang::sym(hcl.obj@key_name) := obs)

  }


  # extracting cluster variables in for loop
  for(method_dist in methods.dist){

    for(method_aggl in methods.aggl){

      hclust_res <-
        get_hclust_obj(hcl.obj = hcl.obj,
                       method.dist = method_dist,
                       method.aggl = method_aggl,
                       fdb.fn = "warning")

      if(!base::is.null(hclust_res)){

        # add variables for k input
        if(base::is.numeric(k)){

          for(k_val in k){

            var_name <- stringr::str_c("hcl", method_dist, method_aggl, "k", k_val, sep = "_")

            cluster_var <-
              stats::cutree(tree = hclust_res, k = k_val) %>%
              base::as.data.frame() %>%
              magrittr::set_colnames(value = var_name) %>%
              tibble::rownames_to_column(var = hcl.obj@key_name) %>%
              dplyr::mutate(
                !!rlang::sym(var_name) := stringr::str_c(cluster.prefix, !!rlang::sym(var_name))
              )

            # join with cluster df
            cluster_df <- dplyr::left_join(x = cluster_df, y = cluster_var, by = hcl.obj@key_name)

          }

        }


        # add variables for h input
        if(base::is.numeric(h)){

          for(h_val in h){

            var_name <- stringr::str_c("hcl", method_dist, method_aggl, "h", h_val, sep = "_")

            cluster_var <-
              stats::cutree(tree = hclust_res, h = h) %>%
              base::as.data.frame() %>%
              magrittr::set_colnames(value = var_name) %>%
              tibble::rownames_to_column(var = hcl.obj@key_name) %>%
              dplyr::mutate(
                !!rlang::sym(var_name) := stringr::str_c(cluster.prefix, !!rlang::sym(var_name))
              )
            # join with cluster df
            cluster_df <- dplyr::left_join(x = cluster_df, y = cluster_var, by = hcl.obj@key_name)

          }

        }

      }

    }

  }

  # convert to factors
  vars <- hcl.obj@variables
  key <- hcl.obj@key_name

  not_cluster <- c(vars, key)

  cluster <- base::colnames(cluster_df)[!base::colnames(cluster_df) %in% not_cluster]

  cluster_df <- dplyr::mutate(cluster_df, dplyr::across(.cols = dplyr::all_of(cluster), .fns = base::factor))

  cluster_df <- dplyr::rename_with(cluster_df, .fn = ~ stringr::str_replace(string = .x, pattern = "-", replacement = "."),
                                   .cols = dplyr::all_of(cluster))

  base::return(cluster_df)

}


#' Title
#'
#' @param hc
#' @param k
#' @param h
#'
#' @return
#' @export
#'

get_dendro_data <- function(hcl.obj,
                            method.dist = NULL,
                            method.aggl =NULL,
                            k = NULL,
                            h = NULL,
                            type = "rectangle") {

  assign_hclust_default(hcl.obj)

  are_values(c("k", "h"), mode = "numeric", skip.allow = TRUE, skip.val = NULL)

  check_h_k(k = k, h = h, only.one = TRUE)

  check_hclust_methods(method.dist = method.dist, method.aggl = method.aggl)

  hc <-
    get_hclust_obj(hcl.obj = hcl.obj,
                   method.dist = method.dist,
                   method.aggl = method.aggl)

  hcdata    <-  ggdendro::dendro_data(hc, type = type)
  seg       <-  hcdata$segments
  labclust  <-  stats::cutree(hc, k = k, h = h)[hc$order]

  if(base::is.null(k) & base::is.numeric(h)){

    k <- base::max(base::as.numeric(labclust))

  }

  segclust  <-  base::rep(0L, base::nrow(seg))
  heights   <-  base::sort(hc$height, decreasing = TRUE)
  height    <-  base::mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)

  for (i in 1:k) {
    xi      <-  hcdata$labels$x[labclust == i]
    idx1    <-  seg$x    >= base::min(xi) & seg$x    <= base::max(xi)
    idx2    <-  seg$xend >= base::min(xi) & seg$xend <= base::max(xi)
    idx3    <-  seg$yend < height
    idx     <-  idx1 & idx2 & idx3
    segclust[idx] <- i
  }

  idx                    <-  base::which(segclust == 0L)
  segclust[idx]          <-  segclust[idx + 1L]
  hcdata$segments$clust  <-  segclust %>% base::as.factor()
  hcdata$segments$line   <-  base::as.integer(segclust < 1L) %>% base::as.factor()
  hcdata$labels$clust    <-  labclust %>% base::as.factor()

  base::return(hcdata)

}



# plotting ----------------------------------------------------------------

#' Title
#'
#' @param nbLabels
#' @param direction
#' @param fan
#'
#' @return
#' @export
#'

define_label_params <- function(nbLabels,
                                labels.angle = 0,
                                labels.hjust = 0,
                                direction = c("tb", "bt", "lr", "rl"),
                                fan       = FALSE) {
  if (base::isTRUE(fan)){

    angle       <-  360 / nbLabels * 1:nbLabels + 90
    idx         <-  angle >= 90 & angle <= 270
    angle[idx]  <-  angle[idx] + 180
    hjust       <-  base::rep(0, nbLabels)
    hjust[idx]  <-  1

  } else {

    angle       <-  base::rep(labels.angle, nbLabels)
    hjust       <-  labels.hjust
    if (direction %in% c("tb", "rl")) { hjust <- 1 }
  }

  res_list <- list(angle = angle, hjust = hjust)

  base::return(res_list)

}


#' Title
#'
#' @param hcl.obj
#' @param method.dist
#' @param method.aggl
#' @param display.title
#' @param ...
#'
#' @return
#' @export
#'

plot_dendrogram <- function(hcl.obj,
                            method.dist = NULL,
                            method.aggl = NULL,
                            k = NULL,
                            h = NULL,
                            type = "rectangle",
                            direction = "bt",
                            branch.size = 1,
                            display.labels = FALSE,
                            labels.angle = 90,
                            labels.hjust = 0,
                            labels.nudge = 0.01,
                            labels.size = 3,
                            labels.vjust = 0.5,
                            display.legend = TRUE,
                            display.title = FALSE,
                            clrp = "milo",
                            clrp.adjust = NULL,
                            ...){

  assign_hclust_default(hcl.obj)

  # input check
  check_one_of(input = direction, against = c("bt", "lr"))

  # plotting
  if(base::all(base::is.null(k), base::is.null(h))){

    hclust_res <- get_hclust_obj(hcl.obj = hcl.obj, method.dist = method.dist, method.aggl = method.aggl)

    # most basic dendro plot
    dendro_plot <- ggdendro::ggdendrogram(data = hclust_res, labels = display.labels, ...)

  } else {

    check_h_k(h = h, k = k, only.one = TRUE)

    dendro_data <- get_dendro_data(hcl.obj = hcl.obj,
                                   method.dist = method.dist,
                                   method.aggl = method.aggl,
                                   h = h,
                                   k = k,
                                   type = type)

    segment_df <- ggdendro::segment(dendro_data)

    # basic parameters
    ybreaks   <- base::pretty(segment_df$y, n = 5)
    ymin      <- base::min(segment_df$y)

    cluster_levels <- segment_df$clust %>% base::levels()

    forced_adjustment <- "black"
    base::names(forced_adjustment) <- cluster_levels[1]

    clrp.adjust <- c(clrp.adjust, forced_adjustment)
    breaks <- cluster_levels[2:base::length(cluster_levels)]

    # basic dendro plot
    dendro_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_segment(
        data = segment_df,
        mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend, color = clust),
        lineend = "round",
        size = branch.size,
        show.legend = TRUE
      ) +
      ggplot2::scale_x_continuous(breaks = NULL) +
      scale_color_add_on(variable = segment_df$clust, clrp = clrp, clrp.adjust = clrp.adjust, breaks = breaks, ...) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = NULL, y = NULL, color = "Cluster")

    # flip coordinates if desired
    if(direction == "lr"){

      dendro_plot <-
        dendro_plot + ggplot2::coord_flip()

    }

    # add labels
    if(base::isTRUE(display.labels)){

      label_params <-
        define_label_params(nbLabels = base::nrow(dendro_data$labels),
                            labels.angle = labels.angle,
                            labels.hjust = labels.hjust,
                            direction = direction,
                            fan = FALSE)

      dendro_data$labels$angle <- label_params$angle
      dendro_data$labels$y <- dendro_data$labels$y + labels.vjust

      dendro_plot <-
        dendro_plot +
        ggplot2::geom_text(
          data = ggdendro::label(dendro_data),
          mapping = ggplot2::aes(x = x, y = y, label = label, color = clust, angle = angle),
          hjust = label_params$hjust,
          nudge_y = labels.nudge,
          size = labels.size
        )

    }

  }

  # add title
  if(base::isTRUE(display.title)){

    dendro_plot <-
      dendro_plot +
      ggplot2::labs(
        title = stringr::str_c("Method.dist: ", method.dist),
        subtitle = stringr::str_c("Method.aggl: ", method.aggl)
      )

  }

  # remove legend
  if(base::isFALSE(display.legend)){

    dendro_plot <-
      dendro_plot + ggplot2::theme(legend.position = "none")

  }


  base::return(dendro_plot)

}


#' @rdname plot_dendrogram
#' @export
plot_dendrograms <- function(hcl.obj,
                             methods.dist = NULL,
                             methods.aggl = NULL,
                             k = NULL,
                             h = NULL,
                             type = "rectangle",
                             direction = "bt",
                             branch.size = 1,
                             display.labels = FALSE,
                             labels.angle = 90,
                             labels.hjust = 0,
                             labels.nudge = 0.01,
                             labels.size = 3,
                             labels.vjust = 0.5,
                             display.legend = FALSE,
                             display.title = TRUE,
                             clrp = "milo",
                             nrow = NULL,
                             ncol = NULL,
                             verbose = TRUE,
                             return.grid = FALSE,
                             ...){

  check_h_k(h = h, k = k, only.one = TRUE)

  instruction_df <-
    tidyr::expand_grid(
      method.dist = methods.dist,
      method.aggl = methods.aggl
    ) %>%
    dplyr::mutate(
      type = {{type}},
      direction = {{direction}},
      branch.size = {{branch.size}},
      display.labels = {{display.labels}},
      labels.angle = {{labels.angle}},
      labels.hjust = {{labels.hjust}},
      labels.size = {{labels.size}},
      labels.vjust = {{labels.vjust}},
      display.legend = {{display.legend}},
      display.title = {{display.title}},
      clrp = {{clrp}}
    )

  n_plots <- base::nrow(instruction_df)

  plot_list <- base::vector(mode = "list", length = n_plots)

  for(i in 1:n_plots){

    msg <- glue::glue("Creating dendrogram {i}/{n_plots}.")

    confuns::give_feedback(msg = msg, verbose = verbose)

    plot_dendrogram <- base::as.list(instruction_df[i,])

    plot_list[[i]] <-
      call_flexibly(fn = "plot_dendrogram",
                    fn.ns = "confuns",
                    default = list(hcl.obj = hcl.obj, h = h, k = k, ...),
                    verbose = verbose)

  }

  resulting_grid <- gridExtra::grid.arrange(grobs = plot_list, nrow = nrow, ncol = ncol)

  base::plot(resulting_grid)

  if(base::isTRUE(return.grid)){

    base::return(resulting_grid)

  } else {

    base::invisible(TRUE)

  }

}



# miscellaneous -----------------------------------------------------------


#' Title
#'
#' @param hcl.obj
#'
#' @return
#' @export
#'
assign_hclust_default <- function(hcl.obj){

  ce <- rlang::caller_env()

  default_args <- base::names(hcl.obj@default)

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

      arg_value <- hcl.obj@default[[arg]]

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





# printing

#' Title
#'
#' @param hcl.obj
#' @param key_name
#'
#' @return
#' @export
#'
print_hclust_overview <- function(hcl.obj, key_name = TRUE){

  fdb_list <- list()

  # key
  if(base::isTRUE(key_name)){ fdb_list[["Key name"]] <- hcl.obj@key_name }


  # distance matrices
  dist_matrices <- base::names(hcl.obj@dist_matrices)

  if(base::length(dist_matrices) == 0){

    fdb_list[["Calculated distance matrices"]] <- "none"

  } else {

    fdb_list[["Calculated distance matrices"]] <-
      glue::glue_collapse(dist_matrices, sep = "', '", last = "' and '") %>%
      base::as.character()

  }

  base::print(x = glue_list_report(fdb_list, separator = ": "))

  # hierarchical clustering

  if(base::length(dist_matrices) == 0){

    base::print(glue::glue("No hierarchical clustering has been conducted yet."))

  } else {

    hclust_fdb <-
      purrr::map(.x = dist_matrices,
                 .f = function(dist_mtr){

                   hclust_res <- base::names(hcl.obj@hclust_results[[dist_mtr]])

                   if(base::length(hclust_res) == 0){

                     res <- "none"

                   } else {

                     res <-
                       glue::glue_collapse(hclust_res, sep = ", ", last = " and ") %>%
                       base::as.character()

                   }

                   base::return(res)

                 }) %>%
      purrr::set_names(nm = dist_matrices)

    base::print(glue::glue("\nHierarchical clustering conducted with distance matrix:"))

    base::print(glue_list_report(hclust_fdb, prefix = "- '", separator = "' with agglomerative methods: "))

  }

}
