#' @include S4-Clustering.R



# S4-classes --------------------------------------------------------------

ClusteringHclust <- setClass(Class = "ClusteringHclust",
                             slots = list(
                               dist_matrices = "list"
                             ),
                             contains = "ClusteringMethod"
)

# r-objects ---------------------------------------------------------------

#' @export
valid_methods_aggl <-
  c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid") %>%
  base::sort()

#' @export
valid_methods_dist <-
  c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski") %>%
  base::sort()


# -----



# functions ---------------------------------------------------------------


agglomerate_hierarchical_trees <- function(dist.matrices,
                                           methods.aggl = "ward.D",
                                           verbose = TRUE,
                                           ...){

  dist.matrices <- purrr::keep(.x = dist.matrices, .p = ~ "dist" %in% base::class(.x))

  methods.dist <- base::names(dist.matrices)

  n_methods_dist <- base::length(methods.dist)
  n_methods_aggl <- base::length(methods.aggl)

  list_methods_dist <- list()

  for(method_dist in methods.dist){

    list_methods_aggl <- list()

    msg <-
      glue::glue(
        "Iterating over {n_methods_aggl} agglomerative {ref_method} using distance matrix '{method_dist}'.",
        ref_method = adapt_reference(methods.dist, "method", "methods")
      )

    give_feedback(msg = msg, verbose = verbose)

    for(method_aggl in methods.aggl){

      give_feedback(msg = glue::glue("Agglomerative method: '{method_aggl}'."), verbose = verbose)

      # extract distance matrix
      dist_mtr <- dist.matrices[[method_dist]]

      # do clustering
      results <- stats::hclust(d = dist_mtr, method = method_aggl, ...)

      if(!shiny::isTruthy(x = results)){

        list_methods_aggl[[method_aggl]] <- NULL

        give_feedback(
          msg = glue::glue("Agglomeration failed for method '{method_aggl}' with distance matrix '{method_dist}'."),
          verbose = TRUE,
          fdb.fn = "warning"
          )

      } else {

        list_methods_aggl[[method_aggl]] <- results

      }

    }

    list_methods_dist[[method_dist]] <- purrr::discard(list_methods_aggl, .p = base::is.null)

  }

  list_methods_dist <- purrr::discard(list_methods_dist, .p = base::is.null)

  return(list_methods_dist)

}

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

compute_dist_matrices <- function(data,
                                  methods.dist,
                                  p = 2,
                                  verbose = TRUE){

  check_one_of(
    input = methods.dist,
    against = validMethodsDist()
  )

  # compute matrices in for loop
  n_methods <- base::length(methods.dist)

  out <- list()

  pb <- create_progress_bar(total = n_methods)

  msg <- glue::glue(
    "Computing distance {ref_matrix} according to {n_methods} {ref_method}.",
    ref_matrix = adapt_reference(methods.dist, "matrx", "matrices"),
    ref_method = adapt_reference(methods.dist, "method", "methods")
  )

  give_feedback(msg = msg, verbose = verbose)

  for(method in methods.dist){

    if(base::isTRUE(verbose)){

      pb$tick()

    }

    out[[method]] <- stats::dist(x = data, method = method, p = p)

  }

  out <- purrr::discard(out, .p = base::is.null)

  give_feedback(msg = "Done.", verbose = verbose)

  return(out)

}

define_label_params <- function(nbLabels,
                                labels.angle = 0,
                                labels.hjust = 0,
                                direction = c("tb", "bt", "lr", "rl"),
                                fan = FALSE) {
  if(base::isTRUE(fan)){

    angle <- 360 / nbLabels * 1:nbLabels + 90
    idx <- angle >= 90 & angle <= 270
    angle[idx] <- angle[idx] + 180
    hjust <- base::rep(0, nbLabels)
    hjust[idx] <- 1

  } else {

    angle <- base::rep(labels.angle, nbLabels)
    hjust <- labels.hjust

    if (direction %in% c("tb", "rl")){ hjust <- 1 }

  }

  res_list <- list(angle = angle, hjust = hjust)

  return(res_list)

}


#' @rdname validInput
#' @export
validMethodsAggl <- function(){

  return(valid_methods_aggl)

}

#' @rdname validInput
#' @export
validMethodsDist <- function(){

  return(valid_methods_dist)

}

# -----






# methods for external generics -------------------------------------------

#' @rdname getDistMtr
#' @export
setMethod(
  f = "getDistMtr",
  signature = "ClusteringHclust",
  definition = function(object,
                        method_dist = "euclidean",
                        stop_if_null = FALSE){

    check_one_of(
      input = method_dist,
      against = validMethodsDist()
    )

    out <- object@dist_matrices[[method_dist]]

    if(base::is.null(out & base::isTRUE(stop_if_null))){

      stop(
        glue::glue(
          "No distance matrix found for method '{method_dist}'."
        )
      )

    }

    return(out)

  }
)

#' @rdname getHclust
#' @export
setMethod(
  f = "getHclust",
  signature = "ClusteringHclust",
  definition = function(object,
                        method_dist = "euclidean",
                        method_aggl = "ward.D",
                        stop_if_null = TRUE){

    check_one_of(
      input = method_dist,
      against = validMethodsDist()
    )

    check_one_of(
      input = method_aggl,
      against = validMethodsAggl()
    )

    out <- object@results[[method_dist]][[method_aggl]]

    if(base::is.null(out) && base::isTRUE(stop_if_null)){

      stop(
        glue::glue(
          "No hclust object for distance method '{method_dist}' and agglomerative method '{method_aggl}'."
        )
      )

    }

    return(out)

  })


# -----


