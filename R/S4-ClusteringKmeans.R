#' @include S4-clustering.R



# S4-classes --------------------------------------------------------------

ClusteringKmeans <- setClass(Class = "ClusteringKmeans",
                             slots = list(),
                             contains = "ClusteringMethod"
)

# r-objects ---------------------------------------------------------------

#' @export
valid_methods_kmeans <- c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")

# -----


# functions ---------------------------------------------------------------


#' @title Iterate over kmeans
#'
#' @param data All numeric data.frame or matrix.
#' @param ks Numeric vector. All inputs for argument \code{centers} of function
#' \code{stats::kmeans()}.
#' @param inherit argument_dummy params
#'
#' @return A list named according to all specified kmeans methods.
#' Each of these method-slots contains a list named according to the syntax \emph{k_<k>}
#' where \emph{<k>} is the respective integer of \code{centers}. The content
#' is the output of \code{stats::kmeans()}.
#'
#' @export
#'
#' @examples
compute_clustering_kmeans <- function(data,
                                      ks = 2:10,
                                      methods.kmeans = NULL,
                                      verbose = TRUE,
                                      ...){

  verbose.pb <- verbose

  is_vec(x = ks, mode = "numeric")

  n_methods <- base::length(methods.kmeans)
  n_ks <- base::length(ks)

  if(n_ks > 1){

    msg <-
      glue::glue(
        "Iterating over {n_methods} {ref_method} and {n_ks} input {ref_ks} for argument 'center'.",
        ref_method = adapt_reference(methods.kmeans, sg = "method", pl = "methods"),
        ref_ks = adapt_reference(ks, sg = "option", pl = "options"))

    give_feedback(msg = msg, verbose = verbose)

  }

  verbose_input <- verbose

  if(base::isTRUE(verbose.pb)){

    pb <- create_progress_bar(total = n_methods)

    verbose <- FALSE

  }

  data_mtr <- base::as.matrix(data)

  results <- list()

  for(method in methods.kmeans){

    if(base::isTRUE(verbose.pb)){ pb$tick() }

    msg <-
      glue::glue("Iterating over {n_ks} different input options for argument 'centers' with method '{method}'")

    give_feedback(msg = msg, verbose = verbose)

    for(k in ks){

      msg <- glue::glue("Input for argument 'centers' = {k}.")

      give_feedback(msg = msg, verbose = verbose)

      res <- stats::kmeans(x = data_mtr, centers = k, ...)

      if(shiny::isTruthy(res)){

        slot_name <- stringr::str_c("k", k, sep = "_")

        results[[method]][[slot_name]] <- res

      }

    }

  }

  give_feedback(msg = "Done.", verbose = verbose_input)

  return(results)

}

#' @rdname validInput
#' @export
validMethodsKmeans <- function(){

  return(valid_methods_kmeans)

}

# -----


# own generics ------------------------------------------------------------


# -----


# methods for external generics -------------------------------------------


#' @rdname getKmeans
#' @export
setMethod(
  f = "getKmeans",
  signature = "ClusteringKmeans",
  definition = function(object, k, method_kmeans = "Hartigan-Wong", stop_if_null = TRUE){

  center_string <- stringr::str_c("k", k, sep = "_")

  out <- object@results[[method_kmeans]][[center_string]]

  if(base::is.null(out) && base::isTRUE(stop_if_null)){

    stop(
      glue::glue(
        "No kmeans object for method {method_kmeans} and {center} centers."
      )
    )

  }

  return(out)

})

# -----
