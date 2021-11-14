#' @include S4-clustering.R



# S4-classes --------------------------------------------------------------

ClusteringPam <- setClass(Class = "ClusteringPam",
                             slots = list(),
                             contains = "ClusteringMethod"
)

# r-objects ---------------------------------------------------------------

#' @export
valid_methods_pam <- c("euclidean", "manhattan")

# -----


# functions ---------------------------------------------------------------


#' @title Iterate over pam
#'
#' @param data All numeric data.frame or matrix.
#' @param inherit argument_dummy params
#' @param ... Additional arguments given to \code{cluster::pam()}.
#'
#' @return A list named according to all specified pam methods.
#' Each of these method-slots contains a list named according to the syntax \emph{k_<k>}
#' where \emph{<k>} is the respective integer of \code{ks}. The content
#' is the output of \code{cluster::pam()}.
#'
#' @export
#'
#' @examples
compute_clustering_pam <- function(data,
                                   ks,
                                   methods.pam = NULL,
                                   verbose = TRUE,
                                   ...){

  verbose.pb <- verbose

  ks <- check_ks(k.input = ks, of.length = NULL)

  check_one_of(
    input = methods.pam,
    against = validMethodsPam()
  )

  n_methods <- base::length(methods.pam)
  n_ks <- base::length(ks)

  n_total <- n_methods*n_ks

  give_feedback(
    msg = glue::glue("Iterating over {n_total} combinations of pam-method and k."),
    verbose = verbose
  )

  if(base::isTRUE(verbose.pb)){

    pb <- create_progress_bar(total = n_total)

  }

  pam_data <- base::as.matrix(data)

  results <- list()

  for(method in methods.pam){

    for(k_val in ks){

      if(base::isTRUE(verbose.pb)){

        pb$tick()

      } else if(base::isTRUE(verbose)){

        msg <- glue::glue("Running partitioning around medoids with method '{method}' and k = {k_val}.")

        give_feedback(msg = msg, verbose = verbose)

      }

      res <-
        cluster::pam(
          x = pam_data,
          k = k_val,
          metric = method,
          ...
        )

      res$data <- NULL

      pam_string <- stringr::str_c("k", k_val, sep = "_")

      results[[method]][[pam_string]] <- res

    }

  }

  give_feedback(msg = "Done.", verbose = verbose)

  return(results)

}

#' @rdname validInput
#' @export
validMethodsPam <- function(){

  return(valid_methods_pam)

}

# -----




# methods for external generics -------------------------------------------

#' @rdname getAvgSilWidthsDf
#' @export
setMethod(
  f = "getAvgSilWidthsDf",
  signature = "ClusteringPam",
  definition = function(object,
                        ks,
                        methods_pam = "euclidean"){

    ks <- check_ks(k.input = ks)

    check_one_of(
      input = methods_pam,
      against = validMethodsPam()
    )

    avg_sil_widths_df <-
      purrr::map_df(
        .x = methods_pam,
        .f = function(method_pam){

          method_df <-
            purrr::map_df(
              .x = ks,
              .f = function(k){

                pam <- getPam(object = object, k = k, method_pam = method_pam)

                out <-
                  data.frame(
                    method_pam = method_pam,
                    k = base::as.character(k),
                    avg_widths = pam$silinfo$avg.width

                  )

                return(out)

              }
            )

          return(method_df)

        }
      )

    return(avg_sil_widths_df)

  }
)

#' @rdname getSilWidthsDf
#' @export
setMethod(
  f = "getSilWidthsDf",
  signature = "ClusteringPam",
  definition = function(object,
                        ks,
                        method_pam = "euclidean"){

    ks <- check_ks(k.input = ks)

    ks_string <- stringr::str_c("k", ks, sep = "_")
    ks_string_2 <- stringr::str_c("k", ks, sep = " = ")

    sil_widths_df <-
      purrr::map_df(
        .x = ks,
        .f = function(k){


          pam <-
            getPam(
              object = object,
              k = k,
              method_pam = method_pam,
              stop_if_null = FALSE
            )

          if(!base::is.null(pam)){

            out <-
              base::as.data.frame(pam$silinfo$widths) %>%
              dplyr::mutate(
                cluster = forcats::as_factor(x = cluster),
                cluster_name = stringr::str_c("k", k, sep = " = "),
                x_axis = dplyr::row_number()
              ) %>%
              tibble::rownames_to_column(var = object@key_name)

          } else {

            out <- NULL

          }

          return(out)

        }
      ) %>%
      dplyr::mutate(cluster_name = base::factor(x = cluster_name, levels = ks_string_2)) %>%
      tibble::as_tibble()

    return(sil_widths_df)

  }
)

#' @rdname getPam
#' @export
setMethod(
  f = "getPam",
  signature = "ClusteringPam",
  definition = function(object, k, method_pam = "euclidean", stop_if_null = TRUE){

    k <- check_ks(k.input = k, of.length = 1)

    k_string <- stringr::str_c("k", k, sep = "_")

    out <- object@results[[method_pam]][[k_string]]

    if(base::is.null(out) && base::isTRUE(stop_if_null)){

      stop(
        glue::glue(
          "No pam object for method {method_pam} and {k} centers."
        )
      )

    }

    return(out)

  })

# -----
