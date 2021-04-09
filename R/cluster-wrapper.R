

#' Title
#'
#' @param data
#' @param make.names
#' @param names.prefix
#' @param names.sep
#'
#' @return
#' @export
#'
prepare_cluster_data <- function(data,
                                 make.names = TRUE,
                                 names.prefix = NULL,
                                 names.sep = "-"){

  data_names <- base::rownames(data)

  if(base::is.data.frame(data)){

    data <- dplyr::select_if(data, .predicate = base::is.numeric)

  }

  if(tibble::is_tibble(data)){

    data <- base::as.matrix(data)

  }

  if(base::isTRUE(make.names)){

    is_value(x = names.prefix, mode = "character")

    base::rownames(data) <- stringr::str_c(names.prefix, 1:base::nrow(data), sep = names.sep)

  }

  base::return(data)

}
