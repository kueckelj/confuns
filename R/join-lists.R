#' @title Join two lists by their names
#'
#' @description Overwrites elements of \code{list.1} with those from \code{list.2}
#' that share the same name. Then merges all residual \strong{named} elements of
#'  \code{list.2} into \code{list.1}. Unnamed elements of \code{list.2} will be dropped.
#'
#' @param lst.1 A named list.
#' @param lst.2 A named list.
#' @param drop.unnamed Logical. If set to TRUE unnamed elements of the return list
#' will be dropped.
#'
#' @return A merged list.
#' @export
#'
join_lists <- function(lst.1 = list(),
                       lst.2 = list(),
                       drop.unnamed = TRUE){

  stopifnot(base::is.list(lst.1) && !base::is.data.frame(lst.1) &&
              base::is.list(lst.2) && !base::is.data.frame(lst.2))

  names_1 <- base::names(lst.1)
  names_2 <- base::names(lst.2)

  matches <- names_1[names_1 %in% names_2]

  if(base::length(matches) >= 1){

    for(match in matches){

      lst.1[[match]] <- lst.2[[match]]

      lst.2[[match]] <- NULL

    }

  }

  names_2 <- base::names(lst.2)

  if(base::length(names_2) >= 1){

    for(name in names_2){

      lst.1[[name]] <- lst.2[[name]]

    }

  }

  if(base::isTRUE(drop.unnamed)){

    drop_elements <- base::which(base::names(lst.1) == "")

    if(base::length(drop_elements) != 0){

      lst.1[[drop_elements]] <- NULL

    }

  }

  return(lst.1)

}


