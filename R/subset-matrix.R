


#' Title
#'
#' @param mtr
#' @param identical.dims
#' @param dim
#' @param variables.subset
#'
#' @return
#' @export
#'
subset_mtr <- function(mtr = NULL, dims = NULL, variables.subset = NULL){

  if(base::is.character(variables.subset) && !base::is.null(mtr)){

    mtr_class <- base::class(mtr)

    mtr <- base::as.matrix(mtr)

    vars_keep <-
      stringr::str_subset(variables.subset, pattern = "^-", negate = TRUE)

    vars_discard <-
      stringr::str_subset(variables.subset, pattern = "^-") %>%
      stringr::str_remove_all(pattern = "^-")

    if(1 %in% dims & 2 %in% dims){ # subset both dimensions

      if(base::length(vars_discard) >= 1){

        mtr <- mtr[!base::rownames(mtr) %in% vars_discard, !base::colnames(mtr) %in% vars_discard]

      }

      if(base::length(vars_keep) >= 1){

        mtr <- mtr[base::rownames(mtr) %in% vars_keep, base::colnames(mtr) %in% vars_keep]

      }

    } else {


      if(1 %in% dims){ # subset by rows

        if(base::length(vars_discard) >= 1){

          mtr <- mtr[!base::rownames(mtr) %in% vars_discard,]

        }

        if(base::length(vars_keep) >= 1){

          mtr <- mtr[base::rownames(mtr) %in% vars_keep,]

        }

      } else if(2 %in% dims){ # subset by cols

        if(base::length(vars_discard) >= 1){

          mtr <- mtr[, !base::colnames(mtr) %in% vars_discard]

        }

        if(base::length(vars_keep) >= 1){

          mtr <- mtr[, base::colnames(mtr) %in% vars_keep]

        }

      }

    }


    if("dist" %in% mtr_class){

      mtr <- stats::as.dist(m = mtr)

    }


  }

  base::return(mtr)


}
