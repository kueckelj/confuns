#'  @title Check data.frame validity
#'
#' @param df A data.frame.
#' @param var.class A named list. The names have to match the
#' variable names of the data.frame that are to be validated. The respective
#' elements specify the class the data.frame variable must have specified
#' as character strings.
#'
#' @return An informative error message or an invisible TRUE.
#' @export
#'
#' @examples
#'  # make sure that the input data.frame has
#'  # the numeric variables 'mpg' and 'cyl'.
#'
#'  check_data_frame(df = mtcars,
#'                   var.class = list(mpg = "numeric",
#'                                    cyl = "numeric"))

check_data_frame <- function(df, var.class = list()){

  base::stopifnot(base::is.data.frame(df))
  base::stopifnot(base::is.list(var.class))

  missing_vars <- base::vector(mode = "list")
  wrong_classes <- base::vector(mode = "list", length = ncol(df))

  for(name in base::names(var.class)){

    if(!name %in% base::colnames(df)){

      missing_vars[[name]] <- var.class[[name]]

    } else if(!base::all(var.class[[name]] %in% base::class(df[[name]]))){

      wrong_classes[[name]] <- base::class(df[[name]])

    }

  }

  if(base::any(c(base::length(missing_vars), base::length(wrong_class)) > 0)){

    base::message("Invalid or incomplete input: ")

    if(base::length(missing_vars) != 0){

      base::message("\n1.) Missing variables: ")

      print(base::unlist(missing_vars))

    }

    if(base::length(wrong_class) != 0){

      base::message("\n2.) Wrong variable classes. Should be:")
      print(base::unlist(var.class[names(var.class) %in% names(wrong_classes)]))

      base::message("Are currently:")
      var_classes <- confuns::variable_classes2(df)
      print(var_classes[names(var_classes) %in% names(wrong_classes)])

    }

    base::stop("Please adjust input accordingly in order to proceed.")

  } else {

    base::return(base::invisible(TRUE))

  }

}
