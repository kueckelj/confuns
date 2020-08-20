
#' @title This is a text dummy
#'
#' @description A member of the \code{lazy-check_*()}-family.
#'
#' @details Members of the \code{lazy-check_*()}-family take the arguments
#' of their kind - that are used in the function they are called in - and
#' checks whether these arguments input fit the requirements. They stop and return an
#' error immediately once they stumble upon something invalid. They do not alter or adjust input
#' and return TRUE if the whole function has been executed without anything
#' invalid being found.
#'
#' @return A logical value TRUE if nothing invalid has been detected or an informative
#' error message.

lazy_check_dummy <- function(){}


# is - functions ----------------------------------------------------------

#' @title One dimensional input check
#'
#' @description Checks if input fits the requirements and stops the
#' function if not.
#'
#' @param x Input vector.
#' @param mode Character value. The type of which the input must be.
#' @param ref Character value. Input reference for the error message.
#'
#' @return An invisible TRUE or an informative error message.
#' @export
#'

is_value <- function(x, mode, ref = "x"){

  if(!base::length(x) == 1 ||
     !base::is.vector(x, mode = mode)){

    base::stop(glue::glue("Input '{ref}' must be a {mode} value."))

  }

  base::invisible(TRUE)

}

#' @rdname is_value
#' @export
is_vec <- function(x, mode, ref = "x"){

  if(!base::is.vector(x, mode = mode)){

    base::stop(glue::glue("Input '{ref}' must be a {mode} vector."))

  }

  base::invisible(TRUE)

}



# -----


# lazy check --------------------------------------------------------------

#' @title Check assign input
#'
#' @param assign Logical. If set to TRUE a named list will be assigned to the global
#' environment. This list contains data and information to rebuild or additionally
#' customize the output plot of this function.
#' @param assign_name The name the assigned list is supposed to have specified as
#' a single character value.
#'
#' @inherit lazy_check_dummy description details return
#' @export

check_assign <- function(assign = FALSE,
                         assign_name = character(1)){


  if(!base::is.logical(assign)){

    base::stop("Argument 'assign' needs to be logical.")

  }

  if(base::isTRUE(assign)){

    if(!base::is.character(assign_name) | !base::length(assign_name) == 1){

      base::stop("Argument 'assign_name' needs to be a single character value.")

    }

    if(assign_name == ""){

      base::stop("Argument 'assign_name' must not be ''.")

    }

    if(base::exists(x = assign_name, where = .GlobalEnv)){

      base::stop(stringr::str_c("It already exists an object named '",
                                assign_name, "' in the global environment.",
                                sep = ""))

    }


  }

  base::return(TRUE)

}



#' @title Check data.frame validity
#'
#' @description Checks whether the input data.frame contains variables
#' of certain classes and names.
#'
#' @param df A data.frame.
#' @param var.class A named list. The names have to match the
#' variable names of the data.frame that are to be validated. The respective
#' elements specify the class the data.frame variable must have specified
#' as character strings.
#' @param ref Character value. Input reference for the error message.
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

check_data_frame <- function(df, var.class = list(), ref = "df"){

  base::stopifnot(base::is.data.frame(df))
  base::stopifnot(base::is.list(var.class))


  # check variables
  missing_vars <- base::vector(mode = "list")
  wrong_classes <- base::vector(mode = "list")

  for(name in base::names(var.class)){

    if(!name %in% base::colnames(df)){

      missing_vars[[name]] <- var.class[[name]]

    } else if(!base::any(var.class[[name]] %in% base::class(df[[name]]))){

      wrong_classes[[name]] <- base::class(df[[name]])

    }

  }


  if(base::any(c(base::length(missing_vars), base::length(wrong_classes)) > 0)){

    base::message(glue::glue("Invalid or incomplete '{ref}'-input: "))

    if(base::length(missing_vars) != 0){

      base::message("\n1.) Missing variables: ")

      print(base::unlist(missing_vars))

    }

    if(base::length(wrong_classes) != 0){

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



#' @title Check directory input
#'
#' @param directories Character vector. Directories to check.
#' @param type Character value. One of "folders" or "files". Checks
#' whether the given directories lead to the specified type.
#'
#' @return An informative error message or an invisible TRUE.
#' @export

check_directories <- function(directories, ref = "directories", type = "folders"){

  is_vec(directories, mode = "character", "directories")
  is_value(ref, mode = "character", "ref")
  is_value(type, mode = "character", "type")

  base::stopifnot(type %in% c("files", "folders"))
  type2 <- c("files", "folders")[!c("files", "folders") %in% type]

  not_found <-
    base::lapply(X = directories,
                 FUN = function(dir){

                    check_fun <-
                      base::ifelse(type == "files", base::file.exists, base::dir.exists)

                     if(!check_fun(dir)){

                     base::return(dir)

                     } else {

                     base::return(NULL)

                    }}) %>%
    purrr::discard(.p = base::is.null) %>%
    base::unlist(use.names = FALSE)


  if(!base::is.null(not_found) && base::is.character(not_found)){

    not_found <- stringr::str_c("'\n- '", not_found, collapse = "")

    base::stop(glue::glue("The following directories given as input for argument '{ref}' do not exist or lead to {type2} instead of {type}: { not_found}'"))

  } else {

    base::return(base::invisible(TRUE))

  }

}


# -----


# adjusting check ---------------------------------------------------------

#' @title Compare input to control input
#'
#' @description Compares the values of an input-vector against a control-vector containing
#' valid values and returns the values of input that were found among the valid ones.
#'
#' @param input A vector of any kind.
#' @param against A vector of the same kind as \code{input}.
#' @inherit verbose params
#' @param ref.input The reference character value for input.
#' @param ref.against The reference character value for against.
#'
#' @return An informative error message about which elements of \code{input} were found in \code{against} or an invisible TRUE.
#' @export
#'

check_vector <- function(input,
                         against,
                         verbose = TRUE,
                         ref.input = "input vector",
                         ref.against = "against vector"){

  base::stopifnot(is.vector(input) & is.vector(against))
  base::stopifnot(class(input) == class(against))
  is_value(ref.input, "character", "input")
  is_value(ref.against, "character", "against")

  found <- against[against %in% input]
  missing <- input[!input %in% against]

  if(base::isTRUE(verbose)){

    missing <- stringr::str_c(missing, collapse = "', '")

  }

  if(base::length(found) == 0){

    base::stop(glue::glue("Did not find any element of '{ref.input}' in '{ref.against}'."))

  } else {

    if(base::isTRUE(verbose) && base::length(missing) != 0){

      base::message(glue::glue("Of {ref.input} did not find '{missing}' in {ref.against}."))

    }

    return(found)

  }

}


