
#' @title This is a text dummy.
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
#' feedback message.

lazy_check_dummy <- function(){}


#' @title Print feedback in console
#'
#' @description Helper that gives feedback with a function of choice.
#'
#' @param fdb.fn Character value. Determines the function to call if a feedback
#' message needs to be given. One of \emph{'stop', 'warning'} or \emph{'message'}.
#' @param msg Character value or glue. The message to be printed in the console.
#' @param in.shiny Allows to use the function to stop a function without crashing
#' a shiny session.
#' @inherit verbose params
#'
#' @return
#' @export
#'

give_feedback <- function(fdb.fn = "message", msg = NULL, in.shiny = FALSE, verbose = TRUE, ...){

  if(base::isTRUE(in.shiny)){

    shiny_fdb(in.shiny = TRUE, ui = msg)

    if(type == "error"){

      shiny::req(FALSE)

    }

  } else if(!base::is.null(msg)){

    if(fdb.fn == "stop"){

      base::stop(msg)

    } else if(fdb.fn == "warning"){

      base::warning(msg)

    } else if(fdb.fn == "message" && base::isTRUE(verbose)){

      base::message(msg)

    }

  }

}

# is - functions ----------------------------------------------------------





#' @title List input check
#'
#' @param input Object to be checked.
#'
#' @return Boolean
#' @export
#'

is_list <- function(input){

  base::all(base::is.list(input) && !base::is.data.frame(input))

}



#' @title One dimensional input check
#'
#' @description Checks if input fits the requirements and gives feedback
#' via \code{give_feedback()}.
#'
#' @param x Input vector.
#' @param ... Character vector denoting the objects to be checked.
#' @param return Character value. Either \emph{'boolean'} which returns an
#' invisible TRUE or FALSE depending on if all tests evaluated to TRUE or not.
#' Or \emph{'results'} which returns a named vector of all results.
#' @param mode Character value. The type of which the input must be.
#' @param ref Character value. Input reference for the error message.
#' If set to NULL the value of \code{x} is evaluated via non standard evalulation.
#' @param of.length Numeric value. Denotes that the vector has to be of a certain length.
#' Holds priority over \code{min.length} and \code{max.length} - if not set to NULL the letter
#' two are ignored.
#' @param min.length,max.length Numeric value. Denotes that the vector has to be
#' of certain mininmal and/or maximal length.
#' @param skip.allow Logical. Allows the function to be skipped if \code{x} is equal
#' to \code{skip.val}.
#' @param skip.val The value that \code{x} needs to be equal to in order for the check
#' to be skipped.
#' @inherit give_feedback params
#'
#' @return An invisible TRUE or an informative error message.
#' @export
#'
#' @examples # Not run:
#'
#'  vec1 <- c(1,2),
#'  vec2 <- c(1,2,3,4,5)
#'
#'  is_vec(x = vec1, mode = "numeric", of.length = 2)
#'
#'  are_vectors(c("vec1", "vec2"), mode = "numeric", min.length = 2)
#'
#'
#'

is_value <- function(x,
                     mode,
                     ref = NULL,
                     fdb.fn = "stop",
                     skip.allow = FALSE,
                     skip.val = NULL){

  if(base::isTRUE(skip.allow) && base::identical(x, skip.val)){

    base::invisible(TRUE)

  } else {

    if(base::is.null(ref)){ ref <- base::substitute(x)}

    msg <- NULL

    if(!base::length(x) == 1 ||
       !base::is.vector(x, mode = mode)){

      msg <- glue::glue("Input '{ref}' must be a {mode} value.")

    }

    give_feedback(fdb.fn = fdb.fn, msg = msg)

    return_value <-
      base::ifelse(test = base::is.null(msg), yes = TRUE, no = FALSE)

    base::invisible(return_value)

  }

}

#' @rdname is_value
#' @export
is_vec <- function(x,
                   mode,
                   ref = NULL,
                   of.length = NULL,
                   min.length = NULL,
                   max.length = NULL,
                   fdb.fn = "stop",
                   skip.allow = FALSE,
                   skip.val = NULL){

  if(base::isTRUE(skip.allow) && base::identical(x, skip.val)){

    base::invisible(TRUE)

  } else {

    # refer to input in feedback
    if(base::is.null(ref)){ ref <- base::substitute(x) }

    # default if all requirements are satisfied
    msg <- NULL

    # logical value indicating if the length is to be checked
    length_requirements_given <-
      base::any(c(!base::is.null(min.length), !base::is.null(max.length), !base::is.null(of.length)))

    # check requirements and prepare feedback
    if(base::isTRUE(length_requirements_given)){

      if(!base::is.null(of.length)){

        ref_length <- stringr::str_c(" of length ", of.length, sep = "")

      } else {

        ref_min_length <-
          base::ifelse(test = base::is.null(min.length),
                       yes = "",
                       no = stringr::str_c(" of min. length ", min.length, sep = "")
          )

        ref_max_length <-
          base::ifelse(test = base::is.null(max.length),
                       yes = "",
                       no = stringr::str_c(" of max. length ", max.length, sep = "")
          )

        # connect with 'and' if both requirements are given
        ref_connect <-
          base::ifelse(test = base::sum(c(!base::is.null(min.length), !base::is.null(max.length))) != 2,
                       yes = "",
                       no = " and ")

        ref_length <-
          glue::glue("{ref_min_length}{ref_connect}{ref_max_length}")

      }

    } else {

      ref_length <- ""

    }

    # check input vector and assemble feedback
    if(!base::is.vector(x, mode = mode)){

      msg <- glue::glue("Input '{ref}' must be a {mode} vector{ref_length}.")

    } else if(base::isTRUE(length_requirements_given)){

      if(!base::is.null(min.length) && !base::length(x) >= min.length){

        msg <- glue::glue("Input '{ref}' must be a {mode} vector{ref_length}.")

      } else if(!base::is.null(max.length) && !base::length(x) <= max.length){

        msg <- glue::glue("Input '{ref}' must be a {mode} vector{ref_length}.")

      } else if(!base::is.null(of.length) && !base::length(x) == of.length){

        msg <- glue::glue("Input '{ref}' must be a {mode} vector{ref_length}.")

      }

    }

    give_feedback(fdb.fn = fdb.fn, msg = msg)

    return_value <-
      base::ifelse(test = base::is.null(msg), yes = TRUE, no = FALSE)

    base::invisible(return_value)

  }

}

#' @rdname is_value
#' @export
are_values <- function(...,
                       mode,
                       fdb.fn = "stop",
                       skip.allow = FALSE,
                       skip.val = NULL,
                       return = "boolean"){

  input <- c(...)

  base::stopifnot(base::is.character(input))

  ce <- rlang::caller_env()

  results <-
    purrr::map(.x = input, .f = ~ rlang::parse_expr(.x) %>% base::eval(envir = ce)) %>%
    purrr::set_names(nm = input) %>%
    purrr::imap(.f = confuns::is_value,
                mode = mode, fdb.fn = fdb.fn,
                skip.allow = skip.allow, skip.val = skip.val) %>%
    purrr::flatten_lgl() %>%
    purrr::set_names(nm = input)


  if(base::all(results == TRUE)){

    boolean <- base::invisible(TRUE)

  } else {

    boolean <- base::invisible(FALSE)

  }

  if(return == "boolean"){

    base::return(boolean)

  } else if(return == "results"){

    base::return(results)

  }

}

#' @rdname is_value
#' @export
are_vectors <- function(...,
                        mode,
                        fdb.fn = "stop",
                        of.length = NULL,
                        min.length = NULL,
                        max.length = NULL,
                        skip.allow = FALSE,
                        skip.val = NULL,
                        return = "boolean"){

  input <- c(...)

  base::stopifnot(base::is.character(input))

  ce <- rlang::caller_env()

  results <-
    purrr::map(.x = input, .f = ~ base::parse(text = .x) %>% base::eval(envir = ce)) %>%
    purrr::set_names(nm = input) %>%
    purrr::imap(.f = confuns::is_vec,
                mode = mode,
                fdb.fn = fdb.fn,
                of.length = of.length,
                min.length = min.length,
                max.length = max.length,
                skip.allow = skip.allow,
                skip.val = skip.val) %>%
    purrr::flatten_lgl() %>%
    purrr::set_names(nm = input)

  if(base::all(results == TRUE)){

    boolean <- base::invisible(TRUE)

  } else {

    boolean <- base::invisible(FALSE)

  }

  if(return == "boolean"){

    base::return(boolean)

  } else if(return == "results"){

    base::return(results)

  }

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

      missing_vars[[name]] <- var.class[[name]] #%>% stringr::str_c(collapse = "|")

    } else if(!base::any(var.class[[name]] %in% base::class(df[[name]]))){

      wrong_classes[[name]] <- base::class(df[[name]])

    }

  }


  if(base::any(c(base::length(missing_vars), base::length(wrong_classes)) > 0)){

    base::message(glue::glue("Invalid or incomplete '{ref}'-input: "))

    if(base::length(missing_vars) != 0){

      missing_vars <- purrr::map(missing_vars, stringr::str_c, collapse = "|")

      base::message("\n1.) Missing variables: ")

      print(base::unlist(missing_vars))

    }

    if(base::length(wrong_classes) != 0){

      var.class <- purrr::map(var.class, stringr::str_c, collapse = "|")

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

#' Check valid values
#'
#' @param input An input vector to be checked.
#' @param against A vector of valid inputs.
#' @param ref.input Character value or NULL. The reference for argument \code{input} input.#'
#'
#' @return An error message or an invisible TRUE if all values of input are valid.
#'
#' @details Error message is build via \code{glue::glue()} building the following
#' string:
#'
#' "Value/Values '{invalid_ref}' of {ref.input} is/are invalid. Valid input options are: '{against_ref}'."
#'
#' @export
#'


check_one_of <- function(input, against, ref.input = NULL){

  base::is.vector(input)
  base::is.vector(against)

  if(base::is.null(ref.input)){

    ref.input <- base::substitute(input)

  } else {

    is_value(ref.input, "character", "ref.input")

  }

  if(base::any(!input %in% against)){

    invalid <- input[!input %in% against]

    n_invalid <- base::length(invalid)

    if(n_invalid > 1){

      invalid_ref <-
        stringr::str_c(invalid[1:(n_invalid-1)], collapse = "', '") %>%
        stringr::str_c(., "' and '", invalid[n_invalid])

      ref1 <- "Values"
      ref2 <- "are"

    } else {

      invalid_ref <- invalid

      ref1 <- "Value"
      ref2 <- "is"
    }

    n_against <- base::length(against)

    against_ref <-
      stringr::str_c(against[1:(n_against-1)], collapse = "', '") %>%
      stringr::str_c(., "' or '", against[n_against])


    base::stop(glue::glue("{ref1} '{invalid_ref}' of {ref.input} {ref2} invalid. Valid input-options are: '{against_ref}'."))

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
#' @inherit give_feedback params
#' @param ref.input The reference character value for input.
#' @param ref.against The reference character value for against.
#' @param ... Additional arguments given to \code{give_feedback()}.
#'
#' @return An informative error message about which elements of \code{input} were found in \code{against} or an invisible TRUE.
#'
#' @details If none of the input values are found an error is raised with the message:
#'
#'   glue::glue("Of \code{ref.input} did not find 'missing' in \code{ref.against}.")
#'
#'   If only some of the input values are found the function denoted in \code{fdb.fn} is called with the message:
#'
#'   glue::glue("Did not find any element of \code{ref.input} in \code{ref.against}.")
#'
#' @export
#'

check_vector <- function(input,
                         against,
                         verbose = TRUE,
                         fdb.fn = "message",
                         ref.input = "input vector",
                         ref.against = "against vector",
                         ...){

  base::stopifnot(base::is.vector(input) & base::is.vector(against))
  base::stopifnot(base::class(input) == base::class(against))

  found <- against[against %in% input]
  missing <- input[!input %in% against]

  if(base::isTRUE(verbose) && base::length(missing) != 0){

    missing <- stringr::str_c(missing, collapse = "', '")

  }

  if(base::length(found) == 0){

    base::stop(glue::glue("Did not find any element of {ref.input} in {ref.against}."))

  } else {

    if(base::isTRUE(verbose) && base::length(missing) != 0){

      msg <- glue::glue("Of {ref.input} did not find '{missing}' in {ref.against}.")

      give_feedback(
        fdb.fn = fdb.fn,
        msg = msg,
        ...
      )

    }

    return(input[input %in% found])

  }

}


