
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
#' @param with.time Logical value. Indicates whether the current time is to be
#' added to the feedback message.
#' @inherit verbose params
#'
#' @return
#' @export
#'

give_feedback <- function(fdb.fn = "message", msg = NULL, in.shiny = FALSE, with.time = TRUE, verbose = TRUE, ...){

  if(!base::is.null(msg) && base::isTRUE(with.time)){

    time <- base::Sys.time()

    hours <- lubridate::hour(time)

    ref_hours <-
      base::ifelse(
        test = stringr::str_length(hours) == 1,
        yes = stringr::str_c(0, hours, sep = ""),
        no = hours)

    minutes <- lubridate::minute(time)

    ref_minutes <-
      base::ifelse(
        test = stringr::str_length(minutes) == 1,
        yes = stringr::str_c(0, minutes, sep = ""),
        no = minutes)

    seconds <- base::round(lubridate::second(time), digits = 0)

    ref_seconds <-
      base::ifelse(
        test = stringr::str_length(seconds) == 1,
        yes = stringr::str_c(0, seconds, sep = ""),
        no = seconds)

    time_string <-
      stringr::str_c(
        ref_hours,
        ref_minutes,
        ref_seconds,
        sep = ":"
      )

    msg <- glue::glue("{time_string} {msg}")

  }

  if(base::isTRUE(in.shiny) && !base::is.null(msg)){

    if(fdb.fn == "stop"){

      type <- "error"

    } else if(fdb.fn == "warning") {

      type <- "warning"

    } else if(fdb.fn == "message"){

      type <- fdb.fn

    }

    shiny_fdb(in.shiny = TRUE, ui = msg, type = type, ...)

    if(fdb.fn == "stop"){

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



#' Title
#'
#' @description Returns the appropriate string to extract the feedback
#' from \code{purr::quietly()} results.
#'
#' @inherit give_feedback params
#'

extract_feedback <- function(fdb.fn){

  if(fdb.fn == "message"){

    return("messages")

  } else if(fdb.fn == "warning"){

    return("warnings")

  } else if(fdb.fn == "stop"){

    return("stop")

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



#' @title Any input check
#'
#' @param input Object to be checked.
#' @inherit argument_dummy params
#'
#' @return TRUE if \code{input} is of at least one of
#' the specified classes in \code{valid.classes}. FALSE
#' if not.
#'
#' @export

is_any_of <- function(input, valid.classes){

  res_lgl <-
    purrr::map_lgl(
      .x = valid.classes,
      .f = ~ base::is.vector(x = input, mode = .x)
      )

  if("factor" %in% valid.classes | "any" %in% valid.classes){

    res_lgl <- c(res_lgl, base::is.factor(input))

  }

  base::any(res_lgl)

}



#' @title One dimensional input check
#'
#' @description Checks if input fits the requirements and gives feedback
#' via \code{give_feedback()}.
#'
#' @param x Input vector.
#' @param ... Character vector denoting the objects to be checked.
#' @param return Character value. Either \emph{'boolean'} which makes the function return an
#' invisible TRUE or FALSE depending on if all tests evaluated to TRUE or not.
#' Or \emph{'results'} which returns a named vector of all results.
#' @param mode Character value. The type of which the input must be.
#' @param ref Character value. Input reference for the error message.
#' If set to NULL the value of \code{x} is evaluated via non standard evalulation.
#' @param of.length Numeric value. Denotes that the vector has to be of a certain length.
#' Holds priority over \code{min.length} and \code{max.length} - if not set to NULL the letter
#' two are ignored.
#' @param min.length,max.length Numeric value. Denotes that the vector has to be
#' of certain minimal and/or maximal length.
#' @param skip.allow Logical. Allows the function to be skipped if \code{x} and
#' \code{skip.val} are identical.
#' @param skip.val The value that \code{x} needs to be equal to in order for the check
#' to be skipped.
#' @param verbose Logical value. Indicates whether any kind of feedback is supposed to
#' be given. \code{verbose} set to FALSE shuts down any error, warning or general messages
#' and results in the functions returning what is specified in \code{return}.
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
                     verbose = TRUE,
                     skip.allow = FALSE,
                     skip.val = NULL,
                     with.time = FALSE){

  if(base::isTRUE(skip.allow) && base::identical(x, skip.val)){

    base::invisible(TRUE)

  } else {

    if(base::is.null(ref)){ ref <- base::substitute(x)}

    msg <- NULL

    if(!base::length(x) == 1 ||
       !base::is.vector(x, mode = mode)){

      msg <- glue::glue("Input '{ref}' must be a {mode} value.")

    }

    # give feedback
    if(base::isFALSE(verbose)){fdb.fn <- "message"}

    give_feedback(
      fdb.fn = fdb.fn,
      msg = msg,
      verbose = verbose,
      with.time = with.time)

    return_value <-
      base::ifelse(test = base::is.null(msg), yes = TRUE, no = FALSE)

    return(return_value)

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
                   skip.allow = FALSE,
                   skip.val = NULL,
                   fdb.fn = "stop",
                   verbose = TRUE,
                   with.time = FALSE){

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

    # give feedback
    if(base::isFALSE(verbose)){fdb.fn <- "message"} # verbose = FALSE and fdb.fn = "message" -> no feedback

    give_feedback(
      fdb.fn = fdb.fn,
      msg = msg,
      verbose = verbose,
      with.time = with.time
      )

    return_value <-
      base::ifelse(test = base::is.null(msg), yes = TRUE, no = FALSE)

    return(return_value)

  }

}

#' @rdname is_value
#' @export
are_values <- function(...,
                       mode,
                       fdb.fn = "stop",
                       verbose = TRUE,
                       with.time = FALSE,
                       skip.allow = FALSE,
                       skip.val = NULL,
                       return = "boolean"){

  input <- c(...)

  base::stopifnot(base::is.character(input))

  ce <- rlang::caller_env()

  results <-
    purrr::map(.x = input, .f = ~ rlang::parse_expr(.x) %>% base::eval(envir = ce)) %>%
    purrr::set_names(nm = input) %>%
    purrr::imap(.f = purrr::quietly(

      ~ confuns::is_value(
          x = .x,
          ref = .y,
          mode = mode,
          fdb.fn = "message",
          verbose = verbose,
          with.time = with.time,
          skip.allow = skip.allow,
          skip.val = skip.val
        )

      )
    ) %>%
    purrr::set_names(nm = input)

  # keep as valid if the fdb.fn slot is an empty character (=> no feedback equals valid input)
  valid_inputs <-
    purrr::map_lgl(
      .x = results,
      .f = ~ base::identical(.x[["messages"]], base::character(0))
      )

  # extract the feedback messages of the invalid inputs
  msg <-
    purrr::map(.x = results[!valid_inputs], .f = ~ .x[["messages"]]) %>%
    glue_list_report(
      lst = .,
      separator = NULL,
      combine_via = " \n"
      )

  if(base::length(msg) >= 1){

    give_feedback(
      msg = msg,
      verbose = verbose,
      fdb.fn = fdb.fn,
      with.time = FALSE)

  }

  # extrac the boolean return values of the actual check
  results <-
    purrr::map_lgl(.x = results, .f = ~ .x[["result"]])

  if(base::all(results == TRUE)){

    boolean <- base::invisible(TRUE)

  } else {

    boolean <- base::invisible(FALSE)

  }

  if(return == "boolean"){

    return(boolean)

  } else if(return == "results"){

    return(results)

  }

}

#' @rdname is_value
#' @export
are_vectors <- function(...,
                        mode,
                        fdb.fn = "stop",
                        verbose = TRUE,
                        with.time = FALSE,
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
    purrr::imap(.f =  purrr::quietly(

        ~ confuns::is_vec(
          x = .x,
          ref = .y,
          mode = mode,
          fdb.fn = "message",
          verbose = verbose,
          with.time = with.time,
          of.length = of.length,
          min.length = min.length,
          max.length = max.length,
          skip.allow = skip.allow,
          skip.val = skip.val
        )

      )
    ) %>%
    purrr::set_names(nm = input)

  # keep as valid if the fdb.fn slot is an empty character (=> no feedback equals valid input)
  valid_inputs <-
    purrr::map_lgl(
      .x = results,
      .f = ~ base::identical(.x[["messages"]], base::character(0))
    )

  # extract the feedback messages of the invalid inputs
  msg <-
    purrr::map(.x = results[!valid_inputs], .f = ~ .x[["messages"]]) %>%
    glue_list_report(
      lst = .,
      separator = NULL,
      combine_via = " \n"
      )

  if(base::length(msg) >= 1){

    give_feedback(
      msg = msg,
      verbose = verbose,
      fdb.fn = fdb.fn,
      with.time = FALSE)

  }

  # extrac the boolean return values of the actual check
  results <-
    purrr::map_lgl(.x = results, .f = ~ .x[["result"]])

  if(base::all(results == TRUE)){

    boolean <- TRUE

  } else {

    boolean <- FALSE

  }

  if(return == "boolean"){

    return(boolean)

  } else if(return == "results"){

    return(results)

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

  confuns::is_value(assign, mode = "logical")

  if(base::isTRUE(assign)){

    confuns::is_value(assign_name, mode = "character")

    if(assign_name == ""){

      base::stop("Argument 'assign_name' must not be ''.")

    }

    if(base::exists(x = assign_name, where = .GlobalEnv)){

      base::stop(stringr::str_c("It already exists an object named '",
                                assign_name, "' in the global environment.",
                                sep = ""))

    }

  }

  return(TRUE)

}




#' @title Check no overlap
#'
#' @param x,y Input vectors whoose content is to be checked.
#'
#' @return Error message if overlap is found. TRUE is no overlap is found.

check_no_overlap <- function(x, y, fdb.fn = "stop", with.time = FALSE){

  ref_x <- base::substitute(x)
  ref_y <- base::substitute(y)

  overlap <- base::intersect(x, y)

  if(base::length(overlap) >= 1){

    msg <-
      glue::glue(
        "Overlap is not allowed. {ref1} '{ref_overlap}' {ref2} part of input for argument '{ref_x}' and argument '{ref_y}'.",
        ref1 = adapt_reference(overlap, sg = "Value", pl = "Values"),
        ref2 = adapt_reference(overlap, sg = "is", pl = "are"),
        ref_overlap = glue::glue_collapse(x = overlap, sep = "', '", last = "' and '")
      )

    give_feedback(
      msg = msg,
      fdb.fn = fdb.fn,
      with.time = with.time
    )

  } else {

    return(TRUE)

  }

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
#' @inherit is_value params
#' @inherit give_feedback params
#'
#' @return An informative message, warning or error or TRUE if valid.
#' @export
#'
#' @examples
#'  # make sure that the input data.frame has
#'  # the numeric variables 'mpg' and 'cyl'.
#'
#'  check_data_frame(df = mtcars,
#'                   var.class = list(mpg = "numeric",
#'                                    cyl = "numeric"))

check_data_frame <- function(df,
                             var.class = list(),
                             ref = NULL,
                             verbose = TRUE,
                             with.time = FALSE,
                             fdb.fn = "stop"){

  # denote input reference
  if(base::is.null(ref)){

    ref_input <- base::substitute(df)

  } else {

    ref_input <- ref

  }

  # assemble report if anything is invalid

  all_names <- base::names(df)

  report <- base::vector(mode = "list")

  for(name in base::names(var.class)){

    ref_name <- stringr::str_c("Variable '", name, "'", sep = "")

    if(!name %in% all_names){

      report[[ref_name]] <- "is missing."

    } else if(!is_any_of(input = df[[name]], valid.classes = var.class[[name]])){

        report[[ref_name]] <-
          glue::glue(
            "must be of class '{ref_valid_classes}' but is of {ref1} '{ref_current_class}'.",
            ref_valid_classes = glue::glue_collapse(var.class[[name]], sep = "', '", last = "' or '"),
            ref1 = adapt_reference(base::class(df[[name]]), sg = "class", pl = "classes"),
            ref_current_class = glue::glue_collapse(base::class(df[[name]]), sep = ", ", last = "' and '")
          ) %>%
          base::as.character()

    }

  }

  # return report if anything is invalid else return TRUE
  if(base::length(report) >= 1){

    msg_init <- glue::glue("\n\nProblematic data.frame input for argument '{ref_input}':\n\n")

    msg_report <-
      glue_list_report(
        lst = report,
        separator = " ",
        combine_via = "\n"
      )

    msg <- glue::glue("{msg_init}{msg_report}")

    confuns::give_feedback(
      msg = msg,
      fdb.fn = fdb.fn,
      with.time = with.time,
      verbose = verbose
    )

    base::invisible(FALSE)

  } else {

    return(TRUE)

  }

}

#' @title Check directory input
#'
#' @param directories Character vector. Directories to check.
#' @param type Character value. One of \emph{'files', 'folders', 'create_files'}. Checks
#' whether the given directories lead to the specified type or are creatable.
#' @inherit is_value params
#' @inherit give_feedback params
#'
#' @return An informative error message or an invisible TRUE.
#' @export

check_directories <- function(directories,
                              ref = NULL,
                              type = "folders",
                              fdb.fn = "stop",
                              with.time = FALSE,
                              verbose = TRUE){

  is_vec(directories, mode = "character", "directories")
  is_value(ref, mode = "character", skip.allow = TRUE, skip.val = NULL)
  is_value(type, mode = "character")

  if(base::is.null(ref)){

    ref_input <-
      glue::glue(
        "specified as input for argument '{ref_arg}'",
        ref_arg = base::substitute(directories)
      )

  } else {

    ref_input <- ref

  }


  base::stopifnot(type %in% c("files", "folders", "create_files"))

  msg <- NULL

  if(type %in% c("files", "folders")){

    not_found <-
      purrr::map(.x = directories,
                 .f = function(dir){

                   check_fun <-
                     base::ifelse(type == "files", base::file.exists, base::dir.exists)

                   if(!check_fun(dir)){

                     return(dir)

                   } else {

                     return(NULL)

                   }}) %>%
      purrr::discard(.p = base::is.null) %>%
      base::unlist(use.names = FALSE)

    if(!base::is.null(not_found) && base::is.character(not_found)){

      type2 <- c("files", "folders")[!c("files", "folders") %in% type]

      msg <-
        glue::glue(
          "The following {ref1} {ref_input} {ref2} not exist or {ref3} to {ref4}{ref5} instead of {ref6}{ref7}: \n- {ref_not_found}",
          ref1 = adapt_reference(not_found, sg = "directory", pl = "directories"),
          ref2 = adapt_reference(not_found, sg = "does", pl = "do"),
          ref3 = adapt_reference(not_found, sg = "leads", pl = "lead"),
          ref4 = adapt_reference(not_found, sg = "a ", pl = ""),
          ref5 = adapt_reference(not_found, sg = stringr::str_remove(type2, "s$"), pl = type2),
          ref6 = adapt_reference(not_found, sg = "a ", pl = ""),
          ref7 = adapt_reference(not_found, sg = stringr::str_remove(type, "s$"), pl = type),
          ref_not_found = glue::glue_collapse(not_found, sep = "\n- ")
        )

      confuns::give_feedback(
        msg = msg,
        fdb.fn = fdb.fn,
        with.time = with.time,
        verbose = verbose
      )

    }

  } else if(type == "create_files") {

    not_creatable <-
      purrr::keep(.x = directories, .p = function(dir){

        if(base::file.exists(dir)){

          return(TRUE)

        } else {

          res <-
            base::isFALSE(base::file.create(dir, showWarnings = FALSE))

          if(base::isTRUE(res)){base::file.remove(dir)}

          return(res)

        }

      })

    if(base::length(not_creatable) >= 1){

      msg <-
        glue::glue(
          "Attempting to create {ref1} '{ref_dir}' did not work. Do all subfolders of the specified {ref1} exist? Does the file or folder already exist?",
          ref1 = adapt_reference(not_creatable, sg = "directory", pl = "directories"),
          ref_dir = glue::glue_collapse(not_creatable, sep = "', '", last = "' and '")
          )

      confuns::give_feedback(
        msg = msg,
        fdb.fn = fdb.fn,
        verbose = verbose,
        with.time = with.time
      )

    }

  }

  if(base::is.null(msg)){

    return(TRUE)

  } else {

    return(FALSE)

  }

}

#' @title Check h and k input for hclust
#' @export

check_h_k <- function(h = NULL, k = NULL, only.one = FALSE, skip.allow = TRUE){

  if(base::all(base::is.null(k), base::is.null(h)) & base::isFALSE(skip.allow)){

    msg <- "Please specify either argument 'k/ks' or argument 'h/hs'."

    give_feedback(msg = msg, fdb.fn = "stop")

  }

  if(base::isTRUE(only.one)){

    if(base::all(base::is.numeric(k), base::is.numeric(h))){

      msg <- "Please specify only one of argument 'k' or argument 'h'. Not both."

      give_feedback(msg = msg, fdb.fn = "stop")

    }

  }

 are_vectors(c("k", "h"), mode = "numeric", skip.allow = TRUE, skip.val = NULL)

}

#' @title Overwrite check
#'
#' @description Make sure that nothing is overwritten without specifically allowing it.
#'
#' @inherit check_one_of params
#' @export
check_none_of <- function(input,
                          against,
                          ref.input = NULL,
                          ref.against,
                          overwrite = NULL,
                          fdb.fn = "stop",
                          with.time = FALSE,
                          force = NULL){

  if(base::is.null(ref.input)){

    ref.input <- "Argument input"

  }

  if(base::isTRUE(overwrite) | base::isTRUE(force)){

    base::invisible(TRUE)

  } else {

    overlap <- against[against %in% input]

    if(base::length(overlap) >= 1){

      ref_overlap <- glue::glue_collapse(overlap, sep = "', '", last = "' and '")

      ref1 <- adapt_reference(overlap, sg = "Value", pl = "Values")

      ref2 <- adapt_reference(overlap, sg = "is", pl = "are")

      if(!base::is.null(overwrite)){

        ref_overwrite <- overwrite_hint

      } else {

        ref_overwrite <- ""

      }

      if(!base::is.null(force)){

        ref_overwrite <- " Set argument 'force' to TRUE in order to force computation."

      } else {

        ref_overwrite <- ""

      }

      msg <- glue::glue("{ref1} '{ref_overlap}' of {ref.input} {ref2} already present in {ref.against}.{ref_overwrite}")

      give_feedback(msg = msg, fdb.fn = fdb.fn, with.time = with.time)

    } else {

      base::invisible(TRUE)

    }

  }

}

#' Check valid values
#'
#' @param input An input vector to be checked.
#' @param against A vector of valid inputs.
#' @param ref.input Character value or NULL. The reference for argument \code{input} input.
#'
#' @return An error message or an invisible TRUE if all values of input are valid.
#'
#' @details Error message is build via \code{glue::glue()} building the following
#' string:
#'
#' "Value/Values '\emph{invalid values}' of {ref.input} is/are invalid. Valid input options are: '{\emph{valid inputs}}'."
#'
#' @export
#'
check_one_of <- function(input,
                         against,
                         ref.input = NULL,
                         fdb.fn = "stop",
                         fdb.opt = 1,
                         ref.opt.2 = "valid input options",
                         suggest = TRUE,
                         verbose = TRUE,
                         with.time = FALSE,
                         ...){

  base::is.vector(input)
  base::is.vector(against)

  if(base::is.null(ref.input)){

    ref.input <-
      base::tryCatch({

        ref.input <-
          glue::glue("input for argument '{base::substitute(input)}'") %>%
          base::as.character()

      }, error = function(error){

        "input"

      })

    if(base::length(ref.input) == 2){ ref.input <- "input" }

  } else {

    if("glue" %in% base::class(ref.input)){

      ref.input <- base::as.character(ref.input)

    } else {

      is_value(ref.input, "character")

    }

  }
  
  if(base::length(ref.input) > 1){
    
    ref.input <- "input"
    
    }
    

  if(base::any(!input %in% against)){

    ref_against <- glue::glue_collapse(against, sep = "', '", last = "' and '")

    invalid <- input[!input %in% against]

    valid_input_options <-
      glue::glue("Valid input options are: '{ref_against}'") %>%
      base::as.character()

    if(base::isTRUE(suggest)){

      suggestions <-
        str_suggest_list(
          string = invalid,
          pool = against,
          ...
        )

      n_suggestions <-
        purrr::map(suggestions, .f = base::length) %>%
        base::unname()

      end_string <-
        purrr::discard(.x = suggestions, .p = ~ base::length(.x) == 0) %>%
        purrr::map(
          .f = ~
            scollapse(.x, sep = "', '", last = "' or '") %>%
            base::as.character() %>%
            wrap_strings(wrap.in = c("'", "'?"))
        ) %>%
        glue_list_report(
          separator = "' did you mean ",
          prefix = "Instead of '"
        ) %>%
        base::as.character()

      if(base::any(n_suggestions == 0)){

        end_string <- valid_input_options

      }

    } else {

      end_string <- valid_input_options

    }

    if(fdb.opt == 1){

      msg <-
        glue::glue(
          "{ref1} '{ref_invalid}' of {ref.input} {ref2} invalid. {end_string}.",
          ref1 = adapt_reference(invalid, sg = "Value", pl = "Values"),
          ref2 = adapt_reference(invalid, sg = "is", pl = "are"),
          ref_invalid = glue::glue_collapse(invalid, sep = "', '", last = "' and '"),

        )

    } else if(fdb.opt == 2) {

      msg <- glue::glue(
        glue::glue(
          "Did not find {ref1} '{ref_invalid}' of {ref.input} among {ref.opt.2}. {end_string}.",
          ref1 = adapt_reference(invalid, sg = "value", pl = "values"),
          ref_invalid = glue::glue_collapse(invalid, sep = "', '", last = "' and '")
        )
      )

    }

    confuns::give_feedback(
      msg = msg,
      fdb.fn = fdb.fn,
      with.time = with.time,
      verbose = verbose
    )

  } else {

    return(TRUE)

  }

}
# -----



# adjusting check ---------------------------------------------------------

#' @title Check across subset input
#' @export
check_across_subset_negate <- function(across, across.subset, all.groups){

  across_subset_input <- base::substitute(across.subset)

  # distinguish between groups to keep and groups to discard
  discard_groups <-
    stringr::str_subset(across.subset, pattern = "^-") %>%
    stringr::str_remove_all(pattern = "^-")

  keep_groups <-
    stringr::str_subset(across.subset, pattern = "^[^-]")

  # check for ambiguous input
  duplicated_groups <-
    base::intersect(keep_groups, discard_groups)

  if(base::length(duplicated_groups) >= 1){

    duplicated_groups <- stringr::str_c("(-)", duplicated_groups)

    msg <-
      glue::glue("Ambiguous values ('{duplicated_input}') in input for argument '{across_subset_input}'.",
                 duplicated_input = glue::glue_collapse(x = duplicated_groups, sep = "', ", last = "' and '"))

    give_feedback(fdb.fn = "stop", msg = msg, with.time = FALSE)

  }

  across.subset <- c(keep_groups, discard_groups)

  # keep valid groups
  check_one_of(
    input = across.subset,
    against = all.groups,
    ref.input = base::as.character(glue::glue("input to subset '{across}'-groups"))
  )

  #if no error all are valid
  across.subset_valid <- across.subset

  # keep valid distinguished groups
  discard_groups <- discard_groups[discard_groups %in% across.subset_valid]

  # in case only -across.subset has been provided "refill" 'keep_groups'
  if(base::length(keep_groups) == 0){

    keep_groups <- all.groups

  }

  # discard what has been denoted with -
  keep_groups <- keep_groups[!keep_groups %in% discard_groups]

  return(keep_groups)

}

#' @title Data.frame variable check
#'
#' @description Selects the variables denoted in
#' \code{keep} and \code{variables}. The letter ones
#' are checked for validity. If \code{variables} is
#' set to NULL all valid variables are kept.
#'
#' @param keep Character vector or NULL. If character, specifies variables
#' that are to be kept even if they are not of those classes denoted in
#' \code{valid.classes}. Variables specified like that are not included in
#' the pivoting process!
#'
#' @param ref_df Character value. Given to argument \code{ref} of
#' function \code{check_data_frame()}.
#'
#' @inherit argument_dummy params
#'
#' @return The input \code{df} with all selected variables.
#' @export
#'

check_df_variables <- function(df, valid.classes, variables = NULL, keep = NULL, ref_df = NULL, verbose = TRUE){

  # extract and check 'variables'
  if(base::is.null(variables) | base::any(stringr::str_detect(variables, pattern = "^-"))){

    res_df <-
      purrr::keep(.x = df, .p = ~ is_any_of(.x, valid.classes))

    discard_variables <-
      stringr::str_subset(variables, pattern = "^-") %>%
      stringr::str_remove(pattern = "^-")

    if(base::length(discard_variables) >= 1){

      check_one_of(
        input = discard_variables,
        against = base::colnames(res_df),
        ref.input = "variables to be discarded"
      )

      res_df <- dplyr::select(res_df, -dplyr::all_of(discard_variables))

    }

  } else {

    var.class <-
      purrr::map(.x = variables, .f = function(var){ valid.classes }) %>%
      purrr::set_names(nm = variables)

    check_data_frame(
        df = df,
        var.class = var.class,
        ref = ref_df
      )

    # if no error was thrown keep all variables
    res_df <- dplyr::select(df, dplyr::all_of(variables))

  }

  variables_kept <- base::colnames(res_df)

  # extract and check 'keep'
  if(base::is.character(keep) & !base::all(keep %in% variables_kept)){

    keep <- keep[!keep %in% variables_kept]

    var.class <-
      purrr::map(.x = keep, .f = function(i){ "any" }) %>%
      purrr::set_names(nm = keep)

    check_data_frame(
      df = df,
      var.class = var.class,
      ref = ref_df,
    )

    keep_df <-
      dplyr::select(df, dplyr::all_of(x = keep))

  } else {

    keep_df <- NULL

  }

  # if additional variables have been kept with argument 'keep'
  # add to the resulting data.frame
  if(base::is.data.frame(keep_df) & base::is.data.frame(res_df)){

    res_df <-
      base::cbind(keep_df, res_df)

  }

  return(res_df)

}


#' @title Check and adjust k
#' @return Numeric vector.
#' @export
#'
check_ks <- function(k.input, of.length = NULL){

  ref <- base::substitute(k.input)

  is_vec(x = k.input, ref = ref,  mode = "numeric", of.length = of.length)

  out <-
    base::as.integer(k.input) %>%
    base::unique() %>%
    base::sort()

  return(out)

}


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
#'   glue::glue("Did not find any element of \code{ref.input} in \code{ref.against}.")
#'
#'   If only some of the input values are found the function denoted in \code{fdb.fn} is called with the message:
#'
#'   glue::glue("Of \code{ref.input} did not find 'missing' in \code{ref.against}.")
#'
#' @export

check_vector <- function(input,
                         against,
                         ref.input = "input vector",
                         ref.against = "valid options",
                         ref.connect = "among",
                         fdb.fn = "message",
                         verbose = TRUE,
                         with.time = FALSE){

  base::stopifnot(base::is.vector(input) & base::is.vector(against))
  base::stopifnot(base::class(input) == base::class(against))

  found <- against[against %in% input]
  missing <- input[!input %in% against]

  if(base::isTRUE(verbose) && base::length(missing) != 0){

    missing <- stringr::str_c(missing, collapse = "', '")

  }

  if(base::length(found) == 0){

    msg <-
      glue::glue("Did not find any element of {ref.input} {ref.connect} {ref.against}.")

    confuns::give_feedback(
      msg = msg,
      fdb.fn = "stop",
      with.time = with.time
    )

  } else if(base::length(missing) != 0){

      msg <-
        glue::glue(
          "Of {ref.input} did not find '{missing}' {ref.connect} {ref.against}.",
          missing = glue::glue_collapse(missing, sep = "', '", last = "' and '"))

      give_feedback(
        msg = msg,
        fdb.fn = fdb.fn,
        verbose = verbose,
        with.time = with.time
      )

  }

  return(input[input %in% found])

}


