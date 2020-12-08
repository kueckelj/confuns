

#' @title Construct a flexible function call
#'
#' @description Programming aid: See details for more information on how to use it.
#'
#' @param fn.name Character value. Denotes the function to be called.
#' @param fn.namespace Character value. Denotes the namespace/package from which to call \emph{fn.name}.
#' @param default_list A named list of arguments that can not be specified by the user.
#' @inherit verbose params
#' @param value.fail The return value in case the function call results in an error.
#' @param value.skip The return value in case of \code{fn.name} is specified as FALSE by the user..
#'
#' @return The return value of \emph{fn.name}.
#'
#' @details This function takes two strings as input denoting
#' the function to be called and the namespace from which it is to be called. It expects
#' an object of the same name as \emph{fn.name} to be in it's calling environment specified with an identically named
#' argument from the user of the function from which \code{call_flexibly()} is called.
#'
#' If that object is a list all named elements of that list are considered to be arguments with
#' which function \emph{fn.name} is to be called (= specified list). The names of that list are compared to the names
#' of \code{default_list}. Arguments specified in the default list can not be altered and are discarded from
#' the specified list with an informative warning. Subsequently the names of all remaining arguments are compared
#' to the valid arguments of the function to be called and discarded if unused arguments appear in order to
#' prevent the function call from failing. (This does not happen if the function to be called uses the dot-product '...').
#'
#' If that object is a single TRUE the \code{fn.name} is called with \code{default_list} as input.
#'
#' If that object is anything else the function call is skipped.
#'
#' @export
#'
#' @examples # Not run:
#'
#'  example_fun <- function(plot, runif){
#'
#'    call_flexibly(fn.name = "plot",
#'                  fn.namespace = "base",
#'                  default_list = list(x = 1:10),
#'                  value.fail = "This failed.",
#'                  value.skip = "Okey, I skip that."
#'                  )
#'
#'    call_flexibly(fn.name = "runif",
#'                  fn.namespace = "stats",
#'                  default_list = list(n = 100),
#'                  value.fail = "This failed.",
#'                  value.skip = 1:100)
#'
#'  }
#'
#'
#'  # call the function
#'
#'  example_fun(plot = list(y = 1:10, cex = 5, col = "red"), runif = list(max = 100, min = 1))
#'

call_flexibly <- function(fn.name,
                          fn.namespace,
                          default_list = list(),
                          verbose = TRUE,
                          value.fail = NULL,
                          value.skip = NULL){

  # check input
  are_values("fn.name", "fn.namespace", mode = "character")

  # check if namespace was specified
  if(fn.namespace != ""){

    fn_with_namespace <-
      stringr::str_c( fn.namespace, fn.name, sep = "::")

  } else {

    fn_with_namespace <- fn.name

  }


  # object of class function - the function to be called
  fn_to_call <-
    base::parse(text = fn_with_namespace) %>%
    base::eval()

  # list of not changeable arguments
  default_args <- keep_named(input = default_list)


  # the specified input that determines how to proceed
  ce <- rlang::caller_env()

  input <-
    base::parse(text = fn.name) %>%
    base::eval(envir = ce)


  # ----- Option 1: if a list was specified with arguments

  if(base::is.list(input) & !base::is.data.frame(input)){

    # vector of argument names in the customizable list
    customized_args <- keep_named(input)
    names_customized_args <- base::names(input)

    # vector of argument names in the default list
    names_default_args <- base::names(default_args)

    # discard customized arguments that are not allowed to be changed
    discard <-
      base::intersect(names_customized_args, names_default_args)

    if(base::length(discard) >= 1){

      customized_args <-
        purrr::discard(.x = customized_args,
                       .p = names_customized_args %in% discard)

      ref_arguments <- adapt_reference(input = discard, sg = "Argument")
      ref_verb <- adapt_reference(input = discard, sg = "is", pl = "are")

      ref_discard <- stringr::str_c(discard, collapse = "', '")

      base::warning(glue::glue("{ref_arguments} '{ref_discard}' of function '{fn_with_namespace}()' {ref_verb} fixed and can not be changed."))

    }

    # list of all arguments
    all_args <- base::append(x = default_args, values = customized_args)

    # vector of all argument names
    names_all_args <- base::names(all_args)

    # vector of actually valid argument names
    names_valid_args <- rlang::fn_fmls_names(fn = fn_to_call)

    # if the function to call does not contain ... make sure that no unused
    # arguments appear in the list of arguments (if arguments were provided)
    if(!"..." %in% names_valid_args & !base::is.null(names_all_args)){

      names_all_args_valid <-
        check_vector(
          input = names_all_args,
          against = names_valid_args,
          verbose = TRUE,
          ref.input = base::as.character(glue::glue("specified argument list for function '{fn_with_namespace}()'")),
          ref.against = base::as.character(glue::glue("it's valid arguments (ignoring)"))
        )

      # final argument list
      all_args <- all_args[names_all_args_valid]

    }

    # call the function with the respective arguments

    result <- base::tryCatch(

      rlang::invoke(.fn = fn_to_call, .args = all_args),

      error = function(error){

        base::message(glue::glue("Attempting to call function '{fn_with_namespace}()' with specified parameters failed with the following error message: {error} "))

        base::return(value.fail)

      }

    )


  # ----- Option 2: if only TRUE was given as input
  } else if(base::isTRUE(input) | base::is.function(input)){

    result <-
      base::tryCatch(

        rlang::invoke(.fn = fn_to_call, .args = default_args),

        error = function(error){

          base::message(glue::glue("Attempting to call function '{fn_with_namespace()}' failed with the following error message: {error} "))

          base::return(value.fail)

        }

      )

  # ----- Option 3: if only FALSE or anything else was given as input
  } else {

    if(base::isTRUE(verbose)){ base::message(glue::glue("Skipping function '{fn_with_namespace}()'."))}

    result <- value.skip

  }

  base::return(result)

}


#' @title Assign objects to environments
#'
#' @description Programming aid: See details for more information on how to use it.
#'
#' @param ... Named lists provided as named arguments.
#' @inherit verbose params
#'
#' @details Works in combinaiton with \code{call_flexibly}. Provided named lists of \code{...}
#' are joined to one overall list whoose slots are the argument lists for all flexibly called
#' functions and whoose names correspond to the function names. It calls \code{base::assign()}
#' within \code{purrr::imap()} to assign every argument list to the environment of the calling
#' function in order that all flexible calls find their argument lists.
#'
#' @export
#'
#' @examples # Not run:
#'
#'   example_fun <- function(...){
#'
#'       make_available(...)
#'
#'       ggplot(data = mtcars, mapping = aes(x = wt, y = hp)) +
#'        call_flexibly(fn.name = "geom_point", fn.namespace = "ggplot2") +
#'        call_flexibly(fn.name = "theme", fn.namespace = "ggplot2")
#'
#'   }
#'
#'
#'   gp_list <- list(color = "red", size = 4, shape = 15)
#'   theme_list <- list(panel.background = element_rect(fill = "red", colour = "black"))
#'
#'    example_fun(geom_point = gp_list, theme = theme_list)
#'

make_available <- function(..., verbose = TRUE){

  named_list <-
    keep_named(input = list(...)) %>%
    purrr::keep(.p = is_list) %>%
    purrr::keep(.p = is_named)

  ce <- rlang::caller_env()

  purrr::imap(.x = named_list,
              caller_environment = ce,
              .f = function(fn.list, fn.name, caller_environment){

                base::assign(x = fn.name,
                             value = fn.list,
                             envir = caller_environment)

              })

}

