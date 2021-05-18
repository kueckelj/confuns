

#' @title Logical naming test
#'
#' @description Returns TRUE if \code{input}'s names evaluate to truthy.
#'
#' @param input R object that is to be checked.
#'
#' @export

is_named <- function(input){

  names <- base::names(input)

  shiny::isTruthy(names)

}



#' @title Discard unnamed elements
#'
#' @description Makes sure that all elements of \code{input} are
#' named.
#'
#' @param input R object that is to be checked.
#'
#' @export

discard_unnamed <- function(input){

  input[purrr::map_lgl(.x = base::names(input), .f = ~ shiny::isTruthy(.x))]

}


#' @rdname discard_unnamed
#' @export
keep_named <- function(input){

  input[purrr::map_lgl(.x = base::names(input), .f = ~ shiny::isTruthy(.x))]

}



#' @title Discard values
#'
#' @param input Vector that is to be checked.
#' @param one_of Vector that contains the values that must not be inside input for
#' argument \code{input}.
#'
#' @export

discard_if <- function(input,
                       one_of = NULL,
                       ref.do = "Ignoring",
                       ref.of = "already present",
                       ref.input = NULL,
                       ref.empty = "Skipping.",
                       v.empty = NULL,
                       with.time = FALSE,
                       verbose = FALSE){

  if(base::is.null(ref.input)){

    default_ref <- base::substitute(input)

    ref.input <- stringr::str_c("input for argument '", default_ref, "'", sep = "")

  }

  class_input <- base::class(input)[1]

  if(base::is.factor(input)){

    input_values <- base::levels(input)

  } else if(base::is.character(input)){

    input_values <- base::unique(input)

  }


  if(!base::is.null(one_of)){

    if(base::is.factor(one_of)){

      one_of_values <- base::levels(one_of)

    } else if(base::is.character(one_of)){

      one_of_values <- base::unique(one_of)

    }

    discard_values <- input[input %in% one_of]

    input <- input[!input %in% one_of]

    if(base::isTRUE(verbose)){

      msg <- glue::glue("{ref.do} {ref1} '{discard_values}' of {ref.input} as {ref2} {ref3} {ref.of}.",
                        ref1 = adapt_reference(discard_values, sg = "value", pl = "values"),
                        ref2 = adapt_reference(discard_values, sg = "it", pl = "they"),
                        ref3 = adapt_reference(discard_values, sg = "is", pl = "are"),
                        discard_values = glue::glue_collapse(discard_values, sep = "', '", last = "' and '"),
                        ref.of = ref.of)

      give_feedback(msg = msg, with.time = with.time)

    }

    if(base::length(input) == 0){

      msg <- glue::glue("No values left. {ref.empty}.")

      give_feedback(msg = msg, with.time = with.time)

      input <- v.empty

    }

  }


  return(input)

}

