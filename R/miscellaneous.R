#' @title Adapt glue reference
#'
#' @description Switch between plural or singular reference.
#'
#' @param input Vector to be checked.
#' @param sg Character value to return if length of \code{input} is 1.
#' @param pl Character value to return if length of \code{input} is > 1.
#' If set to NULL an \emph{'s'} is attached to in put of \code{sg}.
#' @param zero Character value to treturn if lengt of \code{input} is 0.
#'
#' @return Either sg or pl.
#' @export

adapt_reference <- function(input, sg, pl = NULL, zero = ""){

  if(base::length(input) == 1){

    base::return(sg)

  } else if(base::length(input) >= 1){

    if(base::is.null(pl)){

      pl <- stringr::str_c(sg, "s", sep = "")

    }

    base::return(pl)

  } else {

    base::return(zero)

  }

}

# helper within plot_dotplot_2d
arrange_axis <- function(df, grouping.var, arrange.var, arrange.by, reverse.within, reverse.all){

  groups <- base::levels(df[[grouping.var]])

  order_labels <- base::character()

  for(g in groups){

    labels_df <-
      dplyr::filter(df, !!rlang::sym(grouping.var) == {{g}})

    if(base::is.character(arrange.by)){

      if(base::isTRUE(reverse.within)){

        labels_df <-
          dplyr::arrange(labels_df, dplyr::desc(!!rlang::sym(arrange.by)))


      } else {

        labels_df <-
          dplyr::arrange(labels_df, !!rlang::sym(arrange.by))

      }

    }

    labels <-
      dplyr::pull(labels_df, {{arrange.var}}) %>%
      base::as.character()

    # prevent duplicates
    labels <- labels[!labels %in% order_labels]

    order_labels <- c(order_labels, labels)

  }

  order_labels <- base::unique(order_labels)

  if(base::isTRUE(reverse.all)){

    order_labels <- base::rev(order_labels)

  }

  df[[arrange.var]] <- base::factor(x = df[[arrange.var]], levels = order_labels)

  return(df)

}

#' @title Assign objects into the global environment
#'
#' @param assign Logical.
#' @param object The object to be assigned.
#' @param name The name of the assigned object.
#'

assign_obj <- function(assign, object, name){

  if(base::isTRUE(assign)){

    base::assign(
      x = name,
      value = object,
      envir = .GlobalEnv
    )

  }

}


#' @export
info_deprecated <- function(x, alternative, test.val = NA){

  ref <- base::substitute(expr = x)

  if(!base::identical(x, test.val)){

    msg <-
      glue::glue(
        "Argument '{ref}' is deprecated. Please use argument '{alternative}' instead."
      )

    give_feedback(msg = msg, fdb.fn = "warning", with.time = FALSE)

  }

  invisible(TRUE)

}


#' @title Return function
#'
#' @param fun Character value. One of \emph{'message', 'warning', 'stop'}.
#'
#' @return The respective function
#'

error_handler <- function(fun){

  is_value(x = fun, mode = "character", ref = "fun")
  base::stopifnot(fun %in% c("message", "warning", "stop"))

  if(fun == "messsage"){

    base::message

  } else if(fun == "warning"){

    base::warning

  } else if(fun == "stop"){

    base::stop

  }

}


#' @title Glue a human readable list report
#'
#' @description Combines all slots of the specified list in \code{lst}
#' that are values to a character/glue object.
#'
#' @param lst A named list of values.
#' @param separator Character value or NULL. Denotes the string with
#' which the slot name is combined with the slot's content. If set to
#' NULL neither the slot names nor the separators are mentioned and
#' the slot's contents are combined as they are.
#' @param prefix Character value. Denotes the string with which to prefix
#' each slots.
#'
#' @return Glue object.
#' @export
#'
#' @examples #Not run:
#'
#'  lst_input <- list("arg1" = TRUE, "arg2" = glue::glue_collapse(1:5, sep = ", "))
#'
#'  glue_list_report(lst = lst_input, separator = " = ")
#'

glue_list_report <- function(lst, prefix = "", separator = " = ", combine_via = "\n", ...){

  lst <- purrr::keep(.x = keep_named(lst), .p = function(x){

    res <- base::vector()

    res[1] <- base::length(x) == 1
    res[2] <- !base::is.list(x)

    res_final <- base::all(res)

    return(res_final)

  })

  report <- base::vector(mode = "character", length = base::length(lst))

  for(slot in base::names(lst)){

    ref_slot <- base::ifelse(base::is.character(separator), slot, "")

    report[slot] <-
      stringr::str_c(combine_via, prefix, ref_slot, separator, base::as.character(lst[[slot]]))

  }

  glue::glue_collapse(report)

}

#' @title Pull var safely
#' @export
pull_var <- function(df, var){

  if(base::is.null(var)){

    out <- NULL

  } else {

    out <- df[[var]]

  }

  return(out)

}


#' @title Wrapper around unfactor()
#'
#' @description If input is a factor it will be returned as
#' a character vector.
#'
#' @param input A vector.
#' @param ... Additional arguments given to s4vctrs::unfactor().
#'
#' @return
#' @export
#'

unfactor <- function(input, ...){

  if(base::is.factor(input)){

    input <- S4Vectors::unfactor(input, ...) %>%
      base::as.character()

  }

  base::return(input)

}



# v -----------------------------------------------------------------------


#' @title Obtain valid input options
#'
#' @description These functions return all valid input options for
#' specific arguments.
#'
#' @return Character vector.
#' @export
#'
validInput <- function(){

  NULL

}

#' @rdname validInput
#' @export
#'
validCurves <- function(){ return(valid_curves) }



