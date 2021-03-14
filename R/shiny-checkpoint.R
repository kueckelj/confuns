#' @title Shiny feedback messages
#'
#' @description Wrapper around \code{shiny::req()} and \code{shiny::showNotification()}.
#' Prevents application from crashing and displays guiding message about what the user
#' is supposed to do in order to continue without this message to appear.
#'
#' @param evaluate A vector of logical tests to be evaluated.
#' @param case_false A character string indicating the message to be displayed if one element of
#' \code{evaluate} turns out to be FALSE. Needs to be in \code{base::names(\code{error/warning_notifiations})}.
#' @param error_notifications A named list of character strings.
#' @param warning_notifications A named list of character strings.
#' @param duration The duration the message is displayed.
#' @param stop_process,stop_app Logical. What is supposed to happen if one element of \code{evaluate}
#' turns out to be FALSE.
#'
#' @return A shiny notification.
#'
checkpoint <- function(evaluate = TRUE,
                       case_false = NULL,
                       error_notifications = feedback_list,
                       warning_notifications = list(),
                       duration = 4,
                       stop_process = TRUE,
                       stop_app = FALSE){

  ##-- check if truthy for all elements
  results <- shiny::isTruthy(evaluate)

  if(base::any(results == F)){##-- at least one of the elements is not truthy

    if(!base::is.null(case_false) & case_false %in% base::names(warning_notifications)){

      ##-- show notification
      shiny::showNotification(ui = warning_notifications[[case_false]], duration = duration, closeButton = TRUE, type = "warning")

    } else if(!is.null(case_false) & case_false %in% names(error_notifications)){

      ##-- show notification
      shiny::showNotification(ui = error_notifications[[case_false]], duration = duration, closeButton = TRUE, type = "error")

      ##-- stop computation and or stop app?
      if(base::isFALSE(stop_app) & base::isTRUE(stop_process)){

        shiny::req(evaluate)

      } else if(base::isTRUE(stop_app)) {

        shiny::stopApp()

      }

    }

  }

}
