#' @title Show shiny - notifications
#'
#' @param in.shiny Logical value.
#' @param ui Given to \code{shiny::showNotification()}.
#' @param type Given to \code{shiny::showNotification()}.
#' @param ... More arguments given \code{shiny::showNotification()}.
#'
#' @return A shiny notification.

shiny_fdb <- function(in.shiny, ui, type = "message", ...){

  if(base::isTRUE(in.shiny)){

    shiny::showNotification(ui = ui, type = type, ...)

  }

}



#' Title
#'
#' @param ns
#'
#' @return
#'
#' @examples
plot_and_save <- function(ns){

  shiny::tagList(
    shiny::fluidRow(
      hs(4,shiny::h5(shiny::strong("Plot:")),
         shiny::actionButton(inputId = ns("update_plot"), label = "Update")),
      hs(4,shiny::h5(shiny::strong("Save:")),
         shiny::downloadButton(outputId = ns("save_as_pdf"), label = "PDF"))
    )
  )

}


#' @title Horizontal Separation (width = 3)

hs <- function(width = 3, ..., offset = 0){

  shiny::column(width = width, ..., offset = offset)

}


#' Title
#'
#' @param ns
#' @param id
#'
#' @return
#'

slider_input_alpha <- function(ns, id = "pt.alpha"){

  shiny::sliderInput(
    inputId = ns(id),
    label = "Transperancy:",
    min = 0.01,
    max = 1,
    step = 0.01,
    value = 0.9
  )


}

#' @rdname slider_input_alpha
slider_input_size <- function(ns,
                              id = "pt.size",
                              min = 0.1,
                              max = 5,
                              step = 0.1,
                              value = 1.2){

  shiny::sliderInput(
    inputId = ns("pt.size"),
    label = "Size:",
    min = min,
    max = max,
    step = step,
    value = value
  )

}



