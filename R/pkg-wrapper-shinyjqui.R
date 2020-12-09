

#' Title
#'
#' @param ns
#' @param id
#' @param items
#' @param item_class
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
change_order_input <- function(ns,
                               id = "change_order",
                               items,
                               item_class = "default",
                               ...){

  shinyjqui::orderInput(
    inputId = ns(id),
    label = "Change Order:",
    items = items,
    item_class = "default"
  )

}

#' @title Work around
#'
#' @description Awkward solution to the problem that
#' input$change_order_order (of shinyjqui::orderInput()) somehow changes it's class

hlpr_order_input <- function(order_input){

  if(base::is.data.frame(order_input)){

    order <- order_input$text

  } else if(base::is.character(order_input)){

    order <- order_input

  }

  base::return(order)

}
