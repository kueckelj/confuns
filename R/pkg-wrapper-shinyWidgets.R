

#' @title
#'
#' @param ns
#' @param id
#' @param choices
#' @param selected
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
across_picker_input <- function(ns,
                                id = "across",
                                choices,
                                selected = "condition",
                                ...){

  shinyWidgets::pickerInput(inputId = ns("across"),
                            label = "Compare across:",
                            choices = choices,
                            selected = selected,
                            ...)

}

#' @rdname across_picker_input
#' @export
across_subset_picker_input <- function(ns,
                                       id = "across_subset",
                                       choices,
                                       selected,
                                       multiple = TRUE,
                                       ...){

  shinyWidgets::pickerInput(inputId = ns(id),
                            label = "Subset groups:",
                            choices = choices,
                            multiple = multiple,
                            selected = selected)

}




#' @rdname across_picker_input
#' @export
colorpanel_picker_input <- function(ns, id = "pt_clrp"){

  shinyWidgets::pickerInput(inputId = ns(id),
                            choices = pretty_colorpanels_list,
                            label = "Color panel:",
                            multiple = FALSE,
                            selected = "milo")

}


#' @rdname across_picker_input
#' @export
color_picker_input <- function(ns, id = "pt_color"){

  shinyWidgets::pickerInput(
    inputId = ns(id),
    label = "Color:",
    choices = grDevices::colors(),
    selected = "black"
  )

}
