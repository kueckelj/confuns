

#' @title Picker inputs
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
across_subset_picker_input <- function(ns,
                                       id = "across_subset",
                                       choices,
                                       selected,
                                       multiple = TRUE,
                                       ...){

  shinyWidgets::pickerInput(inputId = ns(id),
                            label = "Subset Groups:",
                            choices = choices,
                            multiple = multiple,
                            selected = selected)

}


#' @rdname across_picker_input
colorpanel_picker_input <- function(ns, id = "pt_clrp"){

  shinyWidgets::pickerInput(inputId = ns(id),
                            choices = pretty_colorpanels_list,
                            label = "Color Panel:",
                            multiple = FALSE,
                            selected = "milo")

}


#' @rdname across_picker_input
color_picker_input <- function(ns, id = "pt_color"){

  shinyWidgets::pickerInput(
    inputId = ns(id),
    label = "Color:",
    choices = grDevices::colors(),
    selected = "black"
  )

}

#' @rdname across_picker_input
bar_position_picker_input <- function(ns, id = "bar_position", selected = "fill", ...){

  shinyWidgets::pickerInput(inputId = ns(id),
                            label = "Bar Position:",
                            choices = pretty_bar_positions,
                            selected = selected,
                            ...)

}


#' @rdname across_picker_input

include_variables_picker_input <- function(ns, id = "variables", choices, selected, options = list()){

  shinyWidgets::pickerInput(inputId = ns(id),
                            label = "Include Variables:",
                            choices = choices,
                            selected = selected,
                            options = base::append(list(`actions-box`= TRUE), values = options),
                            multiple = TRUE)

}

