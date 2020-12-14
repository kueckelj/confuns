moduleCategoricalStatisticsPlotUI <- function(id, module_width = 6, module_headline = "Categorical Statistics"){

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::column(width = module_width,
                  shiny::wellPanel(
                    shiny::fluidRow(
                      shiny::column(width = 12,
                                    shiny::h4(shiny::strong(module_headline)),
                                    shiny::plotOutput(outputId = ns("module_plot")),
                                    html_breaks(1),
                                    plot_and_save(ns = ns),
                                    html_breaks(1),
                                    shiny::fluidRow(
                                      hs(12,shiny::uiOutput(outputId = ns("change_order")))
                                    ),
                                    html_breaks(2),
                                    shiny::fluidRow(
                                      hs(4,shiny::uiOutput(outputId = ns("across"))),
                                      hs(4,shiny::uiOutput(outputId = ns("across.subset"))),
                                      hs(4,shiny::uiOutput(outputId = ns("categorical_variables")))
                                    ),
                                    shiny::fluidRow(
                                      hs(4, shiny::uiOutput(outputId = ns("bar_position"))),
                                      hs(4, shiny::uiOutput(outputId = ns("clrp")))
                                    )
                      )
                    )
                  )
    )
  )

}



moduleCategoricalStatisticsPlotServer <- function(id, df, n.across.subset){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      # Reactive values ---------------------------------------------------------

      input_df <- shiny::reactive({ df })

      # -----

      # Render UIs --------------------------------------------------------------

      output$across <- shiny::renderUI({

        ns <- session$ns

        across_choices<-
          across_options(df = input_df(), n.across.subset = n.across.subset)

        shiny::req(across_choices)

        across_picker_input(ns = ns,
                            choices = across_choices,
                            selected = across_choices[1])

      })

      output$across.subset <- shiny::renderUI({

        ns <- session$ns

        shiny::req(input$across)

        across_subset_options <-
          across_subset_options(df = input_df(), across = input$across)

        across_subset_picker_input(ns = ns,
                                   id = "across.subset",
                                   choices = across_subset_options,
                                   selected = across_subset_options)

      })

      output$bar_position <- shiny::renderUI({

        ns <- session$ns

        bar_position_picker_input(ns = ns)

      })

      output$categorical_variables <- shiny::renderUI({

        ns <- session$ns

        ctg_variables <-
          across_options(df = input_df(), n.across.subset = n.across.subset)

        ctg_variables <-
          purrr::keep(.x = ctg_variables, .p = ~ !.x %in% input$across)

        include_variables_picker_input(ns = ns,
                                       id = "categorical_variables",
                                       choices = ctg_variables,
                                       selected = ctg_variables)

      })

      output$change_order <- shiny::renderUI({

        ns <- session$ns

        shiny::req(input$across.subset)

        change_order_input(ns = ns, items = input$across.subset)

      })

      output$clrp <- shiny::renderUI({

        ns <- session$ns

        colorpanel_picker_input(ns = ns, id = "clrp")

      })

      # -----



      # Reactive expressions ----------------------------------------------------

      across_subset_ordered <- shiny::reactive({

        hlpr_order_input(input$change_order_order)

      })

      module_plot <- shiny::eventReactive(input$update_plot, {

        checkpoint(evaluate = base::length(input$categorical_variables) != 0,
                   case_false = "no_features_selected")

        plot_categorical_statistics(
          df = input_df(),
          variables = input$categorical_variables,
          across = input$across,
          across.subset = across_subset_ordered(),
          clrp = input$clrp,
          position = input$bar_position
        )

      })


      # -----


      # Plot outputs ------------------------------------------------------------

      output$module_plot <- shiny::renderPlot({

        shiny::req(module_plot())

        module_plot()

      })

      output$save_as_pdf <- shiny::downloadHandler(
        filename = function(){
          base::paste("name", ".pdf", sep = "")
        },
        content = function(file){
          grDevices::pdf(file)
          plot(module_plot())
          grDevices::dev.off()
        },
        contentType = "application/pdf"
      )


    }
  )


}
