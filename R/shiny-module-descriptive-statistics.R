
#' Title
#'
#' @param df
#' @param n.across.subset
#'
#' @return
#' @export
#'
#' @examples
plot_descriptive_statistics_interactive <- function(df, n.across.subset = 10){

  shiny::runApp(
    shiny::shinyApp(
      ui = function(){
        shinydashboard::dashboardPage(

          header = shinydashboard::dashboardHeader(title = "Descriptive Statistics"),

          sidebar = shinydashboard::dashboardSidebar(
            collapsed = TRUE,
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                text = "Descriptive Statistisc",
                tabName = "descr_statistics",
                selected = TRUE
              )
            )
          ),

          body = shinydashboard::dashboardBody(

            shinybusy::add_busy_spinner(spin = "cube-grid", margins = c(0, 10), color = "red"),

            shinydashboard::tabItems(

              shinydashboard::tabItem(tabName = "descr_statistics",

                                      moduleDescrStatPlotUI(id = "ds")

                                      )

            )

          )

        )

      },
      server = function(input, output, session){

        # shiny helper
        shinyhelper::observe_helpers()

        moduleDescrStatPlotServer(id = "ds",
                                  df = df,
                                  n.across.subset = n.across.subset)

      }

    )
  )


}





moduleDescrStatPlotUI <- function(id, module_width = 6, module_headline = "Descriptive Statistics"){

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::column(width = module_width,
                  shiny::wellPanel(
                    shiny::fluidRow(
                      shiny::column(width = 12,
                                    shiny::h4(shiny::strong(module_headline)),
                                    shiny::plotOutput(outputId = ns("module_plot")),
                                    shiny::HTML("<br>"),
                                    plot_and_save(ns = ns),
                                    shiny::HTML("<br>"),
                                    shiny::fluidRow(
                                      hs(4,shiny::uiOutput(outputId = ns("change_order")))
                                    ),
                                    shiny::HTML("<br><br>"),
                                    shiny::fluidRow(
                                      hs(4,shiny::uiOutput(outputId = ns("across"))),
                                      hs(4,shiny::uiOutput(outputId = ns("across_subset"))),
                                      hs(4,shiny::uiOutput(outputId = ns("variables"))),
                                    ),
                                    shiny::fluidRow(
                                      hs(4,shiny::uiOutput(outputId = ns("plot.type"))),
                                      hs(4,shiny::uiOutput(outputId = ns("clrp")))
                                    ),
                                    shiny::uiOutput(outputId = ns("point_add_on")),
                                    shiny::uiOutput(outputId = ns("statistics"))
                      )
                    )
                  )
    )
  )

}

moduleDescrStatPlotServer <- function(id, df, n.across.subset = 10){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){


      # Reactive values ---------------------------------------------------------

      input_df <- shiny::reactive({ df })

      # -----

      # Render UIs --------------------------------------------------------------

      output$variables <- shiny::renderUI({

        ns <- session$ns

        num_variables <-
          dplyr::select_if(input_df(), .predicate = base::is.numeric) %>%
          base::colnames()

        shinyWidgets::pickerInput(inputId = ns("variables"),
                                  label = "Include Variables:",
                                  choices = variables,
                                  selected = variables,
                                  options = list(`actions-box`= TRUE),
                                  multiple = TRUE)

      })

      output$across <- shiny::renderUI({

        ns <- session$ns

        across_groups <-
          across_options(df, n.across.subset = n.across.subset)

        shiny::req(across_groups)

        across_picker_input(ns = ns, choices = across_groups)

      })

      output$across_subset <- shiny::renderUI({

        ns <- session$ns

        shiny::req(input$across)

        across_subset_options <-
          across_subset_options(df = input_df(), across = input$across)

        across_subset_picker_input(ns = ns,
                                   choices = across_subset_options,
                                   selected = across_subset_options)

      })

      output$clrp <- shiny::renderUI({

        ns <- session$ns

        colorpanel_picker_input(ns = ns, id = "clrp")

      })


      output$change_order <- shiny::renderUI({

        ns <- session$ns

        shiny::req(input$across_subset)

        change_order_input(ns = ns, items = input$across_subset)

      })

      output$plot.type <- shiny::renderUI({

        ns <- session$ns

        shinyWidgets::pickerInput(inputId = ns("plot.type"),
                                  label = "Plottype:",
                                  choices = pretty_plottypes,
                                  selected = "boxplot",
                                  multiple = FALSE)

      })

      # --- Point add on

      output$point_add_on <- shiny::renderUI({

        ns <- session$ns

        shiny::req(input$plot.type %in% testable_plottypes)

        across_groups <-
          across_options(df, n.across.subset = n.across.subset)

        shiny::tagList(
          shiny::fluidRow(

            hs(2,
               shinyWidgets::radioGroupButtons(
                                          inputId = ns("display.points"),
                                          label = "Display Points:",
                                          choices = c("Yes" = "yes", "No" = "no"),
                                          selected = "no"
                                          )
              ),
            hs(2,
               shinyWidgets::pickerInput(
                 inputId = ns("shape.to"),
                 label = "Shape to:",
                 choices = c("None" = "none", across_groups),
                 selected = "none")
               ),
            hs(2, color_picker_input(ns = ns, id = "pt.color")),
            hs(3, slider_input_alpha(ns = ns, id = "pt.alpha")),
            hs(3, slider_input_size(ns = ns, id = "pt.size")),


          )
        )

      })


      # --- Statistical part

      output$statistics <- shiny::renderUI({

        ns <- session$ns

        shiny::validate(
          shiny::need(
            expr = input$plot.type %in% testable_plottypes,
            message = "Choose Box- or Violinplot to perform statistical tests."
          )
        )

        shiny::validate(
          shiny::need(
            expr = base::length(input$across_subset) > 1,
            message = "At least two groups are needed to perform statistical tests."
          )
        )

        shiny::validate(
          shiny::need(
            expr = base::length(input$variables) == 1,
            message = "Exactly one variable is needed to perform statistical tests."
          )
        )

        shiny::tagList(
          shiny::fluidRow(
            hs(width = 8,
               shinyWidgets::radioGroupButtons(
                 inputId = ns("test_options_pairwise"),
                 label = "Pairwise Test:",
                 choices = pretty_stattests_pairwise,
                 selected = "none",
                 justified = TRUE)
            ),
            hs(width = 4,
               shinyWidgets::pickerInput(
                 inputId = ns("ref.group"),
                 label = "Reference Group:",
                 choices = input$across_subset,
                 multiple = FALSE)
            )
          ),
          shiny::fluidRow(
            hs(width = 8,
               shinyWidgets::radioGroupButtons(
                 inputId = ns("test_options_groupwise"),
                 label = "Groupwise Test:",
                 choices = pretty_stattests_groupwise,
                 selected = "none",
                 justified = TRUE)
            )
          )
        )


      })

      # ---


      # -----



      # Reactive expressions ----------------------------------------------------

      #!!! awkward solution to the problem that input$change_order_order changes it's class somehow
      across_subset_ordered <- shiny::reactive({

        hlpr_order_input(input$change_order_order)

      })

      module_plot <- shiny::eventReactive(input$update_plot, {

        print(input$shape.to)

        plot_descriptive_statistics(
          df = input_df(),
          variables = input$variables,
          across = input$across,
          across.subset = across_subset_ordered(),
          test.pairwise = input$test_options_pairwise,
          test.groupwise = input$test_options_groupwise,
          clrp = input$clrp,
          display.points = base::ifelse(input$display.points == "yes", TRUE, FALSE),
          pt.size = input$pt.size,
          pt.alpha = input$pt.alpha,
          pt.color = input$pt.color,
          shape.to = input$shape.to,
          n.obs = 100,
          ref.group = input$ref.group,
          plot.type = input$plot.type,
          give_feedback = list(in.shiny = TRUE),
          verbose = FALSE
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
