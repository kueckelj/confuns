
#' @title Opens interactive application
#'
#' @inherit df params
#'
#' @export

plot_statistics_interactive <- function(df, n.across.subset = 10){

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

                                      shiny::fluidRow(
                                        moduleDescriptiveStatisticsPlotUI(id = "descr_stat"),
                                        moduleCategoricalStatisticsPlotUI(id = "categ_stat")
                                        )

              )

            )

          )

        )

      },
      server = function(input, output, session){

        # shiny helper
        shinyhelper::observe_helpers()

        moduleDescriptiveStatisticsPlotServer(id = "descr_stat",
                                        df = df,
                                        n.across.subset = n.across.subset)

        moduleCategoricalStatisticsPlotServer(id = "categ_stat",
                                              df = df,
                                              n.across.subset = n.across.subset)

      }

    )
  )

}




# Modules -----------------------------------------------------------------


# descriptive
moduleDescriptiveStatisticsPlotUI <- function(id, module_width = 6, module_headline = "Descriptive Statistics"){

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
                                      hs(4,shiny::uiOutput(outputId = ns("change_order")))
                                    ),
                                    html_breaks(2),
                                    shiny::fluidRow(
                                      hs(4,shiny::uiOutput(outputId = ns("across"))),
                                      hs(4,shiny::uiOutput(outputId = ns("across.subset"))),
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

moduleDescriptiveStatisticsPlotServer <- function(id, df, n.across.subset = 10){

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

        include_variables_picker_input(ns = ns,
                                       id = "variables",
                                       choices = num_variables,
                                       selected = num_variables[1])

      })

      output$across <- shiny::renderUI({

        ns <- session$ns

        across_groups <-
          across_options(df, n.across.subset = n.across.subset)

        shiny::req(across_groups)

        across_picker_input(ns = ns, choices = across_groups)

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

      output$clrp <- shiny::renderUI({

        ns <- session$ns

        colorpanel_picker_input(ns = ns, id = "clrp")

      })


      output$change_order <- shiny::renderUI({

        ns <- session$ns

        shiny::req(input$across.subset)

        change_order_input(ns = ns, items = input$across.subset)

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
            expr = base::length(input$across.subset) > 1,
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
                 choices = input$across.subset,
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

        plot_descriptive_statistics_shiny(
          df = input_df(),
          variables = input$variables,
          across = input$across,
          across.subset = across_subset_ordered(),
          test.pairwise = input$test_options_pairwise,
          test.groupwise = input$test_options_groupwise,
          clrp = input$clrp,
          display.points = base::ifelse(input$display.points == "yes", TRUE, FALSE),
          pt.size = input$pt.size,
          pt.alpha = (1 - input$pt.alpha),
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


# categorical
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
                                       selected = ctg_variables[1])

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

        plot_categorical_statistics_shiny(
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





# Plotting functions ------------------------------------------------------


#' @title Plot descriptive statistics
#'
#' @description Old but working plotting function for shiny application.
#'
#' @param variables Character vector. The names of the numeric variables of interest.
#' @inherit check_across_subset params
#' @param plot.type Character value. One of \emph{'histogram', 'ridgeplot', 'boxplot',
#' 'violinplot'} or \emph{'density'} to visualize the value distribution of those
#' variables specified via the \code{variables} argument.
#'
#' If set to \emph{'boxplot'} or \emph{'violinplot'} and only one variable is
#' specified statistical test can be performed.
#'
#' @param binwidth Numeric value. Only relevant if \code{plot_type} is set to \emph{'histogram'}.
#' @param display.points Logical. If set to TRUE the value distribution of \code{n.cells} is additionally
#' displayed by points.
#' @param n.obs Numeric value.
#' @param shape.to Character value. Denotes the discrete variable to which the shape of the
#' points are mapped. (Ignored if \code{display.points} is set to FALSE.)
#' @param test.pairwise Character value. One of \emph{'none', 't.test', 'wilcox.test'}.
#' @param test.groupwise Character value. One of \emph{'none', 'anova', 'kruskal.test'}.
#' @param ref.group Character value. Denotes the reference group for the statistical tests. Must
#' be one value of the variable specified in \code{across}.
#' @inherit colors params
#' @param ... Additional arguments given to \code{ggplot2::facet_wrap()}.
#' @inherit verbose params
#'

plot_descriptive_statistics_shiny <- function(df,
                                              variables = "all",
                                              across,
                                              across.subset = NULL,
                                              plot.type = "boxplot",
                                              binwidth,
                                              test.pairwise = "none",
                                              test.groupwise = "none",
                                              display.points = FALSE,
                                              pt.size = 1.2,
                                              pt.alpha = 0.8,
                                              pt.color = "black",
                                              n.obs = 100,
                                              shape.to = "none",
                                              ref.group,
                                              clrp = "milo",
                                              ... ,
                                              verbose = TRUE){


  make_available(..., verbose = verbose)

  # 1. Control --------------------------------------------------------------

  df <- dplyr::ungroup(x = df)

  if(!plot.type %in% c("histogram", "density", "ridgeplot", "boxplot", "violinplot")){

    base::stop("Argument 'plot.type' needs to be one of 'histogram', 'density', 'ridgeplot', 'boxplot', 'violinplot'.")

  }

  if(plot.type %in% c("violinplot", "ridgeplot", "boxplot")){

    max_length = 10

  } else {

    max_length = 25

  }

  is_value(clrp, "character", "clrp")

  # check across input
  is_value(across, "character", "across")

  check_data_frame(
    df = df,
    var.class = list(c("character", "factor")) %>% magrittr::set_names(across),
    ref = "df"
  )

  # check variable input
  confuns::is_vec(variables, "character", "variables")

  if(base::all(variables == "all")){

    if(base::isTRUE(verbose)){base::message("Argument 'variables' set to 'all'. Extracting all valid, numeric variables.")}

    variables <- base::colnames(dplyr::select_if(.tbl = df, .predicate = base::is.numeric))

  } else {

    check_list <-
      purrr::map(variables, function(i){c("numeric", "integer")}) %>%
      magrittr::set_names(value = variables)

    confuns::check_data_frame(
      df = df,
      var.class = check_list,
      ref = "df"
    )

    if(base::isTRUE(verbose)){"All specified variables found."}

  }


  # prevent naming issues
  if(base::any(c("variables", "values") %in% c(variables, across))){

    call_flexibly(fn = "give_feedback",
                  fn.ns = "",
                  default = list(msg = "Arguments 'across' and 'variables' must not contain/be equal to 'variables' and/or 'values'.",
                                 type = "error",
                                 fdb.fn = "stop"),
                  v.fail = base::stop("See error-message above.")
    )

  }


  df <- check_across_subset(df = df, across = across, across.subset = across.subset)

  # -----

  # 2. Data extraction ------------------------------------------------------

  df <-
    tidyr::pivot_longer(
      data = df,
      cols = dplyr::all_of(x = variables),
      names_to = "variables",
      values_to = "values"
    )

  reverse <- FALSE

  # -----

  # 3. Display add on -------------------------------------------------------

  # ggplot main
  if(plot.type %in% c("density","histogram")){

    ggplot_main <-
      ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = values))

  } else if(plot.type == "ridgeplot"){

    ggplot_main <-
      ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = values, y = .data[[across]]))

  } else if(plot.type %in% c("violinplot", "boxplot")){

    ggplot_main <-
      ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = .data[[across]], y = values))

  }

  # ggplot geom
  if(plot.type == "histogram"){

    display_add_on <-
      list(
        ggplot2::geom_histogram(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                                color = "black", binwidth = binwidth,
                                data = df),
        ggplot2::labs(y = NULL)
      )

  } else if(plot.type == "density"){

    display_add_on <-
      list(
        ggplot2::geom_density(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                              color = "black", data = df,alpha = 0.825),
        ggplot2::labs(y = "Density")
      )

  } else if(plot.type == "ridgeplot"){

    reverse <- TRUE

    display_add_on <-
      list(
        ggridges::geom_density_ridges(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                                      color = "black", data = df, alpha = 0.825),
        ggplot2::labs(y = across, x = NULL)

      )

  } else if(plot.type == "violinplot"){

    display_add_on <-
      list(
        ggplot2::geom_violin(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                             color = "black", data = df),
        ggplot2::labs(y = NULL, x = across)
      )

  } else if(plot.type == "boxplot"){

    display_add_on <-
      list(
        ggplot2::geom_boxplot(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                              color = "black", data = df),
        ggplot2::labs(y = NULL, x = across)
      )

  }

  # -----


  # 4. Statistic add on -----------------------------------------------------

  max_value <- base::max(df[["values"]], na.rm = TRUE)
  labels_y <- NULL
  n_variables <- dplyr::n_distinct(df[["variables"]])


  # --- pairwise statistics
  if(n_variables == 1 & plot.type %in% testable_plottypes){

    if(test.pairwise %in% c("t.test", "wilcox.test")){

      comparison_list <-
        ggpubr_comparison_list(ref.group = ref.group, groups = base::levels(df[[across]]))

      labels_y <- ggpubr_y_labels(lst = comparison_list, max.value = max_value)

      pairwise_add_on <- list(
        ggpubr::stat_compare_means(
          method = test.pairwise,
          comparisons = comparison_list,
          label.y = labels_y,
          data = df
        )
      )

    } else if(test.pairwise == "none") {

      if(base::isTRUE(verbose)){base::message("Skip pairwise testing.")}

      pairwise_add_on <- list()

    } else if(base::is.character(test.pairwise)){

      base::warning("Invalid input for argument 'test.pairwise'.")

    }

    # --- groupwise statistic
    if(test.groupwise %in% c("anova", "kruskal.test")){

      if(base::is.null(labels_y)){

        label_y <- max_value*1.1

      } else if(base::is.numeric(labels_y)){

        label_y <- base::max(labels_y, na.rm = TRUE)*1.1

      }

      groupwise_add_on <- list(
        ggpubr::stat_compare_means(
          method = test.groupwise,
          label.y = label_y,
          data = df
        )
      )

    } else if(test.groupwise == "none"){

      if(base::isTRUE(verbose)){base::message("Skip groupwise testing.")}

      groupwise_add_on <- list()

    } else {

      base::warning("Invalid input for argument 'test.groupwise'.")

      groupwise_add_on <- list()

    }

  } else {

    pairwise_add_on <- list()
    groupwise_add_on <- list()

    # base::warning("set up confuns warnings")

  }

  # -----


  # 5. Jitter add on  -------------------------------------------------------

  if(base::isTRUE(display.points) & plot.type %in% testable_plottypes){

    jitter_data <-
      dplyr::group_by(.data = df, !!rlang::sym(across)) %>%
      dplyr::slice_sample(n = n.obs)

    if(shape.to != "none"){

      jitter_add_on <-
        ggplot2::geom_jitter(
          data = jitter_data, size = pt.size, alpha = pt.alpha,
          color = pt.color, mapping = ggplot2::aes(shape = .data[[shape.to]])
        )

    } else {

      jitter_add_on <-
        ggplot2::geom_jitter(
          data = jitter_data, size = pt.size, alpha = pt.alpha,
          color = pt.color, height = 0.25, width = 0.25
        )
    }


  } else {

    jitter_add_on <- list()

  }

  # -----


  # 6. Plotting -------------------------------------------------------------

  facet_add_on <-
    call_flexibly(fn = "facet_wrap",
                  fn.ns = "ggplot2",
                  default = list(facets = stats::as.formula(. ~ variables),
                                 scales = "free"),
                  v.fail = ggplot2::facet_wrap(facets = . ~ variables, scales = "free"))

  ggplot_main +
    display_add_on +
    facet_add_on +
    confuns::scale_color_add_on(aes = "fill", variable = "discrete",
                                clrp = clrp, guide = ggplot2::guide_legend(reverse = reverse)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(color = "black"),
      axis.text.x = ggplot2::element_text(color = "black"),
      strip.text.y = ggplot2::element_text(angle = 0, face = "italic", size = 14),
      strip.placement = "outside",
      strip.background = ggplot2::element_rect(color = "white", fill = "white"),
      panel.spacing.y = ggplot2::unit(10, "pt")
    ) +
    ggplot2::labs(x = NULL) +
    pairwise_add_on +
    groupwise_add_on +
    jitter_add_on

}



#' Title
#'

#'
#' @return
#' @export
#'

plot_categorical_statistics_shiny <- function(df,
                                              variables,
                                              across,
                                              across.subset = NULL,
                                              clrp = "milo",
                                              position = "fill",
                                              ...){


  # 1. Control --------------------------------------------------------------


  # ----


  # 2. Additional checks and data extraction -----------------------------------

  plot_df <-
    tidyr::pivot_longer(data = df,
                        cols = dplyr::all_of(variables),
                        names_to = "variables",
                        values_to = "values")

  plot_df <-
    check_across_subset(df = plot_df, across = across, across.subset = across.subset)

  if(base::is.character(across)){

    all_variables <- c(variables, across)

    facet_add_on <-
      ggplot2::facet_wrap(facets = . ~ variables, scales = "free_x", ...)

    fill <- across

    theme_add_on <- list()

  } else {

    all_variables <- variables

    facet_add_on <-
      ggplot2::facet_wrap(facets = . ~ variables, scales = "free_x", ...)

    if(base::length(all_variables) > 1){

      fill = "variables"

    } else {

      fill = "values"

    }

    theme_add_on <- list(ggplot2::theme(legend.position = "none"))

    if(position == "fill" & base::length(all_variables) > 1){

      position <- "stack"

      base::warning("Argument 'across' is NULL. Using 'stack' for argument 'position'.")

    }

  }

  if(position == "fill"){

    scale_y_add_on <-
      list(
        ggplot2::scale_y_continuous(labels = c(25, 50, 75, 100), breaks = c(0.25, 0.5, 0.75, 1)),
        ggplot2::labs(y = "Percentage")
      )

  } else {

    scale_y_add_on <- ggplot2::labs(y = "Count")

  }

  # ----

  ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_bar(position = position, color = "black",
                      mapping = ggplot2::aes(x = values, fill = .data[[fill]])) +
    facet_add_on +
    confuns::scale_color_add_on(aes = "fill", variable = "discrete", clrp = clrp) +
    ggplot2::theme_classic() +
    theme_add_on +
    ggplot2::theme(strip.background = ggplot2::element_blank()) +
    ggplot2::labs(x = "Groups / Clusters") +
    scale_y_add_on

}


