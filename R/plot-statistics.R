
#' @title Opens interactive application
#'
#' @export

plot_statistics <- function(df, n.across.subset = 10){

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




