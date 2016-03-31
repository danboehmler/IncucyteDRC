#' Shiny Visualisation Server
#'
#' Creates a shiny server for the interactive web app for the running the IncucyteDRC workflow
#'
#' @param input Shiny input list
#' @param output Shiny output list
#' @param con A \code{SQLiteConnection} object to the database
#' @return A shiny server
#' @export
#' @import shiny
shinyVisServer <- function(input, output) {

    test_pm <- reactive({importPlatemapXML(input$platemap_file$datapath)})
    test_data <- reactive({importIncucyteData(input$data_file$datapath, metric='pc')})
    test_list <- reactive({
        splitIncucyteDRCPlateData(test_pm(), test_data(), group_columns='growthcondition')[[1]] %>%
            fitGrowthCurvesGrouped() %>%
            fitGrowthCurvesIndividual()
        })

    output$plot <- renderPlot({
        plotIncucyteDRCSet(test_list(), grouped=TRUE)
    })


}
