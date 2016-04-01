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

    user_pm <- reactive({importPlatemapXML(input$platemap_file$datapath)})

    user_data <- reactive({importIncucyteData(input$data_file$datapath, metric='pc', plateid=input$data_file$name)})

    res <- reactive({
        idrc_set <- splitIncucyteDRCPlateData(user_pm(), user_data(), group_columns=input$group_columns_select)

        if (class(idrc_set) == 'IncucyteDRCSetList') {
            idrc_set <- idrc_set[[as.numeric(input$list_item)]]
        }

        idrc_set <- idrc_set %>%
            fitGrowthCurvesGrouped() %>%
            fitGrowthCurvesIndividual()

        if (input$cut_time_mode) {
                idrc_set <- idrc_set %>%
                    calculateCutTimeForIDRCSet(baseline_time=input$baseline_time_slider,
                                               no_doublings=input$no_doublings_slider,
                                               max_val=input$max_val_slider) %>%
                    calculateDRCData()
        } else {
            idrc_set <- calculateDRCData(idrc_set, input$cut_time_slider)
        }

        return(idrc_set)



        })

    output$plot <- renderPlot({
        plotIncucyteDRCSet(res(), grouped=TRUE)
    })

    output$metadata <- renderTable({
        res()$metadata
    })

    output$drc_data <- renderTable(({
        exportDRCDataToDataFrame(res(), add_metadata=TRUE)
    }))


}
