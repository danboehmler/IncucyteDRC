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

    res_list <- reactive({
        splitIncucyteDRCPlateData(user_pm(), user_data(), group_columns=input$group_columns_select)
    })

    list_element <- reactive({
        if(is.null(input$metadata_row_last_clicked)) {
            1
        } else {
            as.numeric(input$metadata_row_last_clicked)
        }
    })

    res <- reactive({
        idrc_set <- res_list()

        if (class(idrc_set) == 'IncucyteDRCSetList') {
            #idrc_set <- idrc_set[[as.numeric(input$list_item)]]
            idrc_set <- idrc_set[[list_element()]]
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

    drc_data <- reactive({
        if(input$data_format_select == 'dataframe'){
            exportDRCDataToDataFrame(res(), add_metadata=TRUE)
        } else {
            exportDRCDataToPRISM(res(), add_metadata=TRUE)
        }
    })

    metadata_df <- reactive({
        if(class(res_list()) != 'IncucyteDRCSetList') {
            res()$metadata
        } else {
            lapply(res_list(), function(x) x$metadata) %>% dplyr::bind_rows()
        }
    })

    output$plot <- renderPlot({
        plotIncucyteDRCSet(res(), grouped=TRUE)
    })

    output$metadata <- DT::renderDataTable({
        metadata_df()
    }, filter='none', selection=list(mode = 'single', selected = 1),
        options=list(searching=FALSE, sorting=FALSE, paging=FALSE, info=FALSE, ordering=FALSE))

    output$drc_data <- renderTable({
        drc_data()
    })

    output$download_drc_data <- downloadHandler(
        filename = 'drc_data_download.txt',
        content = function(file) {
            write.table(drc_data(), file=file, sep='\t', row.names = FALSE, col.names = TRUE, na='')
        }
    )


}
