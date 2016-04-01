#' Shiny Visualisation UI
#'
#' Creates a shiny UI for the interactive web app for the running the IncucyteDRC workflow
#'
#' @return A shiny UI
#' @export
#' @import shiny

shinyVisUI <- function() {

    navbarPage(title = "IncucyteDRC",
               tabPanel("Input",
                        sidebarLayout(
                            sidebarPanel(
                                fileInput('platemap_file', 'Choose platemap file',
                                          accept=c('text/csv',
                                                   'text/comma-separated-values',
                                                    'text/plain',
                                                   '.Platemap')),
                                fileInput('data_file', 'Choose data file',
                                          accept=c('text/csv',
                                                   'text/comma-separated-values',
                                                   'text/plain',
                                                   'txt')),
                                hr(),
                                conditionalPanel(
                                    condition = "input.cut_time_mode == false",
                                    sliderInput('cut_time_slider', 'Specify cut time', 1,300, 175)
                                    ),
                                conditionalPanel(
                                    condition = "input.cut_time_mode == true",
                                    sliderInput('baseline_time_slider', 'Specify baseline time', 1,100, 24),
                                    sliderInput('max_val_slider', 'Specify maximum value', 20,100, 80),
                                    sliderInput('no_doublings_slider', 'Specify # doublings', 1,6, 4, 0.1)
                                    ),
                                checkboxInput('cut_time_mode', 'Calculate cut time', value=FALSE),


                                selectInput('group_columns_select', 'Select group columns',
                                            choices=c('growthcondition', 'celltype', 'passage', 'seedingdensity'),
                                            selected='growthcondition',
                                            multiple=TRUE,
                                            selectize=TRUE)
                                ),
                            uiOutput('mainpage_ui')

                        )),
               tabPanel("Data",
                        selectInput('data_format_select', 'Select data format',
                                    choices=c('Data Frame' = 'dataframe',
                                              'PRISM' = 'prism'),
                                    selected='dataframe',
                                    multiple=FALSE,
                                    selectize=FALSE),
                        uiOutput('datapage_ui')
                        ),
               tabPanel("Help", "contents"))


}
