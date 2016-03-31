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
                                sliderInput('cut_time_slider', 'Specify cut time', 1,300, 175),
                                numericInput('list_item', 'Specify set', 1)

                                ),
                            mainPanel(
                                tableOutput('metadata'),

                                plotOutput('plot'),

                                tableOutput('drc_data')
                            )
                        )),
               tabPanel("tab 2", "contents"),
               tabPanel("Help", "contents"))


}
