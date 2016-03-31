#' Shiny Visualisation App
#'
#' Provides an interactive web app for the running the IncucyteDRC workflow
#'
#' @param con A \code{SQLiteConnection} object to the database
#' @return Launches an interactive Shiny application
#' @export
#' @import shiny
shinyVisApp <- function() {

    shiny::shinyApp(
        ui = shinyVisUI(),
        server = function(input, output) {
            shinyVisServer(input, output)
        }
    )
}
