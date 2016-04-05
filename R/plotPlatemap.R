#' plotPlatemap
#'
#' Visualisation of an Incucyte platemap
#'
#' @param df Data frame containing platemap data
#'
#' @return ggplot object
#' @export
#' @import ggplot2
#'
#' @examples
#' test_pm <- importPlatemapXML(system.file(file='extdata/example.PlateMap', package='IncucyteDRC'))
#' plotPlatemap(test_pm)
#'
#' library(magrittr)
#' importPlatemapXML(system.file(file='extdata/example2.PlateMap', package='IncucyteDRC')) %>%
#'   plotPlatemap()
plotPlatemap <- function(pm) {

    if(is.null(attr(pm, 'IncucyteDRCPlatemap'))) {
        warning('Recommended that platemap data frames are parsed through importPlatemap function to check formatting')
    }

    p <- ggplot(pm, aes(y=row, x=col)) +
        geom_tile(aes(fill=samptype), colour='black') +
        geom_text(aes(label=sprintf("%s\n%s%s", sampleid, round(conc,4), concunits)), size=rel(2.5)) +
        scale_y_reverse() +
        xlab('') + ylab('') +
        cowplot::theme_cowplot()

    print(p)

}
