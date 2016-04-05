#' plotIncucyteDRCSet
#'
#' Plot the growth curve data from an IncucyteDRCSet object
#'
#' @param idrc_set IncucyteDRCSet object
#'
#' @return ggplot object
#' @export
#' @import ggplot2
#'
#' @examples
#' test_pm <- importPlatemapXML(system.file(file='extdata/example.PlateMap', package='IncucyteDRC'))
#' test_data <- importIncucyteData(system.file(file='extdata/example_data.txt', package='IncucyteDRC'), metric='pc')
#'
#' test_list <- splitIncucyteDRCPlateData(test_pm, test_data, group_columns='growthcondition')
#'
#' str(test_list)
#'
#' test_splines <- fitGrowthCurvesIndividual(test_list[[2]])
#' test_splines <- fitGrowthCurvesGrouped(test_splines)
#' plotIncucyteDRCSet(test_splines, grouped=FALSE)
#' plotIncucyteDRCSet(test_splines, grouped=TRUE)
#'
plotIncucyteDRCSet <- function(idrc_set, grouped=FALSE) {

    if(grouped & is.null(idrc_set$fitted_data_grouped)) {
        stop('Need to fit growth curves first using fitGrowthCurvesGrouped')
    }

    if(!grouped & is.null(idrc_set$fitted_data_grouped)) {
        stop('Need to fit growth curves first using fitGrowthCurvesIndividual')
    }


    #combine the platemap and data
    data <- idrc_set$platemap %>% dplyr::inner_join(idrc_set$platedata$data, by='wellid')

    out_plot <- ggplot(data, aes(x=elapsed, y=value, colour=as.factor(round(conc, 3)))) +
                    geom_point(alpha=0.2) +
                    scale_colour_discrete(name='conc') +
                    facet_wrap(~sampleid) + theme_bw()

    if(grouped) {
        out_plot <- out_plot + geom_line(data=idrc_set$fitted_data_grouped, aes(group=paste0(sampleid, conc)))
    } else {
        out_plot <- out_plot + geom_line(data=idrc_set$fitted_data_indiv, aes(group=wellid))
    }

    if(is.numeric(idrc_set$cut_time)) {
        out_plot <- out_plot + geom_vline(xintercept=idrc_set$cut_time, colour='blue', linetype='dashed')
    }

    return(out_plot)

}
