#' plotDoseResponseCurve
#'
#' Plots the dose response curve for a given sample id from an IncucyteDRCSet object
#'
#' @param idrc_set IncucyteDRCSet object
#' @param sampleid The sample id to plot
#'
#' @return a plot
#' @export
#'
#' @examples
#' test_pm <- importPlatemapXML(system.file(file='extdata/example.PlateMap', package='IncucyteDRC'))
#' test_data <- importIncucyteData(system.file(file='extdata/example_data.txt', package='IncucyteDRC'), metric='pc')
#'
#' test_list <- splitIncucyteDRCPlateData(test_pm, test_data, group_columns='growthcondition')
#'
#' str(test_list)
#'
#' test_drc <- fitGrowthCurvesIndividual(test_list[[2]])
#' test_drc <- fitGrowthCurvesGrouped(test_drc)
#' plotIncucyteDRCSet(test_drc)
#' test_drc <- calculateCutTimeForIDRCSet(test_drc)
#' test_drc <- calculateDRCData(test_drc)
#' test_drc <- fitDoseResponseCurve(test_drc)
#' test_drc <- calculateEC50(test_drc)
#' plotDoseResponseCurve(test_drc, 'PDD00017273')
plotDoseResponseCurve <- function(idrc_set, sampleid) {

    drc_models_filtered <- idrc_set$drc_models %>%
        dplyr::filter(sampleid == sampleid)

    mod <- drc_models_filtered$drc_model[[1]]

    drc:::plot.drc(mod, type='all', broken=TRUE, main=sampleid)
    abline(v=drc::ED(mod,50, display=F)[1], col='red', lty='dashed')


}
