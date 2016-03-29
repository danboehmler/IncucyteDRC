#' plotDoseResponseCurve
#'
#' Plots the dose response curve for a given sample id from an IncucyteDRCSet object
#'
#' @param idrc_set IncucyteDRCSet object
#' @param sampleid The sample id to plot
#' @param native A boolean whether or not to use the native \code{\link[drc]{plot.drc}} function
#'
#' @return a ggplot2 object (if native is FALSE) or NULL but draws to open graphics object (if native is TRUE)
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
#' plotDoseResponseCurve(test_drc, 'PDD00017273', native=TRUE)
#' plotDoseResponseCurve(test_drc, 'PDD00017273', native=FALSE)
plotDoseResponseCurve <- function(idrc_set, sampleid, native=FALSE) {

    q_sampleid <- sampleid

    drc_models_filtered <- idrc_set$drc_models %>%
        dplyr::filter(sampleid == q_sampleid)

    drc_model <- drc_models_filtered$drc_model[[1]]
    if(is.null(drc_model)) return(NULL)

    EC50val <- drc::ED(drc_model,50, display=F)[1]

    #if native is TRUE then just use the drc native plotting function
    if(native) {
        drc:::plot.drc(drc_model, type='all', broken=TRUE, main=sampleid)
        abline(v=EC50val, col='red', lty='dashed')
        return()
    }

    #if not, continue and return a ggplot object
    #extract the generated function from the curve fit and make the curve
    drc_model_func <- drc_model$curve[[1]]
    conc_range <- unique(drc_model$dataList$dose)
    conc_range <- log10(conc_range[conc_range>0])
    conc_seq <- 10^seq(min(conc_range), max(conc_range), 0.1)
    drc_model_curve <- data.frame(conc=conc_seq,
                          value=drc_model_func(conc_seq))

    #do the plot
    p2 <- ggplot(drc_model$origData, aes(y=value, x=conc)) +
        geom_point(alpha=0.8, shape=21, size=rel(3)) +
        ggtitle(sprintf("%s", sampleid)) +
        scale_x_log10() +
        geom_line(data=drc_model_curve, colour='red') +
        geom_vline(xintercept = EC50val, color='red', alpha=0.5, linetype='dashed') +
        cowplot::theme_cowplot()

    return(p2)

}
