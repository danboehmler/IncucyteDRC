#' exportEC50Data
#'
#' Exports EC50 data into a standard data frame format from the dose response fit for an IncucyteDRCSet object
#'
#' @param idrc_set IncucyteDRCSet object
#' @param add_metadata Whether or not to merge IncucyteDRCSet metadata into the output
#'
#' @return IncucyteDRCSet object
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
#' plotIncucyteDRCSet(test_drc)
#' exportDRCDataToDataFrame(test_drc)
#' test_drc <- fitDoseResponseCurve(test_drc)
#' test_drc <- calculateEC50(test_drc)
#' exportEC50Data(test_drc)
#'
exportEC50Data <- function(idrc_set, add_metadata=FALSE) {

    #make sure that fitDoseResponseCurve has been run
    if(is.null(idrc_set$drc_ec50)) {
        stop("Need to run calculateEC50 function first to calculate EC50 values for the object")
    }

    out_df <- idrc_set$drc_ec50

    if(add_metadata & is.data.frame(idrc_set$metadata)) {
        out_df <- merge(out_df, idrc_set$metadata)
    }

    return(as.data.frame(out_df))

}
