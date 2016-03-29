#' exportDRCDataToPRISM
#'
#' Exports data in PRISM format for an IncucyteDRCSet
#'
#' @param idrc_set IncucyteDRCSet object
#' @param include_control Whether to include control sample as zero conc control
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
#' test_splines <- fitGrowthCurvesIndividual(test_list[[2]])
#' plotIncucyteDRCSet(test_splines)
#' test_drc <- calculateDRCData(test_splines, cut_time=100)
#' plotIncucyteDRCSet(test_drc)
#' exportDRCDataToPRISM(test_drc)
#'
exportDRCDataToPRISM <- function(idrc_set, include_control=FALSE, add_metadata=FALSE) {

    out_df <- exportDRCDataToDataFrame(idrc_set, include_control, add_metadata=FALSE) %>%
                    dplyr::transmute(sampleid, col_id=paste(round(conc,4), idx, sep='_'), value) %>%
                    tidyr::spread(col_id, value) %>%
                    as.data.frame()

    if(add_metadata & is.data.frame(idrc_set$metadata)) {
        out_df <- merge(out_df, idrc_set$metadata)
    }

    return(as.data.frame(out_df))

}
