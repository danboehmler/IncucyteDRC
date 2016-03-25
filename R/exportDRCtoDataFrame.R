#' exportDRCtoDataFrame
#'
#' Exports data into a standard data frame format for an IncucyteDRCSet
#'
#' @param idrc_set IncucyteDRCSet object
#' @param include_control Whether to include control sample as zero conc control
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
#' test_splines <- fitIndividualSplines(test_list[[2]])
#' plotIncucyteDRCSet(test_splines)
#' test_drc <- calculateDRCdata(test_splines, cut_time=100)
#' plotIncucyteDRCSet(test_drc)
#' exportDRCtoDataFrame(test_drc)
#'
exportDRCtoDataFrame <- function(idrc_set, include_control=FALSE, add_metadata=FALSE) {

    out_df <- idrc_set$drc_data %>%
                    dplyr::filter(samptype=='S') %>%
                    dplyr::transmute(sampleid, conc, value=cut_val) %>%
                    dplyr::group_by(sampleid, conc) %>%
                    dplyr::mutate(idx=row_number()) %>%
                    dplyr::ungroup()  %>%
                    as.data.frame()

    if(add_metadata & is.data.frame(idrc_set$metadata)) {
        out_df <- merge(out_df, idrc_set$metadata)
    }

    return(out_df)

}
