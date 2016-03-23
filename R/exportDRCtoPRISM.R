#' exportDRCtoPRISM
#'
#' Exports data in PRISM format for an IncucyteDRCSet
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
#' exportDRCtoPRISM(test_drc)
#'
exportDRCtoPRISM <- function(idrc_set, include_control=FALSE) {

    idrc_set$drc_data %>% dplyr::filter(samptype=='S') %>%
        dplyr::select(sampleid, conc, cut_val) %>%
        dplyr::group_by(sampleid, conc) %>%
        dplyr::mutate(idx=row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(sampleid, col_id=paste(round(conc,4), idx, sep='_'), cut_val) %>%
        tidyr::spread(col_id, cut_val) %>%
        as.data.frame()

}
