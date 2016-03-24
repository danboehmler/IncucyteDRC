#' exportEC50Data
#'
#' Exports EC50 data into a standard data frame format from the dose response fit for an IncucyteDRCSet object
#'
#' @param idrc_set IncucyteDRCSet object
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
#' test_drc <- fitIndividualSplines(test_list[[2]])
#' test_drc <- fitGroupSplines(test_drc)
#' plotIncucyteDRCSet(test_drc)
#' test_drc <- calculateCutTime(test_drc)
#' test_drc <- calculateDRCdata(test_drc)
#' plotIncucyteDRCSet(test_drc)
#' exportDRCtoDataFrame(test_drc)
#' test_drc <- fitDoseResponseCurve(test_drc)
#' exportEC50Data(test_drc)
#'
exportEC50Data <- function(idrc_set) {

    return(idrc_set$drc_ec50)

}
