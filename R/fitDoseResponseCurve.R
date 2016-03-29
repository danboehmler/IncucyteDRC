#' fitDoseResponseCurve
#'
#' Fits the dose response curve using drc for an IncucyteDRCSet object
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
#' test_splines <- fitGrowthCurvesIndividual(test_list[[2]])
#' plotIncucyteDRCSet(test_splines)
#' test_drc <- calculateDRCData(test_splines, cut_time=100)
#' plotIncucyteDRCSet(test_drc)
#' exportDRCDataToDataFrame(test_drc)
#' test_drc <- fitDoseResponseCurve(test_drc)
#' test_drc <- calculateEC50(test_drc)
#' exportEC50Data(test_drc)
#'
fitDoseResponseCurve <- function(idrc_set, include_control=FALSE) {

    df <- exportDRCDataToDataFrame(idrc_set, include_control)

    #set up subroutine for fit
    drc_fit <- function(x) {
        m1 <- NULL
        if (length(unique(x$conc)) > 2) {
            try (m1 <- drc::drm(value~conc, fct=drc::LL.4(), data=x))
        }
        return(m1)
    }

    #fit models - derive a data frame
    drc_models <- df %>%
        dplyr::group_by(sampleid) %>%
        dplyr::do(drc_model=drc_fit(.))

    #sort out output
    output <- idrc_set
    output$drc_models <- drc_models

    return(output)

}
