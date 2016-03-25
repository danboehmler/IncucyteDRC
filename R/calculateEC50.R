#' calculateEC50
#'
#' Calculates the EC50s for the dose response curves in an IncucyteDRCSet object
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
#' test_splines <- fitIndividualSplines(test_list[[2]])
#' plotIncucyteDRCSet(test_splines)
#' test_drc <- calculateDRCdata(test_splines, cut_time=100)
#' plotIncucyteDRCSet(test_drc)
#' exportDRCtoDataFrame(test_drc)
#' test_drc <- fitDoseResponseCurve(test_drc)
#' test_drc <- calculateEC50(test_drc)
#' exportEC50Data(test_drc)
#'
calculateEC50 <- function(idrc_set) {

    #extract parameters from models filtering out NULLs
    drc_ec50 <- idrc_set$drc_models %>%
        dplyr::filter(!is.null(drc_model)) %>%
        dplyr::mutate(Tm = drc::ED(drc_model,50, display=F)[1],
                      SE = drc::ED(drc_model,50, display=F)[2],
                      #R2 <- dr.calc.r2(dr),
                      b = coef(drc_model)[1],
                      c = coef(drc_model)[2],
                      d = coef(drc_model)[3],
                      e = coef(drc_model)[4]) %>%
        dplyr::select(-drc_model) %>%
        dplyr::ungroup()

    #deal with null models
    drc_ec50_nulls <- idrc_set$drc_models %>%
        dplyr::filter(is.null(drc_model)) %>%
        dplyr::mutate(Tm=NA, SE=NA, b=NA, c=NA, d=NA, e=NA) %>%
        dplyr::select(-drc_model) %>%
        dplyr::ungroup()

    #combine outputs
    drc_ec50 <- drc_ec50 %>%
        dplyr::bind_rows(drc_ec50_nulls) %>%
        as.data.frame()

    #sort out output
    output <- idrc_set
    output$drc_ec50 <- drc_ec50

    return(output)

}
