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
#' test_splines <- fitIndividualSplines(test_list[[2]])
#' plotIncucyteDRCSet(test_splines)
#' test_drc <- calculateDRCdata(test_splines, cut_time=100)
#' plotIncucyteDRCSet(test_drc)
#' exportDRCtoDataFrame(test_drc)
#' test_drc <- fitDoseResponseCurve(test_drc)
#'
fitDoseResponseCurve <- function(idrc_set, include_control=FALSE) {

    df <- exportDRCtoDataFrame(idrc_set, include_control)

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

    #extract parameters from models filtering out NULLs
    drc_ec50 <- drc_models %>%
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
    drc_ec50_nulls <- drc_models %>%
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
    output$drc_models <- drc_models
    output$drc_ec50 <- drc_ec50

    return(output)

}
