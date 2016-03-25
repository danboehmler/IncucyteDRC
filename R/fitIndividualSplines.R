#' fitIndividualSplines
#'
#' Function to fit splines to the growth curve data in an IncucyteDRCSet object
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
#'
fitIndividualSplines <- function(idrc_set) {

    #combine the platemap and data
    data <- idrc_set$platemap %>% dplyr::inner_join(idrc_set$platedata$data, by='wellid')

    #fit the splines
    fitted_models <- data %>%
        dplyr::group_by(wellid) %>%
        dplyr::do(gc_model=loess (value ~ elapsed  , ., span=0.3, control=loess.control(surface='direct')))

    #establish the data range
    data_range <- seq(from=min(data$elapsed, na.rm=TRUE), to=max(data$elapsed, na.rm = TRUE), by = 1)

    #generate the fitted data for plotting
    fitted_data <- fitted_models %>%
        dplyr::do(data.frame(value=predict(.$gc_model, data_range), elapsed=data_range, wellid=.$wellid, stringsAsFactors=FALSE)) %>%
        dplyr::ungroup() %>%
        dplyr::inner_join(idrc_set$platemap, by='wellid') %>%
        dplyr::select(wellid, sampleid, conc, samptype, concunits, value, elapsed) %>%
        as.data.frame()

    #construct the output object
    output <- idrc_set
    output$fitted_data_indiv <- fitted_data
    output$fitted_models_indiv <- fitted_models

    return(output)


}
