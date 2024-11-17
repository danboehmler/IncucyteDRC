#' fitGrowthCurvesGrouped
#'
#' Function to fit loess function to the growth curve data in an IncucyteDRCSet object
#'
#' @param idrc_set IncucyteDRCSet object
#' @param span Set the span to be used in the curve fit. Default 0.3.
#'
#' @return IncucyteDRCSet object
#' @importFrom stats loess loess.control predict coef
#' @export
fitGrowthCurvesGrouped <- function(idrc_set, span=0.3) {
    # Verify we have data
    if (is.null(idrc_set$platedata) || is.null(idrc_set$platedata$data)) {
        stop("No plate data found")
    }
    
    # Combine platemap and data
    data <- dplyr::inner_join(idrc_set$platemap, idrc_set$platedata$data, by='wellid')
    
    if (nrow(data) == 0) {
        stop("No data after joining platemap and plate data")
    }
    
    # Get unique plate map groups
    platemap_grouped <- idrc_set$platemap %>%
        dplyr::select(sampleid, conc, samptype, concunits) %>%
        dplyr::distinct()
    
    # Fit the curves
    fitted_models <- data %>%
        dplyr::group_by(sampleid, conc) %>%
        dplyr::do(gc_model = tryCatch({
            loess(value ~ elapsed, data=., span=span, control=loess.control(surface='direct'))
        }, error = function(e) NULL))
    
    # Generate sequence for predictions
    elapsed_range <- range(data$elapsed, na.rm=TRUE)
    if (!all(is.finite(elapsed_range))) {
        stop("Invalid elapsed time range")
    }
    data_range <- seq(from=elapsed_range[1], to=elapsed_range[2], by=1)
    
    # Generate fitted data
    fitted_data <- fitted_models %>%
        dplyr::filter(!is.null(gc_model)) %>%
        dplyr::do(data.frame(
            value = predict(.$gc_model[[1]], data_range),
            elapsed = data_range,
            sampleid = .$sampleid,
            conc = .$conc,
            stringsAsFactors = FALSE
        )) %>%
        dplyr::ungroup() %>%
        dplyr::inner_join(platemap_grouped, by=c('sampleid', 'conc')) %>%
        as.data.frame()
    
    # Update output
    output <- idrc_set
    output$fitted_data_grouped <- fitted_data
    output$fitted_models_grouped <- fitted_models
    
    return(output)
}
