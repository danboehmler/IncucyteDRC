#' calculateCutTime
#'
#' Uses the control growth curves in an IncucyteDRCSet to calclate the appropriate cut time for a specific
#' number of doublings
#'
#' @param idrc_set IncucyteDRCSet object
#' @param baseline_time The timepoint which forms the baseline for calculting number of doublings
#' @param no_doublings The number of doublings required
#' @param max_val The maximum allowable growth curve value
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
#'
calculateCutTime <- function(idrc_set, baseline_time=24, no_doublings=4, max_val=80) {

    if(is.null(idrc_set$fitted_models)) {
        stop('Need to fit splines first using fitIndividualSplines')
    }

    #baseline_time=24; no_doublings=4; max_val=80;

    #get the spline models for the control data
    control_drc_data <- idrc_set$fitted_models %>%
        dplyr::inner_join(idrc_set$platemap, by='wellid') %>%
        dplyr::select(wellid, gc_model, sampleid, conc, samptype, concunits) %>%
        dplyr::filter(samptype=='C')


    worker <- function(gcm, wellid) {

        fitted_df <- data.frame(elapsed=seq(0, max(gcm$x), 1))
        fitted_df$value = predict(gcm, fitted_df, se=TRUE)$fit
        fitted_df$diff2 <- c(NA, NA,  diff(fitted_df$value, 1, 2) )  #generate second differences
        fitted_df$ma2 <- stats::filter(fitted_df$diff2,rep(1/30,30),sides=2) #moving average
        if (which.min(fitted_df$ma2) > which.max(fitted_df$ma2)) {  #if the minimum occurs after the maximum, ie typical situation where rate of growth fastest before tails off
            time_neg_inflex <- fitted_df [ which.min(fitted_df$ma2) , 'elapsed' ]  #get the timepoint where slowing of growth is happening fastest
            max_control <- min (predict(gcm , time_neg_inflex) ,  max(fitted_df$value) ) #dont go higher than absolute max control or value at time_neg_inflex
        } else {
            max_control <- max(fitted_df$value) #if the inflexion min occurs before the max this is a slow grower so don't apply inflection point criterion (just return max pc)
        }

        fitted_df <- fitted_df [ 1:which.max(fitted_df$value) , ] #get rid of datapoints after the max value
        val_at_baseline <- predict(gcm , baseline_time) #predict the value at baseline
        val_post_ndoublings <- min ( val_at_baseline * 2^no_doublings , max_val, max_control ) #work out the value post doublings, make sure it's not more than max spec val or max control val
        time_post_ndoublings <- fitted_df [ which.min(abs(fitted_df$value - val_post_ndoublings)) , 'elapsed' ]  #which timepoint closest to desired value
        cut_time <- round(time_post_ndoublings,0)
        actual_doublings <- log2 ( val_post_ndoublings / val_at_baseline )


        return(data.frame(wellid=wellid,
                          baseline_time=baseline_time,
                          no_doublings=no_doublings,
                          max_val=max_val,
                          val_at_baseline=val_at_baseline,
                          val_post_ndoublings=val_post_ndoublings,
                          time_post_ndoublings=time_post_ndoublings,
                          cut_time=cut_time,
                          actual_doublings=actual_doublings,
                          stringsAsFactors=FALSE
                          ))
    }

    control_drc_data %>% do(worker(.$gc_model, .$wellid)) %>% ungroup()

}
