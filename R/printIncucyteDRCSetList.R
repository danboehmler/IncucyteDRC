#' print.IncucyteDRCSetList
#'
#' Prints information on an IncucyteDRCSetList object
#'
#' @param x IncucyteDRCSet object
#'
#' @export
#'
#' @examples
#' test_pm <- importPlatemapXML(system.file(file='extdata/example.PlateMap', package='IncucyteDRC'))
#' test_data <- importIncucyteData(system.file(file='extdata/example_data.txt', package='IncucyteDRC'), metric='pc')
#' test_list <- splitIncucyteDRCPlateData(test_pm, test_data, group_columns='growthcondition')
#' class(test_list)
#' print(test_list)

print.IncucyteDRCSetList <- function(x) {

    cat(sprintf("## This is an IncucyteDRCSetList S3 object containing %s IncucyteDRCSet objects\n", length(x)))
    cat("## Access using standard list notation ie my_list[[1]]\n")
    cat('## Try lapply(my_list, print) to see more information on each object')

}
