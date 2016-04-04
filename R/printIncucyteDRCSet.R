#' print.IncucyteDRCSet
#'
#' Prints information on an IncucyteDRCSet object
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
#' class(test_list[[2]])
#' print(test_list[[2]])

print.IncucyteDRCSet <- function(x) {

    cat("## This is an IncucyteDRCSet S3 object with the following elements\n")
    cat(names(x))

    cat('\n\n## Cut Time (cut_time)\n')
    cat(x$cut_time)
    cat('\n## Metadata (metadata)\n')
    print(x$metadata)

}
