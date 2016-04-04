#' print.IncucyteDRCPlateData
#'
#' Prints information on an IncucyteDRCPlateData object
#'
#' @param x IncucyteDRCPlateData object
#'
#' @export
#'
#' @examples
#' test_data <- importIncucyteData(system.file(file='extdata/example_data.txt', package='IncucyteDRC'), metric='pc')
#' print(test_data)

print.IncucyteDRCPlateData <- function(x) {

    cat("## This is an IncucyteDRCPlateData S3 object with the following elements\n")
    cat(names(x))

    cat('\n\n## Data (data)\n')
    print(head(x$data))
    cat(sprintf("....\n%s rows\n", nrow(x$data)))
    cat('\n## Metric (metric)\n')
    cat(x$metric)

    cat('\n\n## Plate ID (plateid)\n')
    cat(x$plateid)
}
