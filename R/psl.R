
#' Returns a vector with available index names to retrieve using \code{"psl_monthly"}
#'
#' @return A character vector
#'
#' @examples
#' psl_index_names()
#' 
#' @export
psl_index_names <- function() .psl_names

#' Fetches monthly NOAA-PSL series.
#'
#' @param index_names A character vector of index names, see \code{"psl_index_names"},
#' @param output_type The output type of the resulting series. Can be \code{"data.frame"}, \code{"ts"} or \code{"xts"}.
#'
#' @return Time series of index observations.
#'
#' @examples
#' psl_monthly("nina1.anom")
#' 
#' psl_monthly(c("soi", "nao"), output_type="xts")
#' 
#' @export
psl_monthly <- function(index_names, output_type=c("data.frame", "ts", "xts")) {
    output_type <- match.arg(output_type)
    stopifnot(all(index_names %in% .psl_names))

    mfunc <- .psl_monthly_func[[output_type]]
    x <- lapply(index_names, function(w) mfunc(.get_raw_psl(paste("https://psl.noaa.gov/data/correlation/", w, ".data", sep="")), w))
    x <- Reduce(.psl_monthly_merge_func[[output_type]], x)

    .psl_monthly_post_func[[output_type]](x, index_names)
}
