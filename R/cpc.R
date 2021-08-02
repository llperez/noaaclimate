
#' Fetches weekly Nino SST series from NOAA-CPC.
#'
#' @param output_type The output type of the resulting series. Can be \code{"data.frame"}, \code{"ts"} or \code{"xts"}.
#'
#' @return Time series of index observations.
#'
#' @examples
#' cpc_weekly_sst()
#' 
#' cpc_weekly_sst("xts")
#' 
#' @export
cpc_weekly_sst <- function(output_type=c("data.frame", "ts", "xts")) {
    output_type <- match.arg(output_type)
    x0 <- utils::read.csv('https://www.cpc.ncep.noaa.gov/data/indices/wksst9120.for', header=F) 
    prs <- do.call(rbind, strsplit(x0[4:NROW(x0),], "\\s\\s+"))
    weeks <- as.Date(prs[,1], "%d%b%Y")
    vals <- t(apply(prs[,2:NCOL(prs)], 1, function(w) { 
            wq <- strsplit(w, "(?=(\\s|-))", perl=TRUE)
            as.numeric(do.call(cbind, lapply(wq, function(zq) cbind(zq[[1]], paste(zq[[2]], zq[[3]], sep="")))))
    }))

    out <- data.frame(weeks, vals)
    colnames(out) <- c("Week", "nino12_sst", "nino12_sst_anom", "nino3_sst", "nino3_sst_anom", "nino34_sst", "nino34_sst_anom", "nino4_sst", "nino4_sst_anom")

    if (output_type == "ts") {
        return(stats::ts(out[,2:NCOL(out)]))
    } else if (output_type == "xts") {
        return(xts::xts(out[,2:NCOL(out)], as.Date(out$Week)))
    }
    out
}
