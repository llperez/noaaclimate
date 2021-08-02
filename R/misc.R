.get_raw_psl <- function(url) {
    x0 <- utils::read.csv(url, header=F)
    x_split <- lapply(strsplit(x0[,1], "\\s+"), function(z) z[z != ""])
    x_split <- x_split[lapply(x_split, length) > 0]
    x_years <- x_split[[1]]
    x_start <- max(which(lapply(x_split, function(w) w[[1]]) == x_years[1]))
    x_end <- max(which(lapply(x_split, function(w) w[[1]]) == x_years[2]))
    x <-  data.frame(apply(do.call(rbind, x_split[x_start:x_end]), 2, as.numeric))
    na_val <-  as.numeric(x_split[[x_end + 1]])  
    colnames(x) <- c("Year", 1:12)
    x <- stats::reshape(x, direction="long", varying=colnames(x)[2:13], v.names="Value", idvar=c("Year"), timevar="Month", times=colnames(x)[2:13])
    x$Month <- as.numeric(x$Month)
    x <- x[order(x$Year, x$Month),]
    rownames(x) <- NULL
    x$Value[x$Value == na_val] <- NA
    x
}

.raw_psl_ts <- function(x, cname=NULL) {
    stats::na.omit(stats::ts(x$Value, frequency=12, start=x$Year[1], names=cname))
}

.raw_psl_xts <- function(x, cname=NULL) {
    x <- stats::na.omit(xts::xts(x$Value, zoo::as.yearmon(paste(x$Year, x$Month, "01", sep="-"))))
    if (!is.null(cname)) {
        colnames(x) <- cname
    }
    x
}

.raw_psl_df <- function(x, cname=NULL) {
    if (!is.null(cname)) {
        colnames(x) <- c("Year", "Month", cname)
    }
    rownames(x) <- as.character(zoo::as.yearmon(paste(x$Year, x$Month, "01", sep="-")))

    stats::na.omit(x)
}

.psl_monthly_func <- list(
    "data.frame" = .raw_psl_df,
    "ts" = .raw_psl_ts,
    "xts" = .raw_psl_xts
)

.psl_monthly_post_func <- list(
    "data.frame" = function(x, ...) subset(x, select=which(!duplicated(names(x)))),
    "ts" = function(x, cnames, ...) { colnames(x) <- cnames; x },
    "xts" = function(x, ...) x
)

.psl_monthly_merge_func <- list(
    "data.frame" = function(...) merge(..., all=TRUE),
    "ts" = function(...) cbind(...),
    "xts" = function(...) merge(..., all=TRUE)
)


.psl_names <- c(
    "pna",
    "epo",
    "wp",
    "ea",
    "nao",
    "soi",
    "censo",
    "tna",
    "tsa",
    "whwp",
    "oni",
    "meiv2",
    "nina1.anom",
    "nina1",
    "nina3.anom",
    "nina3",
    "nina4.anom",
    "nina4",
    "nina34.anom",
    "nina34",
    "pdo",
    "np",
    "tni",
    "hurr",
    "ao",
    "aao",
    "pacwarm",
    "amon.us",
    "amon.us.long",
    "NTA_ersst",
    "CAR_ersst",
    "qbo",
    "espi"
)
