
##' A fast data.table-based drop-in replacement for
##' \code{\link[raster:crosstab]{raster::crosstab()}}.
##'
##' @title Speedy Cross-tabulation of Two Categorical Rasters
##' @param x A \code{RasterLayer} whose values give the index of the
##'     \code{POLYGON} or \code{MULTIPOLYGON} fragment from which each
##'     cell was rasterized.
##' @param y A categorical \code{RasterLayer} whose values indicate
##'     the habitat classification of the area represented by each
##'     cell.
##' @return A \code{data.table} recording the frequency of each
##'     habitat type within each indexed area. The first column,
##'     \code{"x"}, gives an index corresponding to the \code{"x"}
##'     column of the \code{sp} layer object from which the \code{x}
##'     raster was rasterized.
##' @export
##' @author Joshua O'Brien
##' @examples
##' r <- raster(nc = 5, nr = 5)
##' r[] <- sample(5, 25, replace = TRUE)
##' s <- setValues(r, sample(2, 25, replace = TRUE))
##' crosstabDT(r,s)
crosstabDT <- function(x, y, digits = 0,
                       long = FALSE, useNA = FALSE) {
    if (canProcessInMemory(x)) {
        if (nlayers(x) == 1) {
            DT <- data.table(x = getValues(x),
                             y = getValues(y))
        } else {
            DT <- data.table(getValues(x))
        }
        DT <- round(DT, digits)
        if (!useNA) {
            DT <- DT[complete.cases(DT),]
        }
        X <- DT[, .(Freq = .N), by = names(DT)]
        setkeyv(X, names(X))
        if (!long) {
            X <- fxtabs(X)
        }
        return(X)
    } else {
        tr <- blockSize(x)
        res <- vector(mode = "list", length = tr$n)
        for (i in 1:tr$n) {
            ##
            if (nlayers(x) == 1) {
                data.table(x = getValues(x, row = tr$row[i],
                                         nrows = tr$nrows[i]),
                           y = getValues(y, row = tr$row[i],
                                         nrows = tr$nrows[i]))
            } else {
                DT <- data.table(getValues(x, row = tr$row[i],
                                            nrows = tr$nrows[i]))
            }
            ##
            DT <- round(DT, digits)
            if (!useNA) {
                DT <- DT[complete.cases(DT),]
            }
            X <- DT[, .(Freq = .N), by = names(DT)]
            res[[i]] <- X
        }
        X <- rbindlist(res)
        nms <- setdiff(names(x), "Freq")
        setkeyv(X, nms)
        X <- X[, .(Freq = sum(Freq)), by = nms]
        if (!long) {
            X <- fxtabs(X)
        }
        return(X)

    }
}


fxtabs <- function(X) {
    X <- copy(X)
    vals <- X[["Freq"]]
    ## Prepare empty array
    X[, Freq := NULL]
    dd <- lapply(X, function(x) sort(unique(x), na.last = TRUE))
    res <- array(dim = lengths(dd), data = 0L, dimnames = dd)
    ## Prepare index for assignment of non-zero frequencies
    ii <- X[, lapply(.SD, function(v) as.numeric(addNA(v)))]
    ii <- as.matrix(ii)
    ## Assign non-values into the array
    res[ii] <- vals
    res
}
