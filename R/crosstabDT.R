
##' A fast \code{data.table}-based alternative to
##' \code{\link[raster:crosstab]{raster::crosstab()}}.
##'
##' @title Speedy Raster Cross-tabulation
##' @param x A \code{Raster*} object
##' @param y If \code{x} has just one layer, a \code{RasterLayer}
##'     object. Otherwise, if \code{x} is a multi-layered
##'     \code{RasterStack} or \code{RasterBrick}, this argument (if
##'     any) is unused.
##' @param digits Integer. The number of digits for rounding the
##'     values before cross-tabulation. Default is \code{0}.
##' @param long Logical. If \code{TRUE}, the results are returned in a
##'     'long' format \code{data.table} instead of as a table. Default
##'     is \code{FALSE}.
##' @param useNA Logical. Should the returned table or
##'     \code{data.table} include counts of \code{NA} values? Default
##'     is \code{FALSE}.
##' @return Either a table or a \code{data.table} recording the
##'     frequency of each combination of Raster values.
##' @import methods
##' @import raster
##' @rawNamespace import(data.table, except = shift)
##' @importFrom stats complete.cases
##' @export
##' @author Joshua O'Brien
##' @examples
##' \dontrun{
##' r <- raster(nc = 5, nr = 5)
##' r[] <- sample(5, 25, replace = TRUE)
##' s <- setValues(r, sample(2, 25, replace = TRUE))
##' crosstabDT(r,s)
##' }
crosstabDT <- function(x, y, digits = 0,
                       long = FALSE, useNA = FALSE) {
    compareRaster(x, y)
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


## A fast replacement for stats::xtabs(Freq ~ ., X).
##
## Takes a data.table with at least three columns, one of which must
## be named "Freq", and returns an object just like xtabs() would,
## except without an attached formula attribute.
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
