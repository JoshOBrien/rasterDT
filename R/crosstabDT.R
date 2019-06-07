
##' A fast data.table-based drop-in replacement for
##' \code{\link[raster:crosstab]{raster::crosstab()}}.
##'
##' @title Speedy Cross-tabulation of Two Categorical Rasters
##' @param ID A \code{RasterLayer} whose values give the index of the
##'     \code{POLYGON} or \code{MULTIPOLYGON} fragment from which each
##'     cell was rasterized.
##' @param VAL A categorical \code{RasterLayer} whose values indicate
##'     the habitat classification of the area represented by each
##'     cell.
##' @return A \code{data.table} recording the frequency of each
##'     habitat type within each indexed area. The first column,
##'     \code{"ID"}, gives an index corresponding to the \code{"ID"}
##'     column of the \code{sp} layer object from which the \code{ID}
##'     raster was rasterized.
##' @export
##' @author Joshua O'Brien
##' @examples
##' r <- raster(nc = 5, nr = 5)
##' r[] <- sample(5, 25, replace = TRUE)
##' s <- setValues(r, sample(2, 25, replace = TRUE))
##' crosstabDT(r,s)
crosstabDT <- function(ID, VAL) {
    if(canProcessInMemory(ID)) {
        DT <- data.table(ID = getValues(ID),
                         VAL = getValues(VAL))
        X <- DT[,.N, by = c("ID", "VAL")]
        X <- X[complete.cases(X),]
        setkeyv(X, c("ID", "VAL"))
        dcast(X, ID ~ VAL, value.var = "N", fill = 0)
    } else {
        tr <- blockSize(ID)
        res <- vector(mode = "list", length = tr$n)
        for (i in 1:tr$n) {
            DT <- data.table(ID = getValues(ID, row = tr$row[i],
                                            nrows = tr$nrows[i]),
                             VAL = getValues(VAL, row = tr$row[i],
                                             nrows = tr$nrows[i]))
            X <- DT[, .N, by = c("ID", "VAL")]
            X <- X[complete.cases(X), ]
            setkeyv(X, c("ID", "VAL"))
            res[[i]] <- X
        }
        X <- rbindlist(res)
        X <- X[, .(N = sum(N)), by = c("ID", "VAL")]
        dcast(X, ID ~ VAL, value.var = "N", fill = 0)
    }
}
