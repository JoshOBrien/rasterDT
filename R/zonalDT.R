
##' A fast data.table-based partial replacement for
##' \code{\link[raster:zonal]{raster::zonal()}}.
##'
##' @title Speedy Zonal Statistics
##' @param VAL A \code{RasterLayer} to the totality of whose values
##'     \code{fun} should be applied within each zone.
##' @param ZONE A categorical \code{RasterLayer} with codes
##'     representing zones.
##' @param fun A name or character string giving the function to be
##'     applied to summarize the values by zone. It needs to return a
##'     single (or at least a length-one vector). If \code{VAL} might
##'     contain any \code{NA} values, it should be equipped to handle
##'     them. For large rasters, this function needs to be one, like
##'     \code{sum()} whose value is the same even if carried out in a
##'     two-stage application (i.e. first to data subsets and then to
##'     the results of those subset applications).
##' @return A \code{data.table} with a summary value for each zone.
##' @export
##' @author Joshua O'Brien
##' @examples
##' r <- raster(ncols = 10, nrows = 10)
##' r[] <- runif(ncell(r)) * 1:ncell(r)
##' z <- r
##' z[] <- rep(1:5, each=20)
##' ## for big files, use a character value rather than a function
##' zonalDT(r, z, 'sum')
##'
##' ## for smaller files you can also provide a function
##' zonalDT(r, z, mean)
##' zonalDT(r, z, min)
zonalDT <- function(VAL, ZONE, fun = sum, na.rm = TRUE) {
    fun <- match.fun(fun)
    nx <- nlayers(VAL)
    if (canProcessInMemory(VAL)) {
    ## if (FALSE) {
        ll <- vector(mode = "list", length = nx)
        names(ll) <- names(VAL)
        DT <- data.table(VAL = getValues(VAL),
                         ZONE = getValues(ZONE))
        for (j in 1:nx) {
            X <- DT[, fun(.SD[[j]], na.rm = na.rm), by = ZONE]
            names(X) <- c("ZONE", names(VAL)[j])
            setkey(X, "ZONE")
            ll[[j]] <- X
        }
        X <- Reduce(merge, ll)
        return(X)
    } else {
        tr <- blockSize(VAL)
        res <- vector(mode = "list", length = tr$n)
        for (i in 1:tr$n) {
            ll <- vector(mode = "list", length = nx)
            names(ll) <- names(VAL)
            DT <- data.table(VAL = getValues(VAL, row = tr$row[i],
                                             nrows = tr$nrows[i]),
                             ZONE = getValues(ZONE, row = tr$row[i],
                                              nrows = tr$nrows[i]))
            for (j in 1:nx) {
                X <- DT[, fun(.SD[[j]], na.rm = na.rm), by = ZONE]
                names(X) <- c("ZONE", names(VAL)[j])
                setkey(X, "ZONE")
                ll[[j]] <- X
            }
            res[[i]] <- Reduce(merge, ll)
        }
        X <- rbindlist(res)
        X <- X[, lapply(.SD, fun, na.rm = na.rm), by="ZONE",
               .SDcols = names(X)[-1]]
        setkey(X, "ZONE")
        X
    }
}
