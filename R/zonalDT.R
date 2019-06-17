
##' A fast \code{data.table}-based alternative to
##' \code{\link[raster:zonal]{raster::zonal()}}.
##'
##' @title Speedy Zonal Statistics
##' @param x A \code{Raster*} to the totality of whose values
##'     \code{fun} should be applied within each zone.
##' @param z A categorical \code{RasterLayer} with codes representing
##'     zones.
##' @param fun A name or character string giving the function to be
##'     applied to summarize the values by zone. It needs to return a
##'     single (or at least a length-one vector). If \code{x} might
##'     contain any \code{NA} values, it should be equipped to handle
##'     them. For large rasters, this function needs to be one, like
##'     \code{sum()} whose value is the same even if carried out in a
##'     two-stage application (i.e. first to data subsets and then to
##'     the results of those subset applications).
##' @param na.rm Logical. If \code{TRUE}, \code{NA} values in \code{x}
##'     are ignored.
##' @return A \code{data.table} with a summary value for each zone.
##' @export
##' @author Joshua O'Brien
##' @examples
##' r <- raster(ncols = 10, nrows = 10)
##' r[] <- runif(ncell(r)) * 1:ncell(r)
##' z <- r
##' z[] <- rep(1:5, each = 20)
##' ## for big files, use a character value rather than a function
##' zonalDT(r, z, "sum")
##'
##' ## for smaller files you can also provide a function
##' zonalDT(r, z, mean)
##' zonalDT(r, z, min)
##'
##' ## multiple layers
##' zonalDT(stack(r, r*10), z, "sum")
zonalDT <- function(x, z, fun = sum, na.rm = TRUE) {
    compareRaster(x, z)
    fun <- match.fun(fun)
    nx <- nlayers(x)
    if (canProcessInMemory(x)) {
        ll <- vector(mode = "list", length = nx)
        names(ll) <- names(x)
        DT <- data.table(x = getValues(x),
                         z = getValues(z))
        for (j in 1:nx) {
            X <- DT[, fun(.SD[[j]], na.rm = na.rm), by = z]
            names(X) <- c("z", names(x)[j])
            setkey(X, "z")
            ll[[j]] <- X
        }
        X <- Reduce(merge, ll)
        if (na.rm) {
            X <- X[complete.cases(X), ]
        }
        return(X)
    } else {
        tr <- blockSize(x)
        res <- vector(mode = "list", length = tr$n)
        for (i in 1:tr$n) {
            ll <- vector(mode = "list", length = nx)
            names(ll) <- names(x)
            DT <- data.table(x = getValues(x, row = tr$row[i],
                                             nrows = tr$nrows[i]),
                             z = getValues(z, row = tr$row[i],
                                              nrows = tr$nrows[i]))
            for (j in 1:nx) {
                X <- DT[, fun(.SD[[j]], na.rm = na.rm), by = z]
                names(X) <- c("z", names(x)[j])
                setkey(X, "z")
                ll[[j]] <- X
            }
            res[[i]] <- Reduce(merge, ll)
        }
        X <- rbindlist(res)
        X <- X[, lapply(.SD, fun, na.rm = na.rm), by = "z",
               .SDcols = names(X)[-1]]
        setkey(X, "z")
        if (na.rm) {
            X <- X[complete.cases(X), ]
        }
        X
    }
}
