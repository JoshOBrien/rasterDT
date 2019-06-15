
##' A fast \code{data.table}-based alternative to
##' \code{\link[raster:substitute]{raster::subs()}}.
##'
##' @title Speedy Substitution of Values in a Raster
##' @param x Categorical \code{RasterLayer} with integer values giving
##'     field class.
##' @param dict A \code{data.frame} or \code{data.table} with one (or
##'     possibly more) columns corresponding to the values of cells in
##'     \code{x} and one (or possibly more) columns giving the value
##'     to which each value in \code{x} should be mapped.
##' @param by Vector of one or possibly more integers or character
##'     strings giving the indices or names of the column in
##'     \code{dict} containing the categorical values in \code{x}.
##' @param which Vector of one or possibly more integers or character
##'     strings giving the indices or names of the column in
##'     \code{dict} with the numerical values to which each value in
##'     \code{by} should be mapped.
##' @param subsWithNA Logical. If \code{TRUE} values that are not
##'     matched become NA.  If \code{FALSE}, they retain their
##'     original value (which could also be \code{NA}). This latter
##'     option is handy when you want to replace only one or a few
##'     values. It cannot be used when \code{x} has multiple layers
##' @param filename Character string giving (optional) file name to
##'     which the resultant raster should be written.
##' @param ... Additional arguments as for
##'     \code{\link[raster:writeRaster]{raster::writeRaster()}}, on
##'     which this function relies.
##' @return A \code{RasterLayer} object.
##' @export
##' @author Joshua O'Brien
##' @examples
##' \dontrun{
##' r <- raster(ncol = 10, nrow = 10)
##' vals <- rep(1:5, each = 20)
##' r <- setValues(r, vals)
##' set.seed(4)
##' dict <- data.table(ID = 1:5, VAL = rnorm(5))
##' out <- subsDT(r, dict)
##' plot(out)
##' }
subsDT <- function(x, dict,
                   by = 1, which = 2,
                   subsWithNA = TRUE,
                   filename = "", ...)  {
    nx <- nlayers(x)
    if(is.numeric(by)) by <- names(dict)[by]
    if(is.numeric(which)) which <- names(dict)[which]
    by <- rep(by, length.out = nx)
    which <- rep(which, length.out = nx)
    dict <- data.table(dict, key = by)
    if (nx == 1) {
        out <- raster(x)
    } else {
        out <- brick(x, nl = nx)
    }
    if (canProcessInMemory(x, 3)) {
        ## Execute the substitution
        ll <- vector(mode = "list", length = nx)
        names(ll) <- names(x)
        DT <- data.table(ID = getValues(x))
        names(DT) <- by
        for (i in 1:nx) {
            vals <- dict[DT[, ..i], , on = by[i]][[which[i]]]
            if (isFALSE(subsWithNA)) {
                nn <- is.na(vals)
                vals[nn] <- DT[[i]][nn]
            }
            ll[[i]] <- vals
        }
        vals <- as.matrix(as.data.table(ll))
        out <- setValues(out, vals)
        out@data@isfactor <- FALSE
        out@data@attributes <- list()
        if (filename != '') {
            out <- writeRaster(out, filename = filename, ...)
        }
        return(out)
    } else {
        if (filename == '') {
            filename <- rasterTmpFile()
        }
        out <- writeStart(out, filename = filename, ...)
        tr <- blockSize(out)
        for (i in seq_len(tr$n)) {
            ## Execute the substitution
            ll <- vector(mode = "list", length = nx)
            names(ll) <- names(x)
            DT <- data.table(ID = getValues(x, row = tr$row[i],
                                            nrows = tr$nrows[i]))
            names(DT) <- by
            for (j in 1:nx) {
                vals <- dict[DT[, ..j], , on = by[j]][[which[j]]]
                if (isFALSE(subsWithNA)) {
                    nn <- is.na(vals)
                    vals[nn] <- DT[[j]][nn]
                }
                ll[[j]] <- vals
            }
            vals <- as.matrix(as.data.table(ll))
            out <- writeValues(out, vals, tr$row[i])
        }
        out@data@isfactor <- FALSE
        out@data@attributes <- list()
        out <- writeStop(out)
        return(out)
    }
}

