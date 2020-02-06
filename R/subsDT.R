
##' A fast \code{data.table}-based alternative to
##' \code{\link[raster:subs]{raster::subs()}}.
##'
##' @title Speedy Raster Value Substitution
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
##' r <- raster(ncol = 10, nrow = 10)
##' r[] <- round(runif(ncell(r)) * 10)
##' df <- data.frame(id = 2:8, v = c(10, 10, 11, 11, 12:14))
##' x <- subsDT(r, df)
##' x2 <- subsDT(r, df, subsWithNA = FALSE)
##'
##' df$v2 <- df$v * 10
##' x3 <- subsDT(r, df, which = 2:3)
##'
##' s <- stack(r, r*3)
##' names(s) <- c("first", "second")
##' x4 <- subsDT(s, df)
##' x5 <- subsDT(s, df, which = 2:3)
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
                ## Handle values mapped to NA or NaN by dict
                NA_keys <- dict[[by[i]]][is.na(dict[[which[i]]])]
                if (length(NA_keys)) {
                    nn[DT[[i]] %in% NA_keys] <- FALSE
                }
                ## Restore unmatched cells to their original values
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
                    ## Handle values mapped to NA or NaN by dict
                    NA_keys <- dict[[by[j]]][is.na(dict[[which[j]]])]
                    if (length(NA_keys)) {
                        nn[DT[[j]] %in% NA_keys] <- FALSE
                    }
                    ## Restore unmatched cells to their original values
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

