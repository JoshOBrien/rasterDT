
##' A fast \code{data.table}-based drop-in replacement for
##' \code{\link[raster:substitute]{raster::subs()}}.
##'
##' @title Speedy Substitution of Values in a RasterLayer
##' @param r Categorical \code{RasterLayer} with integer values giving
##'     field class.
##' @param dict A \code{data.frame} or \code{data.table} with one
##'     column corresponding to the levels of cells in \code{r} and
##'     another column giving the value to which each level should be
##'     mapped.
##' @param by Character string giving the name of the column in
##'     \code{dict} containing the categorical values in \code{r}.
##' @param which Character string giving name of the column in
##'     \code{dict} with the numerical values to which each value in
##'     \code{by} should be mapped.
##' @param filename Character string giving (optional) file name.
##' @param ... Additional arguments as for
##'     \code{\link[raster:writeRaster]{raster::writeRaster()}}, on
##'     which this function relies.
##' @return A \code{RasterLayer} object.
##' @export
##' @author Joshua O'Brien
##' @examples
##' ## Small example
##'
##' r <- raster(ncol = 10, nrow = 10)
##' vals <- rep(1:5, each = 20)
##' r <- setValues(r, vals)
##' set.seed(4)
##' dict <- data.table(ID = 1:5, VAL = rnorm(5))
##' out <- subsDT(r, dict)
##' plot(out)
subsDT <- function(r, dict,
                   by = "ID", which = "VAL",
                   filename = "", ...)  {
    out <- raster(r)
    dict <- data.table(dict, key = by)
    if (canProcessInMemory(r, 3)) {
        ## Execute the substitution
        DT <- data.table(ID = getValues(r))
        names(DT) <- by
        vals <- dict[DT, , on = by, nomatch = NULL][[which]]
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
            DT <- data.table(ID = getValues(r, row = tr$row[i],
                                            nrows = tr$nrows[i]))
            names(DT) <- by
            vals <- dict[DT, , on = by, nomatch = NULL][[which]]
            ## Write a block to disk rather than all at once
            out <- writeValues(out, vals, tr$row[i])
        }
        out@data@isfactor <- FALSE
        out@data@attributes <- list()
        out <- writeStop(out)
        return(out)
    }
}

