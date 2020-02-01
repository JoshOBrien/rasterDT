
##' A fast \code{data.table}-based alternative to
##' \code{\link[raster:freq]{raster::freq()}}.
##'
##' @name freqDT
##' @rdname freqDT
##' @title Speedy Frequency Tabulation of Values in a RasterLayer
##' @param x A \code{RasterLayer}, \code{RasterStack}, or
##'     \code{RasterBrick} object field class.
##' @param digits Integer for rounding the cell values. Argument is
##'     passed to \code{\link[base]{round}}
##' @param value A single numeric, logical, or NA value. If supplied,
##'     \code{freqDT()} will only count the number of cells with that
##'     value.
##' @param useNA Character (one of "no", "ifany", or "always"). What
##'     to do with NA values? See \code{\link[base]{table}} for details.
##' @param merge Logical. If \code{TRUE} the list will be merged into
##'     a single \code{data.table}.
##' @param ... Additional arguments as for
##'     \code{\link[raster:writeRaster]{raster::writeRaster()}}, on
##'     which this function relies.
##' @export
##' @author Joshua O'Brien
##' @examples
##' r <- raster(nrow = 18, ncol = 36)
##' r[] <- runif(ncell(r))
##' r[1:5] <- NA
##' r <- r * r * r * 5
##' freqDT(r)
##'
##' freqDT(r, value = 2)
##'
##' s <- stack(r, r*2, r*3)
##' freqDT(s, merge = TRUE)
setGeneric("freqDT", function(x, ...)
    standardGeneric("freqDT"))



##' @rdname freqDT
##' @export
setMethod("freqDT", signature(x = "RasterLayer"),
    function(x, digits = 0,
             value = NULL,
             useNA = c("ifany", "no", "always"),
             ...)  {
        useNA <- match.arg(useNA)

        ## Count number of cells of a particular value
        if (!is.null(value)) {
            if (canProcessInMemory(x, 3)) {
                DT <- data.table(ID = getValues(x))
                if (is.numeric(DT[["ID"]])) {
                    DT[, ID := round(ID, digits = digits)]
                }
                out <-
                    if (is.na(value)) {
                        DT[, sum(is.na(ID))]
                    } else {
                        DT[, sum(ID == value, na.rm = TRUE)]
                    }
            } else {
                tr <- blockSize(x)
                res <- vector(mode = "integer", length = tr$n)
                for (i in seq_len(tr$n)) {
                    DT <- data.table(ID = getValues(x, row = tr$row[i],
                                                    nrows = tr$nrows[i]))
                    if (is.numeric(DT[["ID"]])) {
                        DT[, ID := round(ID, digits = digits)]
                    }
                    res[i] <-
                        if (is.na(value)) {
                            DT[, sum(is.na(ID))]
                        } else {
                            DT[, sum(ID == value, na.rm = TRUE)]
                        }
                }
                out <- sum(res)
            }
            return(out)
        }

        ## Tabulate frequencies of all cell values
        if (canProcessInMemory(x, 3)) {
            DT <- data.table(ID = getValues(x))
            if (is.numeric(DT[["ID"]])) {
                DT[, ID := round(ID, digits = digits)]
            }
            out <- DT[, list(freq = .N), by = "ID"]
        } else {
            tr <- blockSize(x)
            res <- vector(mode = "list", length = tr$n)
            for (i in seq_len(tr$n)) {
                DT <- data.table(ID = getValues(x, row = tr$row[i],
                                                nrows = tr$nrows[i]))
                if (is.numeric(DT[["ID"]])) {
                    DT[, ID := round(ID, digits = digits)]
                }
                X <- DT[, list(freq = .N), by = "ID"]
                res[[i]] <- X
            }
            X <- rbindlist(res)
            out <- X[, list(freq = sum(freq)), by = "ID"]
        }
        ## Handle useNA options ("ifany", "no", or "always")
        if (useNA == "no") {
            out <- out[!is.na(ID),]
        } else if (useNA == "always" & !any(out[, is.na(ID)])) {
            out <- rbind(out, data.table(ID = NA, freq = 0))
        }
        setkey(out, "ID")
        out
    }
    )


##' @rdname freqDT
##' @export
setMethod("freqDT", signature(x = "RasterStackBrick"),
    function(x, digits = 0,
             value = NULL,
             useNA = c("ifany", "no", "always"),
             merge = FALSE,
             ...) {
        nl <- nlayers(x)
        res <- vector(length = nl, mode = "list")
        for (i in 1:nl) {
            res[[i]] <- freqDT(raster(x, i), digits = digits,
                               value = value, useNA = useNA,
                               progress = "", ...)

        }
        names(res) <- names(x)
        if (!is.null(value)) {
            res <- unlist(res)
            return(res)
        }
        if(merge) {
            r <- Reduce(function(x, y) merge(x, y, all = TRUE), res)
            names(r)[-1] <- names(x)
            return(r)
        } else {
            return(res)
        }
    }
    )
