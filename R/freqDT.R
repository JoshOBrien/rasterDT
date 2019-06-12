
setGeneric("freqDT", function(x, ...)
    standardGeneric("freqDT"))



##' A fast \code{data.table}-based drop-in replacement for
##' \code{\link[raster:freq]{raster::freq()}}.
##'
##' @name freqDT
##' @rdname freqDT
##' @title Speedy Frequency Tabulation of Values in a RasterLayer
##' @param x A \code{RasterLayer}, \code{RasterStack}, or
##'     \code{RasterBrick} object field class.
##' @param digits
##' @param value
##' @param useNA
##' @param progress
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
##' freqDT(r, value=2)
##'
##' s <- stack(r, r*2, r*3)
##' freqDT(s, merge = TRUE)
setMethod("freqDT", signature(x = "RasterLayer"),
## freqDT <- function(r, digits = 0,
    function(x, digits = 0,
             value = NULL,
             useNA = c("ifany", "no", "always"),
             progress = "", ...)  {
        useNA <- match.arg(useNA)

        ## Count number of cells of a particular value
        if (!is.null(value)) {
            if (canProcessInMemory(x, 3)) {
                DT <- data.table(ID = getValues(x))
                DT[, ID := round(ID, digits = digits)]
                out <-
                    if(is.na(value)) {
                        DT[is.na(ID), .N]
                    } else {
                        DT[ID == value, .N]
                    }
            } else {
                tr <- blockSize(x)
                res <- vector(mode = "integer", length = tr$n)
                for (i in seq_len(tr$n)) {
                    DT <- data.table(ID = getValues(x, row = tr$row[i],
                                                    nrows = tr$nrows[i]))
                    DT[, ID := round(ID, digits = digits)]
                    res[i] <-
                        if(is.na(value)) {
                            DT[is.na(ID),.N]
                        } else {
                            DT[ID == value, .N]
                        }
                }
                out <- sum(res)
            }
            return(out)
        }

        ## Tabulate frequencies of all cell values
        if (canProcessInMemory(x, 3)) {
            DT <- data.table(ID = getValues(x))
            DT[, ID := round(ID, digits = digits)]
            out <- DT[, list(freq = .N), by = "ID"]
        } else {
            tr <- blockSize(x)
            res <- vector(mode = "list", length = tr$n)
            for (i in seq_len(tr$n)) {
                DT <- data.table(ID = getValues(x, row = tr$row[i],
                                                nrows = tr$nrows[i]))
                DT[, ID := round(ID, digits = digits)]
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
             progress = "", ...) {
        nl <- nlayers(x)
        res <- list()
        for (i in 1:nl) {
            res[[i]] <- freqDT(raster(x, i), digits = digits,
                               useNA = useNA, progress = "", ...)

        }
        names(res) <- names(x)
        if(merge) {
            r <- Reduce(function(x, y) merge(x, y, all = TRUE), res)
            names(r)[-1] <- names(x)
            return(r)
        } else {
            return(res)
        }
    }
    )
