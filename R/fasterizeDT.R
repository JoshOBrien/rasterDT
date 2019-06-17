
##' A front end for
##' \code{\link[fasterize:fasterize]{fasterize::fasterize()}}, fixing
##' several of its infelicities.
##'
##' @title Fixed up fasterize Function
##' @param x An \code{sf::sf()} object with a geometry column of
##'     \code{POLYGON} and/or \code{MULTIPOLYGON} objects or a
##'     \code{sp::SpatialPolygonsDataFrame} object.
##' @param raster A raster object. Used as a template for the raster
##'     output
##' @param field character. The name of a column in \code{x},
##'     providing a value for each of the polygons rasterized. If NULL
##'     (default), all polygons will be given a value of 1.
##' @param fun \code{?fasterize::fasterize}
##' @param background Value to put in the cells that are not covered
##'     by any of the features of x. Default is NA.
##' @param by \code{?fasterize::fasterize}
##' @return A raster of the same size, extent, resolution and
##'     projection as the supplied raster template. Unlike
##'     \code{\link[fasterize:fasterize]{fasterize::fasterize()}},
##'     \code{fasterizeDT} returns a raster of the same type as the
##'     data in the column of \code{x} column selected by the
##'     \code{field} argument.
##' @importFrom fasterize fasterize
##' @importFrom sf st_as_sf
##' @export
##' @author Joshua O'Brien
##' @examples
##' SPDF <- shapefile(system.file("external/lux.shp", package = "raster"))
##' llratio <- 1/cos(pi * mean(coordinates(SPDF)[,2])/180)
##' rr <- raster(extent(SPDF),
##'              resolution = c(llratio * 0.01, 0.01),
##'              crs = proj4string(SPDF))
##'
##' ## An integer-valued field produces a numeric raster
##' rInt <- fasterizeDT(SPDF, rr, field = "ID_2")
##' plot(rInt, col = colorRampPalette(blues9)(12))
##'
##' ## A character-valued field returns a factor raster
##' rFac <- fasterizeDT(SPDF, rr, field = "NAME_2")
##' if (require(rasterVis)) {
##'     levelplot(rFac)
##' }
fasterizeDT <- function (x,
                         raster,
                         field = NULL,
                         fun = "last",
                         background = NA_real_,
                         by = NULL) {
    if (class(x) == "SpatialPolygonsDataFrame") {
        x <- st_as_sf(x)
    }
    if (is.null(field)) {
        field <- names(x)[1]
    }
    field_class <- class(x[[field]])
    ## Convert character field to factor
    if (field_class == "character") {
        val <- x[[field]]
        val <- factor(val, levels = sort(unique(val)))
        x[[field]] <- val
    }
    ## Rasterize polygons
    out_raster <-
        fasterize(sf = x, raster = raster, field = field,
                  fun = fun, background = background, by = by)
    ## Ensure that any RAT attached to output raster is derived from
    ## input char or factor field
    if(field_class %in% c("character", "factor")) {
        lev <- levels(x[[field]])
        RAT <- data.frame(ID = seq_along(lev), VALUE = lev)
        ## Silence warning emitted when overwriting a RAT from input
        ## raster
        suppressWarnings(levels(out_raster) <- RAT)
    } else {
        out_raster@data@isfactor <- FALSE
        out_raster@data@attributes <- list()
    }
    out_raster
}

