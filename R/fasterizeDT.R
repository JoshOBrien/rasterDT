
##' A front end for
##' \code{\link[fasterize:fasterize]{fasterize::fasterize()}}, fixing
##' several of its infelicities.
##'
##' Unlike other functions in this package, \code{fasterizeDT()} does
##' not use \code{data.table} to speed up its computations. Instead,
##' it is a wrapper for \code{fasterize::fasterize()}, intended to
##' address several of that function's limitations.
##'
##' Most importantly, \code{fasterizeDT()} takes care to properly
##' handle rasterization operations in which either the template
##' \code{RasterLayer} or the selected polygon feature field is a
##' factor. Specifically, it always returns a raster whose type
##' (numeric or factor) and levels (if a factor) match that of the
##' spatial polygon attribute indicated by its \code{field} argument.
##' Second, when \code{field} specifies an attribute of class
##' \code{"character"}, \code{fasterizeDT()} automatically converts it
##' to a factor and returns a factor raster. In this, it is unlike
##' both \code{fasterize::fasterize()} and
##' \code{raster::rasterize()}. Finally, unlike
##' \code{fasterize::fasterize()}, \code{fasterizeDT()} accepts as
##' inputs either \code{sf::sf()} objects or
##' \code{sp::SpatialPolygonsDataFrame} objects.
##'
##' @title Fast polygon rasterization using numeric, factor, or
##'     character fields
##' @param x Either an \code{sf::sf()} object with a geometry column
##'     of \code{POLYGON} and/or \code{MULTIPOLYGON} objects or a
##'     \code{sp::SpatialPolygonsDataFrame} object.
##' @param raster A \code{RasterLayer} object to be used as a template
##'     for the raster output.
##' @param field Character. The name of a column in \code{x},
##'     providing a value for each of the polygons rasterized. If
##'     \code{NULL} (the default), all polygons will be given a value
##'     of \code{1}.
##' @param fun Character. The name of a function by which to combine
##'     overlapping polygons. Currently takes \code{"sum"},
##'     \code{"first"}, \code{"last"}, \code{"min"}, \code{"max"},
##'     \code{"count"}, or \code{"any"}. For more details, see
##'     \code{\link[fasterize:fasterize]{?fasterize::fasterize}}.
##' @param background Value to put in the cells that are not covered
##'     by any of the features of \code{x}. Default is \code{NA}.
##' @param by Character string giving the name of a column in \code{x}
##'     by which to aggregate layers. If set, \code{fasterizeDT} will
##'     return a \code{RasterBrick} with as many layers as unique
##'     values of the \code{by} column.
##' @return A raster of the same size, extent, resolution and
##'     projection as the supplied raster template. Unlike
##'     \code{\link[fasterize:fasterize]{fasterize::fasterize()}},
##'     \code{fasterizeDT} returns a raster of the same type as the
##'     data in the column of \code{x} selected by the \code{field}
##'     argument.
##' @importFrom fasterize fasterize
##' @importFrom sf st_as_sf
##' @export
##' @author Joshua O'Brien
##' @examples
##' ## Load example polygons and prepare a template raster
##' SPDF <- rgdal::readOGR(system.file("external", package = "raster"), "lux")
##' llratio <- 1/cos(pi * mean(coordinates(SPDF)[, 2])/180)
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
    if (inherits(x, "SpatialPolygonsDataFrame")) {
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

