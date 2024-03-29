% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fasterizeDT.R
\name{fasterizeDT}
\alias{fasterizeDT}
\title{Polygon Rasterization Using Numeric, Factor, or
    Character Fields}
\usage{
fasterizeDT(
  x,
  raster,
  field = NULL,
  fun = "last",
  background = NA_real_,
  by = NULL
)
}
\arguments{
\item{x}{Either an \code{sf::sf()} object with a geometry column
of \code{POLYGON} and/or \code{MULTIPOLYGON} objects or a
\code{sp::SpatialPolygonsDataFrame} object.}

\item{raster}{A \code{RasterLayer} object to be used as a template
for the raster output.}

\item{field}{Character. The name of a column in \code{x},
providing a value for each of the polygons rasterized. If
\code{NULL} (the default), all polygons will be given a value
of \code{1}.}

\item{fun}{Character. The name of a function by which to combine
overlapping polygons. Currently takes \code{"sum"},
\code{"first"}, \code{"last"}, \code{"min"}, \code{"max"},
\code{"count"}, or \code{"any"}. For more details, see
\code{\link[fasterize:fasterize]{?fasterize::fasterize}}.}

\item{background}{Value to put in the cells that are not covered
by any of the features of \code{x}. Default is \code{NA}.}

\item{by}{Character string giving the name of a column in \code{x}
by which to aggregate layers. If set, \code{fasterizeDT} will
return a \code{RasterBrick} with as many layers as unique
values of the \code{by} column.}
}
\value{
A raster of the same size, extent, resolution and
    projection as the supplied raster template. Unlike
    \code{\link[fasterize:fasterize]{fasterize::fasterize()}},
    \code{fasterizeDT} returns a raster of the same type as the
    data in the column of \code{x} selected by the \code{field}
    argument.
}
\description{
A front end for
\code{\link[fasterize:fasterize]{fasterize::fasterize()}}, fixing
several of its infelicities.
}
\details{
Unlike other functions in this package, \code{fasterizeDT()} does
not use \code{data.table} to speed up its computations. Instead,
it is a wrapper for \code{fasterize::fasterize()}, intended to
address several of that function's limitations.

Most importantly, \code{fasterizeDT()} takes care to properly
handle rasterization operations in which either the template
\code{RasterLayer} or the selected polygon feature field is a
factor. Specifically, it always returns a raster whose type
(numeric or factor) and levels (if a factor) match that of the
spatial polygon attribute indicated by its \code{field} argument.
Second, when \code{field} specifies an attribute of class
\code{"character"}, \code{fasterizeDT()} automatically converts it
to a factor and returns a factor raster. In this, it is unlike
both \code{fasterize::fasterize()} and
\code{raster::rasterize()}. Finally, unlike
\code{fasterize::fasterize()}, \code{fasterizeDT()} accepts as
inputs either \code{sf::sf()} objects or
\code{sp::SpatialPolygonsDataFrame} objects.
}
\examples{
\donttest{
## Load example polygons and prepare a template raster
if (require(raster)) {
SPDF <- shapefile(system.file("external/lux.shp", package = "raster"))
llratio <- 1/cos(pi * mean(coordinates(SPDF)[, 2])/180)
rr <- raster(extent(SPDF),
             resolution = c(llratio * 0.01, 0.01),
             crs = proj4string(SPDF))

## An integer-valued field produces a numeric raster
rInt <- fasterizeDT(SPDF, rr, field = "ID_2")
plot(rInt, col = colorRampPalette(blues9)(12))

## A character-valued field returns a factor raster
rFac <- fasterizeDT(SPDF, rr, field = "NAME_2")
if (require(rasterVis)) {
    levelplot(rFac)
}
}
}
}
\author{
Joshua O'Brien
}
