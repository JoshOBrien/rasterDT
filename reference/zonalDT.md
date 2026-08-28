# Speedy Zonal Statistics

A fast `data.table`-based alternative to
[`raster::zonal()`](https://rspatial.github.io/terra/reference/zonal.html).

## Usage

``` r
zonalDT(x, z, fun = sum, na.rm = TRUE)
```

## Arguments

- x:

  A `Raster*` to the totality of whose values `fun` should be applied
  within each zone.

- z:

  A categorical `RasterLayer` with codes representing zones.

- fun:

  A name or character string giving the function to be applied to
  summarize the values by zone. It needs to return a single (or at least
  a length-one vector). If `x` might contain any `NA` values, it should
  be equipped to handle them. For large rasters, this function needs to
  be one, like [`sum()`](https://rdrr.io/r/base/sum.html) whose value is
  the same even if carried out in a two-stage application (i.e. first to
  data subsets and then to the results of those subset applications).

- na.rm:

  Logical. If `TRUE`, `NA` values in `x` are ignored.

## Value

A `data.table` with a summary value for each zone.

## Author

Joshua O'Brien

## Examples

``` r
r <- raster(ncols = 10, nrows = 10)
r[] <- runif(ncell(r)) * 1:ncell(r)
z <- r
z[] <- rep(1:5, each = 20)
## for big files, use a character value rather than a function
zonalDT(r, z, "sum")
#> Key: <z>
#>        z     layer
#>    <int>     <num>
#> 1:     1  92.06847
#> 2:     2 297.08973
#> 3:     3 504.58335
#> 4:     4 757.46285
#> 5:     5 760.91244

## for smaller files you can also provide a function
zonalDT(r, z, mean)
#> Key: <z>
#>        z     layer
#>    <int>     <num>
#> 1:     1  4.603423
#> 2:     2 14.854486
#> 3:     3 25.229167
#> 4:     4 37.873142
#> 5:     5 38.045622
zonalDT(r, z, min)
#> Key: <z>
#>        z      layer
#>    <int>      <num>
#> 1:     1 0.19173515
#> 2:     2 0.05449026
#> 3:     3 0.08397092
#> 4:     4 6.01396433
#> 5:     5 3.12063895

## multiple layers
zonalDT(stack(r, r*10), z, "sum")
#> Key: <z>
#>        z   layer.1   layer.2
#>    <int>     <num>     <num>
#> 1:     1  92.06847  920.6847
#> 2:     2 297.08973 2970.8973
#> 3:     3 504.58335 5045.8335
#> 4:     4 757.46285 7574.6285
#> 5:     5 760.91244 7609.1244
```
