# Speedy Raster Cross-tabulation

A fast `data.table`-based alternative to
[`raster::crosstab()`](https://rspatial.github.io/terra/reference/crosstab.html).

## Usage

``` r
crosstabDT(x, y, digits = 0, long = FALSE, useNA = FALSE)
```

## Arguments

- x:

  A `Raster*` object

- y:

  If `x` has just one layer, a `RasterLayer` object. Otherwise, if `x`
  is a multi-layered `RasterStack` or `RasterBrick`, this argument (if
  any) is unused.

- digits:

  Integer. The number of digits for rounding the values before
  cross-tabulation. Default is `0`.

- long:

  Logical. If `TRUE`, the results are returned in a 'long' format
  `data.table` instead of as a table. Default is `FALSE`.

- useNA:

  Logical. Should the returned table or `data.table` include counts of
  `NA` values? Default is `FALSE`.

## Value

Either a table or a `data.table` recording the frequency of each
combination of raster values.

## Author

Joshua O'Brien

## Examples

``` r
r <- raster(nc = 5, nr = 5)
r[] <- runif(ncell(r)) * 2
s <- setValues(r, runif(ncell(r)) * 3)
crosstabDT(r, s)
#>    y
#> x   0 1 2 3
#>   0 0 0 6 2
#>   1 1 5 5 1
#>   2 2 3 0 0

rs <- r/s
r[1:5] <- NA
s[20:25] <- NA
x <- stack(r, s, rs)
crosstabDT(x, useNA = TRUE, long = TRUE)
#> Key: <layer.1, layer.2, layer.3, Freq>
#>     layer.1 layer.2 layer.3  Freq
#>       <num>   <num>   <num> <int>
#>  1:      NA       0      11     1
#>  2:      NA       2       0     3
#>  3:      NA       2       1     1
#>  4:       0      NA       0     1
#>  5:       0       2       0     3
#>  6:       0       3       0     1
#>  7:       1      NA       0     2
#>  8:       1      NA       1     1
#>  9:       1       0      10     1
#> 10:       1       1       1     5
#> 11:       1       2       0     1
#> 12:       1       2       1     1
#> 13:       2      NA       2     1
#> 14:       2      NA       7     1
#> 15:       2       1       1     2
```
