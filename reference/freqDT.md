# Speedy Raster Value Frequency Tabulation

A fast `data.table`-based alternative to
[`raster::freq()`](https://rspatial.github.io/terra/reference/freq.html).

## Usage

``` r
freqDT(x, ...)

# S4 method for class 'RasterLayer'
freqDT(x, digits = 0, value = NULL, useNA = c("ifany", "no", "always"), ...)

# S4 method for class 'RasterStackBrick'
freqDT(
  x,
  digits = 0,
  value = NULL,
  useNA = c("ifany", "no", "always"),
  merge = FALSE,
  ...
)
```

## Arguments

- x:

  A `RasterLayer`, `RasterStack`, or `RasterBrick` object field class.

- ...:

  Additional arguments as for
  [`raster::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html),
  on which this function relies.

- digits:

  Integer for rounding the cell values. Argument is passed to
  [`round`](https://rdrr.io/r/base/Round.html)

- value:

  A single numeric, logical, or NA value. If supplied, `freqDT()` will
  only count the number of cells with that value.

- useNA:

  Character (one of "no", "ifany", or "always"). What to do with NA
  values? See [`table`](https://rdrr.io/r/base/table.html) for details.

- merge:

  Logical. If `TRUE` the list will be merged into a single `data.table`.

## Author

Joshua O'Brien

## Examples

``` r
r <- raster(nrow = 18, ncol = 36)
r[] <- runif(ncell(r))
r[1:5] <- NA
r <- r * r * r * 5
freqDT(r)
#> Key: <ID>
#>       ID  freq
#>    <num> <int>
#> 1:    NA     5
#> 2:     0   277
#> 3:     1   137
#> 4:     2    79
#> 5:     3    70
#> 6:     4    50
#> 7:     5    30

freqDT(r, value = 2)
#> [1] 79

s <- stack(r, r*2, r*3)
freqDT(s, merge = TRUE)
#> Key: <ID>
#>        ID layer.1 layer.2 layer.3
#>     <num>   <int>   <int>   <int>
#>  1:    NA       5       5       5
#>  2:     0     277     217     192
#>  3:     1     137      98      85
#>  4:     2      79      65      56
#>  5:     3      70      50      41
#>  6:     4      50      47      40
#>  7:     5      30      29      28
#>  8:     6      NA      37      26
#>  9:     7      NA      33      25
#> 10:     8      NA      27      17
#> 11:     9      NA      27      31
#> 12:    10      NA      13      22
#> 13:    11      NA      NA      16
#> 14:    12      NA      NA      17
#> 15:    13      NA      NA      17
#> 16:    14      NA      NA      22
#> 17:    15      NA      NA       8
```
