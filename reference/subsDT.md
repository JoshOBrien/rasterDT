# Speedy Raster Value Substitution

A fast `data.table`-based alternative to
[`raster::subs()`](https://rdrr.io/pkg/raster/man/subs.html).

## Usage

``` r
subsDT(x, dict, by = 1, which = 2, subsWithNA = TRUE, filename = "", ...)
```

## Arguments

- x:

  Categorical `RasterLayer` with integer values giving field class.

- dict:

  A `data.frame` or `data.table` with one (or possibly more) columns
  corresponding to the values of cells in `x` and one (or possibly more)
  columns giving the value to which each value in `x` should be mapped.

- by:

  Vector of one or possibly more integers or character strings giving
  the indices or names of the column in `dict` containing the
  categorical values in `x`.

- which:

  Vector of one or possibly more integers or character strings giving
  the indices or names of the column in `dict` with the numerical values
  to which each value in `by` should be mapped.

- subsWithNA:

  Logical. If `TRUE` values that are not matched become NA. If `FALSE`,
  they retain their original value (which could also be `NA`). This
  latter option is handy when you want to replace only one or a few
  values. It cannot be used when `x` has multiple layers

- filename:

  Character string giving (optional) file name to which the resultant
  raster should be written.

- ...:

  Additional arguments as for
  [`raster::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html),
  on which this function relies.

## Value

A `RasterLayer` object.

## Author

Joshua O'Brien

## Examples

``` r
r <- raster(ncol = 10, nrow = 10)
r[] <- round(runif(ncell(r)) * 10)
df <- data.frame(id = 2:8, v = c(10, 10, 11, 11, 12:14))
x <- subsDT(r, df)
x2 <- subsDT(r, df, subsWithNA = FALSE)

df$v2 <- df$v * 10
x3 <- subsDT(r, df, which = 2:3)

s <- stack(r, r*3)
names(s) <- c("first", "second")
x4 <- subsDT(s, df)
x5 <- subsDT(s, df, which = 2:3)
```
