# Convert a Categorical Raster to a Value Raster

Use a categorical raster's RAT to convert it to a continuous raster

## Usage

``` r
cat_to_val(r, which = 2)
```

## Arguments

- r:

  A categorical raster with a RAT (returned by `levels(r)[[1]]`), whose
  first column contain an entry for every factor level present in the
  raster. At least one of the subsequent columns should contain numeric
  values to which each level should be converted.

- which:

  An integer or character string giving the index or name of the column
  in `r`'s RAT with the numerical values to which each value in `r`
  should be mapped. Default value is `2`.

## Value

A continuous raster with each category level in `r` replaced by its
corresponding value.

## Author

Joshua O'Brien

## Examples

``` r
r_cat <- raster(matrix(c(2, 2, 2, 1), ncol = 2))
levels(r_cat) <- data.frame(ID = c(1, 2),
                            VAL1 = c(0.1, 200),
                            VAL2 = c(33, 44))

## Second column of RAT is used by default
r_con1 <- cat_to_val(r_cat)
as.matrix(r_con1)
#>      [,1]  [,2]
#> [1,]  200 200.0
#> [2,]  200   0.1

## Use 'which=' argument for conversion to another RAT column
r_con2 <- cat_to_val(r_cat, which = "VAL2")
as.matrix(r_con2)
#>      [,1] [,2]
#> [1,]   44   44
#> [2,]   44   33
```
