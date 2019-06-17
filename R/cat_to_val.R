
##' Use a Categorical Raster's RAT to convert it to a Continuous
##' Raster
##' @title Convert a Categorical Raster to a Value Raster
##' @param r A categorical raster with a RAT (returned by
##'     \code{levels(r)[[1]]}), whose first column contain an entry
##'     for every factor level present in the raster. At least one of
##'     the subsequent columns should contain numeric values to which
##'     each level should be converted.
##' @param which An integer or character string giving the index or
##'     name of the column in \code{r}'s RAT with the numerical values
##'     to which each value in \code{r} should be mapped. Default
##'     value is \code{2}.
##' @return A continuous raster with each category level in \code{r}
##'     replaced by its corresponding value.
##' @export
##' @author Joshua O'Brien
##' @examples
##' r_cat <- raster(matrix(c(2, 2, 2, 1), ncol = 2))
##' levels(r_cat) <- data.frame(ID = c(1, 2),
##'                             VAL1 = c(0.1, 200),
##'                             VAL2 = c(33, 44))
##'
##' ## Second column of RAT is used by default
##' r_con1 <- cat_to_val(r_cat)
##' as.matrix(r_con1)
##'
##' ## Use `which=` argument for conversion to another RAT column
##' r_con2 <- cat_to_val(r_cat, which = "VAL2")
##' as.matrix(r_con2)
cat_to_val <- function(r, which = 2) {
    dict <- levels(r)[[1]]
    subsDT(r, dict, which = which)
}
