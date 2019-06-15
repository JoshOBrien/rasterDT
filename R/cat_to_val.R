
##' Use its RAT to Convert a Categorical Raster to a Continuous Raster
##' @title Convert a Categorical Raster to a Value Raster
##' @param r A categorical raster whose RAT (returned by
##'     \code{levels(r)[[1]]} has columns named \code{"ID"} and
##'     \code{"VAL"}. \code{"ID"} should contain an entry for every
##'     factor level present in the raster, while \code{"VAL"} gives
##'     the values to which each level should be converted.
##' @return A continuous raster with each category level in \code{r}
##'     replaced by its corresponding value.
##' @export
##' @author Joshua O'Brien
##' @examples
##' r_cat <- raster(matrix(c(2,2,2,1), ncol=2))
##' levels(r_cat) <- data.frame(ID = c(1, 2), VAL = c(0.1, 200))
##' r_con <- cat_to_val(r_cat)
##' as.matrix(r_con)
cat_to_val <- function(r) {
    dict <- levels(r)[[1]]
    subsDT(r, dict)
}
