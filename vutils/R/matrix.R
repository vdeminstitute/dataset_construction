#' @export
load_matrix <- function(m, drop.vignettes = TRUE) {
    stopifnot(is.matrix(m))
    if (isTRUE(drop.vignettes))
        m <- m[!is_vignette(rownames(m)),, drop = F]
    m
}