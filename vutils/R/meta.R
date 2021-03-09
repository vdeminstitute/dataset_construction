#' @export
meta <- function(obj, attribute_value = NULL) {
    v <- attr(obj, "meta_data")
    if (is.null(v)) {
        info("No meta data stored in this object.")
        return(NULL)
    }
    if (is.null(attribute_value))
        return(v)
    v[names(v) == attribute_value]
}