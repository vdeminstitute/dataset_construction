#' @export 
named_list <- function(...) {
    return(setNames(list(...), unlist(as.list(match.call())[-1L])))
}