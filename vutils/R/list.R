#' @export 
named_list <- function(...) {
    return(setNames(list(...), unlist(as.list(match.call())[-1L])))
}


#' Replicate item as list for Map
#'
#' @export
list_replicate <- function(obj, n) {
    ll <- vector("list", n)
    for (i in 1:n) {    
        ll[[i]] <- obj 
    }
    return(ll)
}

#' Safe loading of list and data.frame elements
#'
#' @export
safe <- function(obj) {
    if(is.null(obj)) {
        stop("Object returns NULL!")
    }
    return(obj)
}

#' @export
list_loop <- function(ll, element_function) {
    out <- element_function(ll[1])
    if (length(ll) == 1)
        return(out)
    return(c(out, list_loop(ll[2:length(ll)], element_function)))
}

remove_null_element_function <- function(element) {
    if (is.null(element[[1]]))
        return(NULL)
    return(element)
}

#' @export
list_remove_null <- function(ll) {
    list_loop(ll, remove_null_element_function)
}

remove_try_error_element_function <- function(element) {
    if (class(element[[1]])[1] == "try-error")
        return(NULL)
    return(element)
}

#' @export
list_remove_try_error <- function(ll) {
    list_loop(ll, remove_try_error_element_function)
}