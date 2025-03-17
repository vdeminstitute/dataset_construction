#' @export
add_class <- function(obj, class) {
	class(obj) <- c(class, class(obj))
	return(obj)
}



