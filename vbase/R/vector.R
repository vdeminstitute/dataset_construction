#' @export
zipper <- function(...) {
    ll <- list(...)
    stopifnot(lapply(ll, length) %>% unlist %>% unique %>% {length(.) == 1})
    lapply(1:length(ll[[1]]), function(i) {
        lapply(ll, function(v) {
            v[i]
        }) %>% Reduce(c, .)
    }) %>% Reduce(c, .)
}

#' @export
zipper_names <- function(v, suffix = c("", "_old")) {
    clean_v <- v %>% gsub(suffix[1], "", .) %>% gsub(suffix[2], "", .)
    nonidents <- v[clean_v %^% suffix[1] %in% v & clean_v %^% suffix[2] %in% v]
    idents <- v[!v %in% nonidents]
    first_vars <- nonidents[1:(length(nonidents) / 2)]
    second_vars <- v[!v %in% c(idents, first_vars)]
    return(c(idents, zipper(first_vars, second_vars)))
}

#' @export
lagVector <- function(x, lags = 1) {
    stopifnot(is.vector(x))
    if (length(x) < lags) {
        stop(sprintf("As length of x [%d] < lags [%d], nothing can be returned",
            length(x), lags))
    }

    out <- x[ c(
        rep.int(x = as.integer(NA), times = lags),
        seq_along(x)[-seq.int(from = 1, to = lags, by = 1)]-lags) ]
    
    stopifnot(length(x) == length(out))
    stopifnot(class(x) == class(out))
    return(out)
}

#' @export
create_index <- function(v, na_index = TRUE) {
    out <- v != lagVector(v)
    out[1] <- TRUE
    
    if (na_index) out[is.na(out)] <- TRUE

    out <- as.integer(cumsum(out))

    return(out)
}

#' @export
ifelse_ <- function(bool, v1, v2) {
	na_error <- TRUE
	if (na_error) {
		stopifnot(`There are NAs in the first argument!` = 
			!anyNA(bool))
	}

	stopifnot(`v1 and v2 have different class!` = class(v1)[1] == class(v2)[1])
	stopifnot(`v1 and v2 have different length!` = 
		length(v1) == length(v2) | length(v1) == 1 | length(v2) == 1)
	
	le <- max(length(v1), length(v2))

	outv <- vector(mode = typeof(v1), le)
	class(outv) <- class(v1)

	# Recycle v1 and v2
	v1 <- rep_len(v1, le)
	v2 <- rep_len(v2, le)

	outv[bool] <- v1[bool]
	outv[!bool] <- v2[!bool]
	return(outv)
}
