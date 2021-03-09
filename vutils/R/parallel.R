#' Check parallelized results for errors
#'
#' Given a list of outputs from \code{\link[parallel]{mclapply}} or
#' \code{\link[parallel]{mcMap}}, asserts whether any errors were
#' encountered.
#'
#' @param ll List of results
#'
#' @details Currently, no attempt is made to ascertain the location
#'     within the results list of any errors. Feel free to properly
#'     extend this function.
#'
#' @examples
#' \dontrun{
#' ll <- parallel::mclapply(1:3, sdf)
#' mc_assert(ll)
#' }
#'
#' @export
mc_assert <- function(ll) {
    if (!is.list(ll))
        ll <- as.list(ll)

    err <- Filter(is.try_error, ll) %>% unique

    if (length(err) > 0) {
        vapply(err, get_errmsg, character(1)) %>%
            paste(collapse = "\n") %>%
            stop(call. = F)
    }
}



#' Check all forked processes.
#'
#' Waits for all forked processes to finish and checks their return values
#' @export
all_forks_done <- function(pattern = "fork", forklist) {
    if (!missing(forklist)) {
        ll <- parallel::mccollect(mget(forklist, envir = .GlobalEnv))
    } else {
        ll <- prallel::mccollect(mget(ls(pattern = "fork\\d+",
                                         envir = .GlobalEnv),
                                      envir = .GlobalEnv))
    }

    all(unlist(lapply(ll, is.null)))
}
