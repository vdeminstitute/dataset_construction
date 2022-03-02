#' Process posteriors for exporting
#'
#' @param path File path to MM/BFA/Binary/HLI file with posteriors
#' @details Normally the posteriors are located in \code{super} (BFA, MM) or \code{posterior} (HLI) directories.
#' @export
get_posterior <- function(path) {
    vname <- basename(path) %>% gsub("_\\d+.rds", "", x = .)

    fl <- vbase::read_file(path)[[1]]

    posterior <- if (grepl("bfa|hli", path)) {
            fl[["thin_post"]]
        } else if (grepl("mm|binary", path)) {
            fl[["post.sample"]][["z"]]
        } else {stop("Doesn't match BFA/MM/HLI/Binary file path!")}

    posterior <- posterior[!grepl("[A|B]_", rownames(posterior)),]

    df <- cbind(
        country_text_id = gsub(pattern = "([A-Z]+) (\\d+-\\d+-\\d+)", replacement = "\\1", x = rownames(posterior)),
        historical_date = gsub(pattern = "([A-Z]+) (\\d+-\\d+-\\d+)", replacement = "\\2", x = rownames(posterior)),
        var_name = vname,
        data.frame(posterior), stringsAsFactors = FALSE)

    colnames(df) <- gsub("^X", "V", colnames(df))
    bool <- c(FALSE, FALSE, FALSE, rep(TRUE, ncol(posterior)))
    
    if (!grepl("mm|binary", path)) {
        df[, bool] <- lapply(df[, bool], qnorm)
    }

    df[, bool] <- round(df[, bool], 5)

    stopifnot(all(nchar(df[["country_text_id"]]) == 3))
    stopifnot(all(!is.na(df[, c("country_text_id", "historical_date")])))
    
    return(df)
}
