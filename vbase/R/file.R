#' Check if writable
#'
#' Return \code{TRUE} if either a directory or file is writeable.
#' @param s CharacterVector path
#'
#' @examples
#' \dontrun{
#' file.writeable("example.txt")
#' }
#'
#' @name check_writeable
NULL

#' @describeIn check_writeable Check file
#' @export
file.writeable <- function(s) {
    path <- ifelse(file.exists(s), s, dirname(s))
    file.access(path, mode = 2) == 0
}

guard <- function(f) {
    str <- substitute(f) %>% deparse
    tryCatch(f, error = function(e) stop("Unable to find function: " %^% str, call. = F))
}

#' Read a given file into \code{R}
#'
#' Convenience wrapper that calls the appropriate function to read a
#' file into \code{R}.
#'
#' @param f File to open.
#' @param fn Optional argument to explicitly specify the function R
#'     should use to read the given file.
#' @param \\dots Addition arguments passed to the associated function found.
#'
#' @details If no function is provided, \code{read_file} selects a
#'     function from a hardcoded list depending on the suffix of the
#'     provided file. If a function is not found, an error will be
#'     raised.
#'
#' @export
read_file <- function(f, fn = NULL, msg = TRUE, ...) {
    if (missing(f))
        stop("Missing file")

    if (file.access(f, mode = 4) == -1)
        stop("Unable to open file: " %^% f)

    fn <- fn %||% switch(tools::file_ext(f),
                        "csv"  = guard(data.table::fread) %>% partial(data.table = F),
                        "xlsx" = guard(openxlsx::read.xlsx) %>% partial(detectDates = T),
                        "dta"  = guard(readstata13::read.dta13),
                        "sav"  = guard(haven::read_sav),
                        "rds"  = readRDS,
                        "dput" = dget,
                        "txt"  = readLines,
                        "xls" = guard(readxl::read_excel),
                        # thanks to this guy, he already did everything for me: https://www.r-bloggers.com/safe-loading-of-rdata-files-2/
                        "RData" = function(Rdata, env = new.env()) {load(Rdata,env); return(env)},
                        "R" = readLines,
                        stop("Unable to find function to open file: " %^% f))
    if(msg) info("Reading file " %^% f)
    fn(f, ...)

}

#' Write an object to file
#'
#' Convenience wrapper that calls the appropriate function to write an
#' object to file.
#'
#' @param o An object of any type.
#' @param f File to write to.
#' @param fn Optional argument to explicity specify the function
#'     \code{R} should use to write the given object to file.
#' @param \\dots Additional arguments passed to the found function.
#'
#' @details If no function is provided, \code{save_file} selects a
#'     function from a hardcoded list depending on the suffix of the
#'     provided file. If a function is not found, an error will be
#'     raised.
#'
#' @export
write_file <- function(o = NULL, f, fn = NULL, rotate = FALSE, msg = TRUE,
                       overwrite = TRUE, dir_create = FALSE, ...) {
    if (missing(f))
        stop("Missing file")

    if (!overwrite & file.exists(f)) {
        warn("File already exists and overwrite is set to false!")
        return(NULL)
    }

    if (dir_create) {
        dir.create(dirname(f), showWarnings = F, recursive = T)
    }

    if (file.access(dirname(f), mode = 2) == -1)
        stop("Unable to write in directory: " %^% dirname(f))

    if (rotate) {
        while (file.exists(f)) {
            n <- sub("^.*[_](.*?)[.].*?$", "\\1", f)
            if (suppressWarnings(is.na(as.numeric(n)))) {
                f <- sub("^(.*)([.].*?)$", "\\1" %^% "_0" %^% "\\2", f)
            } else
                f <- sub("^(.*[_])(.*?)([.].*?)$", "\\1" %^% (as.numeric(n) + 1) %^% "\\3", f)
        }
    }

    # Never save a tibble. I hate tibbles.
    if ("tbl_df" %in% class(o))
        o <- as.data.frame(o)

    if (missing(o)) {
      fn <- fn %||% switch(tools::file_ext(f),
                           "RData" = save.image)
      fn(f, ...)
    } else {
      fn <- fn %||% switch(tools::file_ext(f),
                           "csv"  = partial(utils::write.csv, na = "", row.names = FALSE),
                           "xlsx" = guard(openxlsx::write.xlsx),
                           "xls"  = guard(openxlsx::write.xlsx),
                           "sav"  = guard(haven::write_sav),
                           "dta"  = write.dta,
                           "rds"  = saveRDS,
                           "dput" = dput,
                           "txt"  = writeLines,
                           stop("Unable to find function to write object to file: " %^% f))
      fn(o, f, ...)
    }

    if(msg) info("Writing file " %^% f %^% " was successful.")

    # Set permissions of file to group writable for tetralith
    if (grepl("/mnt/tetra/", f, fixed = TRUE)) {
        Sys.chmod(f, "664", use_umask = FALSE)
        Sys.chmod(dirname(f), "2775", use_umask = FALSE)
    }

}

write.dta <- function(df, ...) {
    if (!is.data.frame(df))
        stop("The object must be of class data.frame")

    if (!requireNamespace("readstata13", quietly = T))
        stop("Missing package: readstata13", call. = F)

    # readstata13 doesn't like when we have NAs in character columns, replace with ""
    character.b <- vapply(df, is.character, logical(1))

    if (any(character.b))
        df[, character.b] <- lapply(df[, character.b, drop = F],
                                    function(v) ifelse(is.na(v), "", v))

    guard(readstata13::save.dta13)(df, ...)
}




