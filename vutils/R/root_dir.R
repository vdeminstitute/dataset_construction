#' @export
load_country <- function(ROOT = Sys.getenv("ROOT_DIR")) {    
    stopifnot(ROOT != "")
    vutils::read_file(file.path(ROOT, "refs", "country_table.rds"))
}

#' @export
load_country_unit <- function(ROOT = Sys.getenv("ROOT_DIR")) {
    stopifnot(ROOT != "")
    vutils::read_file(file.path(ROOT, "refs", "country_unit.rds"))
}

#' @export
load_qtable <- function(ROOT = Sys.getenv("ROOT_DIR")) {
    stopifnot(ROOT != "")
    vutils::read_file(file.path(ROOT, "refs", "question_table.rds"))
}