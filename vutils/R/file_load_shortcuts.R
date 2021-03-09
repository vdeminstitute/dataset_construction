#' @export
load_qtable <- function() {
    read_file(file.path(Sys.getenv("ROOT_DIR"), "refs", "question_table.rds"))
}

#' @export
load_country <- function() {
    read_file(file.path(Sys.getenv("ROOT_DIR"), "refs", "country_table.rds"))
}

#' @export
load_country_unit <- function() {
    read_file(file.path(Sys.getenv("ROOT_DIR"), "refs", "country_unit.rds"))
}

#' @export
load_codebook <- function() {
    read_file(file.path(Sys.getenv("ROOT_DIR"), "refs", "codebook.rds"))
}

#' @export
load_ds <- function(cy = TRUE) {
    if (cy) {
        read_file("~/data/datasets/v10/Country_Year_V-Dem_Full+others_R_v10/V-Dem-CY-Full+Others-v10.rds")
    } else {
        read_file("~/data/datasets/v10/Country_Date_V-Dem_R_v10/V-Dem-CD-v10.rds")
    }
}