

#' @export
stid <- function(id) {
    Sys.setenv("TASK_ID" = as.character(id))
}

#' @export
load_task_file <- function(variable, module, ROOT = Sys.getenv("ROOT_DIR"), db = db) {
    stopifnot(ROOT != "") 
    tasks <- load_tasks(db)
    tasks %>% dplyr::filter(module_name == module, question_name == variable) %$% 
        task_id %>% vutils::find_dep_file_by_task(.) %>% vutils::read_file(.)
}

#' @export
load_tasks <- function(db) {
    dplyr::tbl(db, dbplyr::in_schema("pipe", "make")) %>% 
        dplyr::collect(n = Inf)
}

#' @export
get_globals <- function() {
    # Having both pointers is a temporary solution
    # We want to use DB in the future
    db <<- vutils::pg_connect(Sys.getenv("DS_VERSION"))
    DB <<- db
    # If DEBUG is TRUE use first task_id per module in interactive mode
    if (isTRUE(as.logical(Sys.getenv("DEBUG"))) & interactive() & Sys.getenv("TASK_ID") == "") {
        tt <- dplyr::tbl(db, dbplyr::in_schema("pipe", "make")) %>% 
            dplyr::collect(n = Inf) %>%
            dplyr::filter(module_name == Sys.getenv("MODULE_NAME")) %>%
            dplyr::arrange(task_id) %$% 
            dplyr::first(task_id)
        Sys.setenv(TASK_ID = tt)
        warn("SETTING TASK_ID FROM DEBUG MODE!!!")
    }

    ROOT <<- Sys.getenv("ROOT_DIR")
    OUTFILE <<- vutils::create_outfile(id = Sys.getenv("TASK_ID"),
                          ROOT = ROOT,
                          db = db)
    VARNAME <<- vutils::get_varname(Sys.getenv("TASK_ID"), db)
    TASK_ID <<- Sys.getenv("TASK_ID")
}

#' @export
no_test <- function() {
    !isTRUE(as.logical(Sys.getenv("UNIT_TESTS")))
}

#' @export
set_env <- function(...) {
    if(interactive())
        Sys.setenv(...)
}
