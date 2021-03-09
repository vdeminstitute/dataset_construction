
#' @export
get_varname <- function(id, db) {
    dplyr::tbl(db, dbplyr::in_schema("pipe", "make")) %>%
        dplyr::filter(task_id == id) %>%
        dplyr::select(question_name) %>%
        dplyr::collect(n = Inf) %$% question_name
}

#' @export
get_outdir <- function(id, db) {
    dplyr::tbl(db, dbplyr::in_schema("pipe", "modules")) %>%
        dplyr::filter(module_id == id) %>%
        dplyr::pull(outdir)
}

#' @export
create_outfile <- function(id, ROOT, db) {
    outdir <- dplyr::tbl(db, dbplyr::in_schema("pipe", "make")) %>%
        dplyr::filter(task_id == id) %>%
        dplyr::pull(module_id) %>%
        get_outdir(., db)

    varname <- get_varname(id, db)
    return(file.path(ROOT, outdir, paste0(varname, "_", add_zeroes(id), ".rds")))
}

#' @export
create_outfile_local <- function(ids, tasks, modules, ROOT) {
    df <- dplyr::left_join(tasks, modules, by = c("module_id", "module_name")) %>%
        dplyr::filter(task_id %in% ids)
    formatted_ids <- vapply(ids, add_zeroes, FUN.VALUE = character(1))
    outfile <- 
        file.path(ROOT, df$outdir, 
            paste0(df$question_name, "_", formatted_ids, ".rds"))
    return(outfile)
}


#' @export
add_zeroes <- function(id) {
    n_rep <- 6 - {nchar(as.character(id))}
    zeroes <- Reduce("paste0",{rep(0, n_rep)})
    zeroes %^% id
}

#' @export
find_dep_files <- function(id, db, check_dep_status = TRUE) {

    varname <- get_varname(id, db)

    df <-
        dplyr::tbl(db, dbplyr::in_schema("pipe", "make")) %>%
        dplyr::collect(n = Inf) %>%
        as.data.frame(stringsAsFactors = F)
    dependencies <- strsplit(df$deps[df$task_id == id], split = ",") %>%
        unlist %>%
        as.integer
    df %<>%
        dplyr::filter(task_id %in% dependencies) %>%
        dplyr::select(task_id, question_name, status, module_name)
    # stop if any dependencies are not done yet
    if (any(df$status != "done") & check_dep_status) {
        update_task_status_no_timestamp(status = "waiting", db = db)
        info("Some dependencies are not done yet!")
        quit(save = "no", status = 0)
    }
    # Do any questions show up twice? e.g. for coder_table?
    double_que =
        ifelse(length(unique(df$question_name)) != nrow(df), TRUE, FALSE)

    ll <- list()
    for (i in 1:nrow(df)) {
        if (!double_que) {
            ll[[df$question_name[i]]] <-
                read_file(find_dep_file_by_task(df$task_id[i], varname))
        } else {
            ll[[df$question_name[i] %^% "_" %^% df$module_name[i]]] <-
                read_file(find_dep_file_by_task(df$task_id[i], varname))
        }
    }
    ll
}


#' @export
try_find_dep_files <- function(id, db, lev) {

    varname <- get_varname(id, db)
    # load only cd or cy and then remove object to save memory!
    df <-
        dplyr::tbl(db, dbplyr::in_schema("pipe", "make")) %>%
        dplyr::collect(n = Inf) %>%
        as.data.frame(stringsAsFactors = F)
    dependencies <- strsplit(df$deps[df$task_id == id], split = ",") %>%
        unlist %>%
        as.integer
    df %<>%
        dplyr::filter(task_id %in% dependencies) %>%
        dplyr::select(task_id, question_name, status, module_name)

    ll <- list()
    for (i in 1:nrow(df)) {
        tryCatch({
            ll[[df$question_name[i] %^% "_" %^% df$module_name[i]]] <-
                read_file(find_dep_file_by_task(df$task_id[i], varname))
        }, error = function(err){
            print("failed to load task file: " %^%
                df$question_name[i] %^% "_" %^% df$module_name[i])
        }, warning = function(war){})
    }
    ll
}

#' @export
find_dep_file_by_task <- function(id, varname = "") {
    ll <- list.files(Sys.getenv("ROOT_DIR"),
                     full.names = T,
                     recursive = T,
                     pattern = add_zeroes(id))

    if (length(ll) == 0)
        stop("Did not find dependency file for task_id " %^% id)

    if (length(ll) > 1) {
        ll <- ll[grepl(varname, ll)]
        if (length(ll) != 1)
            stop("Several files found, dont know which one to use.")
    }

    stopifnot(length(ll) == 1)

    ll
}

