
#'@export
pg_update_var <- function(df, name, db) {

    callerName <- vbase::callerCall(getName = TRUE)
    vbase::info(sprintf("%s will be phased out", callerName))

    qids <- unique(df$question_id)
    stopifnot(length(qids) == 1)
    vbase::pg_send_query(db,
        "DELETE FROM " %^% name %^% " " %^%
        "WHERE question_id = " %^% qids %^% ";")
    vbase::pg_append_table(df, name, db)
}

#' @export
try_find_dep_files <- function(id, db, lev) {

    callerName <- vbase::callerCall(getName = TRUE)
    vbase::info(sprintf("%s will be phased out", callerName))

    varname <- get_varname(id, db)
    # load only cd or cy and then remove object to save memory!
    df <-
        dplyr::tbl(db, "make") %>%
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