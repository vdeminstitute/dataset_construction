
#' Lock task in make table
#'
#' @export
lock_task <- function(id = Sys.getenv("TASK_ID"), db) {
    DBI::dbGetQuery(db,
        "UPDATE pipe.make SET lock = TRUE WHERE task_id = " %^% id %^% ";")
}

#' Update task status of make table
#'
#' @export
update_task_status <- function(status = "done",
                               id = Sys.getenv("TASK_ID"),
                               db) {
    if (id == "")
        stop("No TASK_ID specified!")

    DBI::dbGetQuery(db,
        "UPDATE pipe.make " %^%
            "SET status = '" %^% status %^% "', " %^%
                "ts = NOW() " %^%
            "WHERE task_id = " %^% id %^% ";")
}

#' @export
update_task_status_no_timestamp <- function(status = "done",
                               id = Sys.getenv("TASK_ID"),
                               db) {
    if (id == "")
       stop("No TASK_ID specified!")

    DBI::dbGetQuery(db,
        "UPDATE pipe.make " %^%
            "SET status = '" %^% status %^% "' " %^%
            "WHERE task_id = " %^% id %^% ";")
}

#'@export
pg_update_timestamp <- function(id, db) {
    pg_send_query(db, "UPDATE pipe.make SET ts = NOW() WHERE task_id = " %^% id %^% ";")
}

#' @export
pg_update_many_timestamps <- function(task_ids, db) {
    for (task_id in task_ids) {
        pg_update_timestamp(task_id, db)
        Sys.sleep(1)
    }
}