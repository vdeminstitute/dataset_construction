#'@export
pg_update_var <- function(df, name, db) {
    qids <- unique(df$question_id)
    stopifnot(length(qids) == 1)
    pg_send_query(db,
        "DELETE FROM " %^% name %^% " " %^%
        "WHERE question_id = " %^% qids %^% ";")
    pg_append_table(df, name, db)
}