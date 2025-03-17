#' Get schema name
#' @export
schema_name <- function(name) {
    unlist(strsplit(name, ".", fixed = TRUE))
}

#' Create a database table from a data.frame
#'@export
pg_create_table <- function(df, name, db, overwrite = FALSE, ...) {
    DBI::dbWriteTable(db,
        name = schema_name(name),
        value = df,
        row.names = FALSE,
        overwrite = overwrite, ...)
	return(invisible(NULL))
}

#' Create table types
#'@export
pg_create_table_types <- function(name, columns, types, db) {
    cols <- paste(paste(columns, types, sep = " "), collapse = ", ")
    query <- "CREATE TABLE " %^% name %^% "(" %^% cols %^% ");"
    pg_send_query(db, query)

}

#' Does a table exist in a database?
#'@export
pg_exists_table <- function(name, db) {
    DBI::dbExistsTable(db, name = schema_name(name))
}

#' Truncate a database table
#'@export
pg_truncate_table <- function(name, db) {
    pg_send_query(db, "TRUNCATE TABLE " %^% name %^% ";")
}

#' Append to a database table
#'@export
pg_append_table <- function(df, name, db) {
    DBI::dbWriteTable(db,
        name = schema_name(name),
        value = df,
        row.names = FALSE,
        append = TRUE)
	
    return(invisible(NULL))
}

#' @title Replace a PostgreSQL table
#' @description This function replaces a table in a PostgreSQL database with a new data frame.
#' @param df A data frame containing the new data to be inserted into the table.
#' @param name Character string with the name of the table to be replaced.
#' @param db A PostgreSQL database connection object.
#' @return Returns Null invisibly.
#' @export
pg_replace_table <- function(df, name, db) {
    pg_truncate_table(name, db)
    pg_append_table(df, name, db)
}

#' @title Drop a PostgreSQL table
#' @description This function drops a table from a PostgreSQL database.
#' @param name Character string with the name of the table to drop.
#' @param db A PostgreSQL database connection object.
#' @return Returns NULL invisibly.
#' @export
pg_drop_table <- function(name, db) {
    pg_send_query(db, sprintf("DROP TABLE %s;", name))
}


#' @title Send SQL query to database without expecting a return value
#' @param db Database connection.
#' @param query SQL query.
#' @return Returns NULL invisibly.
#' @export
pg_send_query <- function(db, query) {
    rs <- DBI::dbSendStatement(conn = db, statement = query)
	DBI::dbClearResult(rs)
	return(invisible(NULL))
}

#' @title Send SQL query to database and expecting a return
#' @description If you want to only send a query without expecting a return value, use \code{\link{pg_send_query}}.
#' @param db Database connection.
#' @param query SQL query.
#' @return Returns a data.frame.
#' @export
pg_get_query <- function(db, query) {
	DBI::dbGetQuery(db, query)
}

#'@export
pg_update_table <- function(df, name, db) {
    if (pg_exists_table(name, db)) {
        pg_truncate_table(name, db)
        pg_append_table(df, name, db)
    } else {
        pg_create_table(df, name, db)
    }
	return(invisible(NULL))
}

#'@export
pg_create_index <- function(column, name, db, unique_idx = FALSE) {
    if (unique_idx) {
        unique_text = "UNIQUE "
    } else {
        unique_text = ""
    }

    pg_send_query(db,
        "CREATE " %^% unique_text %^% "INDEX ON " %^%
        name %^% " (" %^% column %^% ");")
	return(invisible(NULL))
}

#' Prepare string for postgres query
#'
#' @export
pg_text <- function(v) {
    cat(paste0("'", paste0(v, collapse = "','"), "'"), sep = "\n")
}

#' Prepare edata for e_data table
#' 
#' @export
edata_wide_to_long <- function(db, df) {
    vars <- grep("^e_", names(df), value = TRUE)
    qids <- dbGetQuery(db, sprintf("SELECT question_id, name FROM question WHERE name IN ('%s') AND archived = FALSE;", paste(vars, collapse = "', '")))

    stopifnot(nrow(qids) == length(vars))

    update_frequency <- dbGetQuery(db, sprintf("SELECT DISTINCT(update_frequency) FROM e_data WHERE question_id IN (%s);", qids$question_id))
    stopifnot(nrow(update_frequency) == 1)
    info(sprintf("Update frequency found: %s", as.character(update_frequency)))

    df_long <- df %>%
        select(country_id, year, all_of(vars)) %>%
        wide_to_long(., id_vars = c("country_id", "year")) %>%
        left_join(qids, by = c("variable" = "name")) %>%
        mutate(update_frequency = as.character(update_frequency)) %>%
        rename(code = value,
            tag = variable) %>%
        filter(!is.na(code)) %>%
        arrange(question_id, country_id, year)

    return(df_long)
}

#' Delete and append edata
#' 
#' @export
pg_delete_and_append_edata <- function(db, df) {
    qids <- unique(df$question_id)
    dbSendQuery(db, sprintf("DELETE FROM e_data WHERE question_id in (%s);", paste(qids, collapse = ", ")))
    info(sprintf("Deleted e_data rows for questions: %s", qids))

    pg_append_table(df, "e_data", db)
    info("Appended e_data rows")
}