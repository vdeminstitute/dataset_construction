#' Use .pgpass file for postgres login
#'
#' Reads login credentials from a pgpass file and returns a list of
#' named vectors for each entry.
#'
#' @param pass_file Location of pgpass file. Default: ~/.pgpass.
#' @param ... Named regular expressions used to filter entries from
#'     ~/.pgpass. Names must correspond to one or more of "host",
#'     "port", "dbname", "user", "password".
#'
#' @examples
#' pg_pass()
#' pg_pass(host = "example-host", user = "example-user")
#'
#' @export
pg_pass <- function(pass_file = file.path(Sys.getenv("HOME"), ".pgpass"), ...) {
    # If HOME is unset file.path will return "/.pgpass"
    if (!dir.exists(Sys.getenv("HOME")) | !file.exists(pass_file)) {
        warning("Unable to find pass_file")
        return(NULL)
    }

    lines <- try(readLines(pass_file))

    if (class(lines) == "try-error")
        return(NULL)

    fields <- c("host", "port", "dbname", "user", "password")
    ll <- lapply(lines, function(l) {
        stats::setNames(strsplit(l, ":")[[1]], fields)
    })

    if (length(ll) == 0)
        return(NULL)

    args <- list(...)

    if (length(args) > 0) {
        unknown <- names(args)[!names(args) %in% fields]
        if (length(unknown) > 0)
            sprintf("Unknown fields: %s", paste(unknown, collapse = ",")) %>% stop

        out.ll <- Filter(function(l) {
            # Lexical scoping!!
            f <- function(s) grepl(args[[s]], l[[s]])
            all(vapply(names(args), f, logical(1)))
        }, ll)

        if (length(out.ll) == 0) NULL else out.ll
    } else {
        ll
    }
}

#' Connect to the V-Dem DB
#'
#' Convenience function meant to connect directly to the V-Dem
#' Postgres Database. Calls \code{\link{pg_pass}} with the default
#' location of \code{pass_file}, which should include all connection
#' details. For more fine grain control, \code{\link{pg_pass}} should
#' be used directly, or alternatively if a \code{.pgpass} entry is
#' missing, \code{\link[DBI]{dbConnect}}, \code{\link[DBI]{dbDriver}},
#' and \code{\link[RPostgres]{Postgres}}.
#'
#' @return Returns an object of class \code{PostgreSQLConnection}.
#'
#' @export
pg_connect <- function(dbname = "XXXXXX") {
    if (!requireNamespace("RPostgreSQL", quietly = T))
        stop("Missing package: RPostgreSQL", call. = F)

    creds <- pg_pass(dbname = dbname)[[1]]
    creds <- creds[names(creds) != "dbname"]

    do.call(DBI::dbConnect, c(drv = RPostgreSQL::PostgreSQL(max.con = 50),
                              dbname = dbname,
                              creds))
}


#'@export
schema_name <- function(name) {
    strsplit(name, ".", fixed = T) %>% unlist
}

#'@export
pg_create_table <- function(df, name, db, overwrite = FALSE, ...) {
    DBI::dbWriteTable(db,
                      name = schema_name(name),
                      value = df,
                      row.names = F,
                      overwrite = overwrite, ...)
}

#'@export
pg_create_table_types <- function(name, columns, types, db) {
    cols <- paste(columns, types, sep = " ") %>% paste(collapse = ", ")
    query <- "CREATE TABLE " %^% name %^% "(" %^% cols %^% ");"
    pg_send_query(db, query)
}

list_all_connections <- function() {
    DBI::dbListConnections(RPostgreSQL::PostgreSQL())
}

kill_all_connections <- function () {

    all_cons <- DBI::dbListConnections(RPostgreSQL::PostgreSQL())

    print(all_cons)

    for(con in all_cons)
        dbDisconnect(con)

    print(paste(length(all_cons), " connections killed."))

}


#'@export
pg_exists_table <- function(name, db) {
    DBI::dbExistsTable(db,
                       name = schema_name(name))
}

#'@export
pg_truncate_table <- function(name, db) {
    DBI::dbGetQuery(db, "TRUNCATE TABLE " %^% name %^% ";")
}

#'@export
pg_append_table <- function(df, name, db) {
    DBI::dbWriteTable(db,
                      name = schema_name(name),
                      value = df,
                      row.names = F,
                      append = T)
}

#'@export
pg_replace_table <- function(df, name, db) {
    pg_truncate_table(name, db)
    pg_append_table(df, name, db)
}

#'@export
pg_drop_table <- function(name, db) {
    DBI::dbGetQuery(db, "DROP TABLE " %^% name %^% ";")
}

#'@export
pg_send_query <- function(db, query) {
    DBI::dbGetQuery(conn = db, statement = query)
}




#'@export
pg_update_table <- function(df, name, db) {
    if (pg_exists_table(name, db)) {
        pg_truncate_table(name, db)
        pg_append_table(df, name, db)
    } else {
        pg_create_table(df, name, db)
    }
}

#'@export
pg_create_index <- function(column, name, db, unique_idx = F) {
    if (unique_idx) {
        unique_text = "UNIQUE "
    } else {
        unique_text = ""
    }

    pg_send_query(db,
        "CREATE " %^% unique_text %^% "INDEX ON " %^%
        name %^% " (" %^% column %^% ");")
}

