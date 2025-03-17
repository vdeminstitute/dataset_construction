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

    if (class(lines) == "try-error") {
        return(NULL)
    }

	# Remove comments
	lines <- lines[!grepl("^#", lines)]

	# Remove empty lines
	lines <- lines[lines != ""]

	# Remove lines that do not have at least five [:]
    lines_id <- unlist(lapply(strsplit(lines, ":"), length)) >= 5
    lines <- lines[lines_id]

    fields <- c("host", "port", "dbname", "user", "password")
    ll <- lapply(lines, function(l) {
        out <- strsplit(l, ":")[[1]]
        # If password contains colon we need to collapse it.
        len <- length(out)
        if (len > 5) {
            out[5] <- paste(out[5:len], collapse = ":")
            out <- out[1:5]
        }
        # Remove colon preceding backslash in password.
        # Colons in PostgreSQL passwords stored in .pgpass file need to be escaped.
        out[5] <- gsub("\\:", ":", out[5], fixed = TRUE)
        stats::setNames(out, fields)
    })

    if (length(ll) == 0L) {
        stop("No entries found in ~/.pgpass file!")
    }
	# Remove entries that have a star anywhere (outside the password)
	# This function currently does not support matching *!
	#ll <- Filter(function(l) {
	#	!any(unlist(l)[1:4] == "*")
	#}, ll)

    args <- list(...)
	if (length(args) == 0L) {
        return(ll)
    }
	
	# Wrong arguments supplied?
    unknown <- names(args)[!names(args) %in% fields]
    if (length(unknown) > 0) {
        stop(sprintf("Unknown fields: %s", paste(unknown, collapse = ",")))
    }
    
    out.ll <-
        Filter(
            function(l) {
            f <- function(s) {(args[[s]] == l[[s]]) | (l[[s]] == "*")}
                all(vapply(names(args), f, logical(1)))
    }, x = ll)

    if (length(out.ll) == 0L) {
		stop("No matches for pgpass file!")
	}
		
	out <- out.ll[[1]]

	# Replace * with input arguments
	for (n in names(out)) {
		if (out[n] == "*")
			out[n] <- args[n]
	}

	return(out)
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
pg_connect <- function(dbname = Sys.getenv("DEFAULT_DB"), 
	user = Sys.getenv("DB_USER"),
	drv = "RPostgreSQL", ...) {

	if (dbname == "")
		stop("No default database specified")	
    if (!requireNamespace("RPostgreSQL", quietly = TRUE))
        stop("Missing package: RPostgreSQL", call. = FALSE)

	info(paste0("Connecting to ", dbname))

	if (user == "") {
		creds <- pg_pass(dbname = dbname, ...)
	} else {
		creds <- pg_pass(dbname = dbname, user = user, ...)
	}
    
	if (Sys.info()["sysname"] != "Linux" | drv == "RPostgres") {
		
        if (!requireNamespace("RPostgres", quietly = T)) {
            stop("Missing package: RPostgres", call. = FALSE)
        }
		
        return(do.call(
            DBI::dbConnect,
            c(drv = RPostgres::Postgres(), creds, bigint = "integer")
            ))
	} else {
		return(do.call(
            DBI::dbConnect,
            c(drv = RPostgreSQL::PostgreSQL(max.con = 50), creds)))
	}
}

#' @export
pg_list_all_connections <- function() {
    DBI::dbListConnections(RPostgreSQL::PostgreSQL())
}

#' @export
pg_kill_all_connections <- function () {

    all_cons <- DBI::dbListConnections(RPostgreSQL::PostgreSQL())

    print(all_cons)

    for(con in all_cons)
        dbDisconnect(con)

    print(paste(length(all_cons), " connections killed."))

}