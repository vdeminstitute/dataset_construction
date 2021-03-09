#!/usr/bin/env Rscript

suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(vutils))

db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")
destination <- file.path(ROOT, "accountability")
path <- Sys.getenv("ACC_SSH_DIR")
OUTFILE <- create_outfile(id = Sys.getenv("TASK_ID"),
                          ROOT = Sys.getenv("ROOT_DIR"),
                          db = db)
check_direct_deps(id = Sys.getenv("TASK_ID"), db)
varname <- get_varname(id = Sys.getenv("TASK_ID"), db)
infile <- list.files(file.path(path, "out"), pattern = varname, full.names = T)

stopifnot(`More than one file found!` = length(infile) == 1)

logs <- list.files(file.path(path, "logs"), pattern = varname, full.names = T)
stopifnot(length(logs) == 1)
d <- readLines(logs)

print(shQuote(d))

if (any(grepl("Finished calculation :)", d, fixed = TRUE)) &
        length(infile) == 1) {
    ll <- read_file(infile)
    ll$logs <- d
    ll[[varname]]$cy <- select(ll[[varname]]$cy, country_text_id, year, matches("acc"))
    write_file(ll, OUTFILE, dir_create = T)
	update_task_status(db = db)
    lock_task(db = db)
    quit(save = "no", status = 0)
}

if (any(grepl("Error", d))) {
    update_task_status(status = "error", db = db)
    quit(save = "no", status = 0)
}

update_task_status(status = "waiting", db = db)
