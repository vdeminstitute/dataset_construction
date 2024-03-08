#!/usr/bin/env Rscript

suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(vutils))

db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")
varname <- get_varname(id = Sys.getenv("TASK_ID"), db)
OUTFILE <- create_outfile(id = Sys.getenv("TASK_ID"),
                          ROOT = Sys.getenv("ROOT_DIR"),
                          db = db)
check_direct_deps(id = Sys.getenv("TASK_ID"), db)
DONE <- FALSE

# Check if directory is mounted to HPC
stopifnot(is_path_mounted())


# Check log files for convergence or if they are done?!
ll <- list.files(file.path(Sys.getenv("BFA_SSH_DIR"), "logs"),
                 full.names = T)

file_log <- ll[grepl(varname %^% "-", ll, fixed = T)]
if (length(file_log) > 1)
    stop("There are multiple log files for the same BFA!")
if (length(file_log) == 0)
    stop("There is no log file, probably waiting for free node!")
f <- readLines(file_log)
fl <- f %>% trimws(which = "both") %>% .[. != ""]
# print to receive log messages in postgres
print(shQuote(fl))
f <- paste(fl, collapse = "")

if (grepl("Error", f, fixed = T)) {
    update_task_status(status = "error", db = db)
    quit(save = "no", status = 0)
}

if (grepl("Finished", f, fixed = T) &
        !grepl("Convergence check failed", f, fixed = T)) {
    update_task_status(db = db)
    DONE <- TRUE
}

if (grepl("Finished", f, fixed = T) &
        grepl("Convergence check failed", f, fixed = T)) {
    update_task_status(status = "convergence", db = db)
    DONE <- TRUE
}

if (DONE) {
    bfa_file <- read_file(file.path(Sys.getenv("BFA_SSH_DIR"), "out",
        varname %^% ".rds")) 
    bfa_file$logs <- fl

    bfa_file %>%
    write_file(OUTFILE, dir_create = T)
    lock_task(db = db)
    quit(save = "no", status = 0)
}


if (!DONE) update_task_status(status = "hpc_waiting", db = db)
