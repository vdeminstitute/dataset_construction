#!/usr/bin/env Rscript
# ==========================================================================
# Read HPC log file for this variable and check if it is done.
# If it is done then copy the result file back to local computer.
# If the variable does not pass convergence criteria then mark it as
# failed, copy the file to local computer, and exit with status 4,
# this will change the task status to `convergence` and it needs to be manually
# set to done for the data pipeline to proceed.
# If the variable is still running then exit with status 3, the
# task will be flagged as `hpc_waiting`.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

db <- pg_connect()
get_globals()
check_direct_deps(id = Sys.getenv("VPIPE_TASK_ID"), db)
DONE <- FALSE

# Check if directory is mounted to HPC
stopifnot(is_path_mounted())

# Check log files for convergence or if they are done?!
ll <- list.files(file.path(Sys.getenv("BFA_SSH_DIR"), "logs"), full.names = TRUE)

# Can we safely read a log file?
file_log <- ll[grepl(TASK_NAME %^% "-", ll, fixed = TRUE)]
if (length(file_log) > 1) {
    stop("There are multiple log files for the same BFA!")
}
if (length(file_log) == 0) {
    stop("There is no log file, probably waiting or running.")
}

# Read log file
f <- readLines(file_log)
fl <- trimws(f, which = "both")
fl <- fl[fl != ""]

# print to receive log messages in postgres
print(shQuote(fl))
f <- paste(fl, collapse = "")

# Set status depending on the log file?
if (grepl("Error", f, fixed = TRUE)) {
    quit(save = "no", status = 1L)
}

if (grepl("Finished", f, fixed = TRUE) & !grepl("Convergence check failed", f, fixed = TRUE)) {
	EXIT_STATUS <- 0L
    DONE <- TRUE
}

if (grepl("Finished", f, fixed = TRUE) &
        grepl("Convergence check failed", f, fixed = TRUE)) {
    EXIT_STATUS <- 4L
    DONE <- TRUE
}

if (DONE) {
    bfa_file <-
        list.files(
            file.path(
                Sys.getenv("BFA_SSH_DIR"), "out"),
            pattern = paste0(TASK_NAME, "_\\d+\\.rds$"), full.names = TRUE)
    stopifnot(length(bfa_file) == 1)
    bfa_file <- read_file(bfa_file)
    bfa_file$logs <- fl
    write_file(bfa_file, OUTFILE, dir_create = TRUE)

	db <- pg_connect()
    lock_task(db = db)
	DBI::dbDisconnect(db)

    quit(save = "no", status = EXIT_STATUS)
}

quit(save = "no", status = 3L)
