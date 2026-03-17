#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# This script checks that the integrity index has finished and if it has, saves the output to the version folder.
# ------------------------------------------------------------------------------
suppressMessages(library(dplyr))
suppressMessages(library(vpipe))
suppressMessages(library(vutils))

db <- pg_connect()
get_globals()
check_direct_deps(id = Sys.getenv("VPIPE_TASK_ID"), db)

# Check if directory is mounted to HPC
stopifnot(is_path_mounted())

# Grab logs
fp <- list.files(file.path(Sys.getenv("INTEGRITY_SSH_DIR"), "logs"), pattern = TASK_NAME, full.names = TRUE)

if (length(fp) > 1) {
    warn(sprintf("Multiple log files found for %s", TASK_NAME))
    quit(save = "no", status = 1L)
}

logs <- readLines(fp)

if (any(grepl("Convergence failed", logs))) {
    info("Integrity BFA job did not converge, check logs for details")
    convergence <- TRUE
} else {
    convergence <- FALSE
}

# Grab output file
fp <- list.files(file.path(Sys.getenv("INTEGRITY_SSH_DIR"), "out"), pattern = TASK_NAME, full.names = TRUE)

if (length(fp) > 1) {
    warn(sprintf("Multiple output files found for %s", TASK_NAME))
    quit(save = "no", status = 1L)
} 

if (length(fp) == 0) {
    info(sprintf("No output file found for %s", TASK_NAME))
    quit(save = "no", status = 1L)
}

model_output <- read_file(fp)
out <- list(
    v2x_electoral_integrity = model_output,
    logs = ll[["logs"]])

write_file(out, OUTFILE, dir_create = TRUE)
lock_task(db = db)
# Exit successfully
quit(save = "no", status = 0L)
