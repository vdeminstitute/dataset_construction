#!/usr/bin/env Rscript
# ==========================================================================
# Copy BFA file to HPC and submit job
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

db <- pg_connect()
get_globals()
objs <- find_dep_files(Sys.getenv("VPIPE_TASK_ID"), db)

# Check if directory is mounted to HPC
stopifnot(is_path_mounted())

write_file(objs[[TASK_NAME]],
    file.path(Sys.getenv("INTEGRITY_SSH_DIR"), "input", "int_input.rds"),
    dir_create = TRUE)

# Set timeout limit 
tim <- "60:00:00"

info("Timeout chosen: " %^% tim)

# submit job
integrity_submit_tetralith_job(index = TASK_NAME,
                         timeout = tim,
                         script = "R/integrity_bfa.R",
                         directory = Sys.getenv("INTEGRITY_DIR"),
                         hpc = "tetralith")
lock_task(db = db)
