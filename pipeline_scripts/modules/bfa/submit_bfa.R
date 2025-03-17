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
    file.path(Sys.getenv("BFA_SSH_DIR"), "z.sample", TASK_NAME %^% ".rds"),
    dir_create = TRUE)

# Set timeout limit depending on number of components of the index
n_comps <- length(objs[[TASK_NAME]][[TASK_NAME]]$components)
tim <- case_when(
    n_comps <= 4 ~ "48:00:00",
    n_comps <= 6 ~ "72:00:00",
	n_comps <= 10 ~ "120:00:00",
    TRUE ~ "168:00:00")

info("Timeout chosen: " %^% tim)

# submit job
bfa_submit_tetralith_job(index = TASK_NAME,
                         timeout = tim,
                         script = "R/bfa.R",
                         directory = Sys.getenv("BFA_DIR"),
                         hpc = "tetralith")
lock_task(db = db)
